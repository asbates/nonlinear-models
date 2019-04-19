
# model flu data using spline based methods

# ========================
# ======= setup ==========
# ========================

library(tidyverse)
library(fields)  # tps
library(earth)  # MARS
library(mgcv)  # GAM
library(forecast)
library(yardstick)

theme_set(theme_bw())

flu <- read_csv("data/ilinet-calif-up-to-2019-03-31.csv") %>% 
  mutate(t = 1:nrow(.))

flu

flu_train <- filter(flu, week_start < "2019-01-01")
flu_test <- filter(flu, week_start >= "2019-01-01")

flu_train %>% 
  summarise(start = min(week_start), end = max(week_start))

flu_test %>% 
  summarise(start = min(week_start), end = max(week_start))

flu_ts <- ts(flu$ilitotal, frequency = 52)
flu_ts_train <- flu_ts[1:nrow(flu_train)]
flu_ts_test <- flu_ts[(nrow(flu_train) + 1):nrow(flu)]

# Tps requires x/y
x_train <- flu_train %>%
  select(t)

y_train <- flu_train %>% 
  select(ilitotal)

x_test <- flu_test %>% 
  select(t)

y_test <- flu_test %>% 
  select(ilitotal)

flu %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line()

ggAcf(flu_ts)
ggPacf(flu_ts)

# =========================================
# ====== models as function of time =======
# =========================================

# baseline model
# this took a long time on my computer
arima_mod <- auto.arima(flu_ts_train)
arima_mod

# thin plate spline
tps_mod <- Tps(x_train, y_train)

# MARS
mars_mod <- earth(ilitotal ~ t, data = flu_train)

# GAM
gam_mod <- gam(ilitotal ~ s(t), data = flu_train, method = "REML")


flu_train <- flu_train %>% 
  mutate(
    fitted_arima = fitted(arima_mod),
    fitted_tps = fitted(tps_mod)[,1],
    fitted_mars = predict(mars_mod)[,1],
    fitted_gam = fitted(gam_mod)
  )


flu_train %>% 
  select(week_start, ilitotal, starts_with("fitted")) %>% 
  gather("model", "value", -week_start, -ilitotal) %>% 
  mutate(model = str_remove(model, "fitted_")) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = value, color = model))




# =================================
# ======= models using lags =======
# =================================

# use one month of previous data as predictors
flu <- flu %>% 
  mutate(
    lag_ilitotal = lag(ilitotal),
    lag2_ilitotal = lag(ilitotal, n = 2L),
    lag3_ilitotal = lag(ilitotal, n = 3L),
    lag4_ilitotal = lag(ilitotal, n = 4L)
  )


# this assigns 13 weeks or 3 months to testing
# Tps doesn't handle NAs
flu_train <- filter(flu, week_start < "2019-01-01") %>% 
  filter(!is.na(lag4_ilitotal))
  
flu_test <- filter(flu, week_start >= "2019-01-01")

flu_train %>% 
  summarise(start = min(week_start), end = max(week_start))

flu_test %>% 
  summarise(start = min(week_start), end = max(week_start))

flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line()

x_train <- flu_train %>% 
  select(starts_with("lag"))

y_train <- flu_train %>% 
  select(ilitotal)

x_test <- flu_test %>% 
  select(starts_with("lag"))

y_test <- flu_test %>% 
  select(ilitotal)

flu_tps <- Tps(x_train, y_train)

plot(flu_tps)

flu_train <- flu_train %>% 
  mutate(.pred_tps = predict(flu_tps)[,1])

flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = .pred_tps), color = "grey")

flu_train %>% 
  yardstick::metrics(truth = ilitotal, estimate = .pred_tps)


flu_mars <- earth(
  ilitotal ~ lag_ilitotal + lag2_ilitotal + lag3_ilitotal + lag4_ilitotal,
  data = flu_train)

flu_train <- flu_train %>% 
  mutate(.pred_mars = predict(flu_mars)[,1])

flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = .pred_mars), color = "grey")

flu_train %>% 
  yardstick::metrics(truth = ilitotal, estimate = .pred_mars)


flu_gam <- gam(
  ilitotal ~ s(lag_ilitotal) + s(lag2_ilitotal) + 
             s(lag2_ilitotal) + s(lag4_ilitotal),
  data = flu_train,
  method = "REML")

flu_train <- flu_train %>% 
  mutate(.pred_gam = predict(flu_gam))

flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = .pred_gam), color = "grey")


flu_train %>% 
  yardstick::metrics(truth = ilitotal, estimate = .pred_gam)


flu_test %>% 
  mutate(.pred_tps = predict(flu_tps,x = x_test)[,1]) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = .pred_tps), color = "grey")

# shoud we also include lags of number of providers?
# fewer providers reporting might mean lower number of reported cases
# it also might be an indicator of data quality/accuracy b/c there might be
#  unreported cases with fewer providers reporting
flu <- flu %>% 
  mutate(
    lag_num_providers = lag(num_of_providers),
    lag2_num_providers = lag(num_of_providers, n = 2L),
    lag3_num_providers = lag(num_of_providers, n = 3L),
    lag4_num_providers = lag(num_of_providers, n = 4L)
  )
