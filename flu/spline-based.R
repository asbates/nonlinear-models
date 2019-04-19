
# model flu data using spline based methods

# ========================
# ======= setup ==========
# ========================

library(tidyverse)
library(fields)  # tps
library(earth)  # MARS
library(mgcv)  # GAM
library(forecast)

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


# baseline model
arima_mod <- auto.arima(flu_ts_train)
arima_mod


# =========================================
# ====== models using time as 1,2,... =====
# =========================================

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


# see how top models do on test set
flu_test <- flu_test %>% 
  mutate(
    pred_arima = forecast(arima_mod, h = 13)$mean[1:13],
    pred_tps = predict(tps_mod, x_test)[,1]
  )

flu_test %>% 
  select(week_start, ilitotal, starts_with("pred")) %>% 
  gather("model", "forecast", -week_start, -ilitotal) %>% 
  mutate(model = str_remove(model, "pred_")) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = forecast, color = model))


flu %>%
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(week_start, pred_arima), data = flu_test, color = "blue") +
  geom_line(aes(week_start, pred_tps), data = flu_test, color = "red")


# ============================================
# ===== models using time as year, week ======
# ============================================
flu_train <- filter(flu, week_start < "2019-01-01")
flu_test <- filter(flu, week_start >= "2019-01-01")

x_train <- flu_train %>%
  select(year, week)

y_train <- flu_train %>% 
  select(ilitotal)

x_test <- flu_test %>% 
  select(year, week)

y_test <- flu_test %>% 
  select(ilitotal)

tps_mod <- Tps(x_train, y_train)
mars_mod <- earth(ilitotal ~ year + week, data = flu_train)
gam_mod <- gam(ilitotal ~ s(year) + s(week), data = flu_train, method = "REML")


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


# see how top models do on test set
flu_test <- flu_test %>% 
  mutate(
    pred_tps = predict(tps_mod, x_test)[,1],
    pred_mars = predict(mars_mod, x_test)[,1],
    pred_gam = predict(gam_mod, newdata = flu_test)
  )

flu_test %>% 
  select(week_start, ilitotal, starts_with("pred")) %>% 
  gather("model", "forecast", -week_start, -ilitotal) %>% 
  mutate(model = str_remove(model, "pred_")) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = forecast, color = model))


flu %>%
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(week_start, pred_tps), data = flu_test, color = "blue") +
  geom_line(aes(week_start, pred_gam), data = flu_test, color = "red") +
  geom_line(aes(week_start, pred_mars), data = flu_test, color = "green")




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

# Tps doesn't handle NAs
flu_train <- filter(flu, week_start < "2019-01-01") %>% 
  filter(!is.na(lag4_ilitotal))
  
flu_test <- filter(flu, week_start >= "2019-01-01")

flu_train %>% 
  summarise(start = min(week_start))

x_train <- flu_train %>% 
  select(starts_with("lag"))

y_train <- flu_train %>% 
  select(ilitotal)

x_test <- flu_test %>% 
  select(starts_with("lag"))

y_test <- flu_test %>% 
  select(ilitotal)

tps_mod <- Tps(x_train, y_train)

mars_mod <- earth(
  ilitotal ~ lag_ilitotal + lag2_ilitotal + lag3_ilitotal + lag4_ilitotal,
  data = flu_train)

gam_mod <- gam(
  ilitotal ~ s(lag_ilitotal) + s(lag2_ilitotal) + 
    s(lag2_ilitotal) + s(lag4_ilitotal),
  data = flu_train,
  method = "REML")


flu_train <- flu_train %>% 
  mutate(
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


# see how top models do on test set
flu_test <- flu_test %>% 
  mutate(
    pred_tps = predict(tps_mod, x_test)[,1],
    pred_mars = predict(mars_mod, x_test)[,1],
    pred_gam = predict(gam_mod, newdata = flu_test)
  )

flu_test %>% 
  select(week_start, ilitotal, starts_with("pred")) %>% 
  gather("model", "forecast", -week_start, -ilitotal) %>% 
  mutate(model = str_remove(model, "pred_")) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = forecast, color = model))


flu %>%
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(week_start, pred_tps), data = flu_test, color = "blue") +
  geom_line(aes(week_start, pred_gam), data = flu_test, color = "red") +
  geom_line(aes(week_start, pred_mars), data = flu_test, color = "green")

# mars and gam pretty close so compare numerically
yardstick::metrics(flu_test, truth = ilitotal, estimate = pred_gam)
yardstick::metrics(flu_test, truth = ilitotal, estimate = pred_mars)

# and the winner is.... MARS!



# further investigation into GAM

gam2 <- gam(ilitotal ~ s(t, bs = "gp"), data = flu_train)
gam2 <- gam(ilitotal ~ s(year, bs = "gp") + s(week, bs = "gp"),
            data = flu_train)
gam2 <- gam(ilitotal ~ s(t, bs = "cc"),
            data = flu_train)
gam2 <- gam(ilitotal ~ s(t, bs = "cr"),
            data = flu_train)
gam2 <- gam(ilitotal ~ s(t, bs = "ad"),
            data = flu_train)

flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = fitted(gam2)), color = 'grey')

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
