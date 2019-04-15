
# model flu data using spline based methods


library(tidyverse)
library(fields)  # tps
library(earth)  # MARS
library(mgcv)  # GAM

theme_set(theme_bw())

flu <- read_csv("data/ilinet-calif-up-to-2019-03-31.csv")

flu

flu %>% 
  ggplot(aes(week_start, unweighted_ili)) +
  geom_line()

flu %>% 
  ggplot(aes(week_start, num_of_providers)) +
  geom_line()

flu %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line()

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
