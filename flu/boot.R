

# this file looks at using a time series bootstrap to generate forecast
#  intervals

library(tidyverse)
library(forecast)
library(rsample)
library(yardstick)
library(mgcv)
library(earth)
library(boot)

flu <- read_csv("data/ilinet-calif-up-to-2019-03-31.csv")

# -------------------------------------------
# ----------- model selection ---------------
# -------------------------------------------

# make ts object for plotting to determine number of lags
flu_train_ts <- flu %>% 
  filter(week_start < "2019-01-01") %>% 
  pull(ilitotal) %>% 
  ts(., frequency = 52)

Acf(flu_train_ts)
Pacf(flu_train_ts)
gglagplot(flu_train_ts)


# consider up to 2 lags
flu <- flu %>% 
  mutate(
    lag1 = lag(ilitotal),
    lag2 = lag(ilitotal, 2L)
  ) %>%
  filter(!is.na(lag2))

flu_train <- filter(flu, week_start < "2019-01-01")
flu_test <- filter(flu, week_start >= '2019-01-01')


# cross validation to decide on which model & how many lags
ro_split <- rolling_origin(
  flu_train,
  initial = 52,
  assess = 13,
  skip = 12
)

ro_split
ro_split$splits[[1]]$in_id
ro_split$splits[[1]]$out_id
ro_split$splits[[2]]$in_id
ro_split$splits[[2]]$out_id

# ----- model fitting ------
fit_gam_lag1 <- function(split, ...){
  gam(ilitotal ~ s(lag1), data = analysis(split), method = "REML")
}

fit_gam_lag2 <- function(split, ...){
  gam(ilitotal ~ s(lag1) + s(lag2), data = analysis(split), method = "REML")
}

fit_mars_lag1 <- function(split, ...){
  earth(ilitotal ~ lag1, data = analysis(split))
}

fit_mars_lag2 <- function(split, ...){
  earth(ilitotal ~ lag1 + lag2, data = analysis(split))
}


ro_split <- ro_split %>% 
  mutate(
    gam_lag1 = map(splits, fit_gam_lag1),
    gam_lag2 = map(splits, fit_gam_lag2),
    mars_lag1 = map(splits, fit_mars_lag1),
    mars_lag2 = map(splits, fit_mars_lag2)
  )


ro_split$gam_lag1[[1]]
ro_split$mars_lag1[[1]]

# ------ predictions --------
gam_pred <- function(split, ...){
  predict(..., newdata = assessment(split))
}

mars_pred <- function(split, ...){
  predict(..., newdata = assessment(split))[,1]
}

ro_split <- ro_split %>% 
  mutate(
    gam_lag1_pred = map2(splits, gam_lag1, gam_pred),
    gam_lag2_pred = map2(splits, gam_lag2, gam_pred),
    mars_lag1_pred = map2(splits, mars_lag1, mars_pred),
    mars_lag2_pred = map2(splits, mars_lag2, mars_pred)
  )

ro_split
ro_split$mars_lag2_pred[[1]]


# ----- CV error ------

fcast_error <- function(split, ...){
  rmse(assessment(split), truth = ilitotal, estimate = ...)$.estimate
}

ro_split <- ro_split %>% 
  mutate(
    gam_lag1_err = map2_dbl(splits, gam_lag1_pred, fcast_error),
    gam_lag2_err = map2_dbl(splits, gam_lag2_pred, fcast_error),
    mars_lag1_err = map2_dbl(splits, mars_lag1_pred, fcast_error),
    mars_lag2_err = map2_dbl(splits, mars_lag2_pred, fcast_error)
  )

ro_split %>% 
  select(ends_with("err"))

ro_split %>% 
  select(ends_with("err")) %>% 
  map_dbl(mean)

# gam_lag1_err  gam_lag2_err mars_lag1_err mars_lag2_err 
# 137.8208      134.8649      140.1484      135.4901


# --------------------------------
# ------ forecasts & errors ------

gam_mod <- gam(ilitotal ~ s(lag1) + s(lag2),
               data = flu_train,
               method = "REML")

checkresiduals(gam_mod, test = "LB")

# flu_train_ts <- flu_train %>% 
#   select(ilitotal, lag1, lag2) %>% 
#   ts()
# 
# flu_test_ts <- flu_test %>% 
#   select(ilitotal, lag1, lag2) %>% 
#   ts()
# 
# fcast <- function(fit_series, fcast_series){
#   fit <- gam(ilitotal ~ s(lag1) + s(lag2),
#              data = as.data.frame(fit_series))
#   predict(fit, newdata = fcast_series)
# }
# 
# 
# 
# sb_boot <- tsboot(
#   flu_train_ts,
#   statistic = fcast,
#   R = 2,
#   sim = "geom",
#   l = 5,
#   flu_test_ts
# )
# 
# 
# get_series <- function(series){
#   series
# }
# 
# series_index <- 1:nrow(flu_train)
# 
# sb_boot <- tsboot(
#   flu_train_ts,
#   statistic = get_series,
#   R = 2,
#   sim = "geom",
#   l = 5
# )
# 
# all.equal(sb_boot$t0, flu_train_ts)
# 
# class(sb_boot$t)
# dim(sb_boot$t)
# 
# sb_boot$t[1,]
# 
# 
# sb_boot_indices <- tsboot(
#   series_index,
#   statistic = get_series,
#   R = 2,
#   sim = "geom",
#   l = 5
# )
# 
# sb_boot_indices$t0
# sb_boot_indices$t
# dim(sb_boot_indices$t)
# class(sb_boot_indices$t[1,])
# 
# as.data.frame(sb_boot_indices$t)
# 
# id1 <- sb_boot_indices$t[1,]

# ---------- forecast intervals ------
# we want to be able to keep the bootstrapped series but tsboot
#  gives back the statistics after drawing each bootstrap sample
# so just have it return the indices which we can use to get the actual 
#  bootstrapped series

index_series <- 1:nrow(flu_train)

return_series <- function(series){
  series
}

nboot <- 50

set.seed(843)
sb_boot_indices <- tsboot(
  index_series,
  statistic = return_series,
  R = nboot,
  sim = "geom",
  l = 5
)

sb <- as_tibble(sb_boot_indices$t) %>% 
  mutate(resample = row_number()) %>% 
  nest(-resample)

sb

sb <- sb %>% 
  mutate(sb_ind = map(data, as.integer))

sb

sb_index <- sb$sb_ind
sb_series <- vector("list", nboot)
sb_series <- map(sb_index, ~slice(flu_train, .))

sb <- sb %>% 
  mutate(sb_series = sb_series)


test_series <- vector("list", nboot)
for(i in seq_along(test_series)) test_series[[i]] <- flu_test

sb$test_series <- test_series           
sb

fcast <- function(fit_series, fcast_series){
  fit <- gam(ilitotal ~ s(lag1) + s(lag2),
             data = as.data.frame(fit_series))
  predict(fit, newdata = fcast_series)
}

sb <- sb %>% 
  mutate(fcast = map2(sb_series, test_series, fcast))

sb

fcast_error <- function(split, ...){
  rmse(assessment(split), truth = ilitotal, estimate = ...)$.estimate
}

fcast_error <- function(series, forecast){
  rmse_vec(series$ilitotal, forecast)
}

sb <- sb %>% 
  mutate(fcast_err = map2_dbl(test_series, fcast, fcast_error))

sb %>% 
  select(fcast_err) %>% 
  map_dbl(mean)

# 242.996

# ------- plot bootstrapped series -----------
# sb %>% 
#   select(resample, data) %>% 
#   mutate(
#     ilitotal = map_dbl(data, ~.$ilitotal)
#   )
# 
# sb %>% 
#   mutate(
#     low = map_dbl()
#   )
# 
# map_dbl(sb$fcast, mean)
# 
# map_df(sb$fcast, as_tibble)
# 
# sb %>% 
#   select(resample, fcast) %>% 
#   unnest(fcast) %>% View()
# 
# sb %>% 
#   select(resample, test_series, fcast) %>% 
#   unnest() %>% 
#   View()
# 
# sb %>% 
#   select(resample, test_series, fcast) %>% 
#   unnest() %>% 
#   ggplot(aes(week_start, ilitotal)) +
#   geom_line() +
#   geom_line(aes(y = fcast, color = resample))

sb_quantiles <- sb %>% 
  select(resample, test_series, fcast) %>% 
  unnest() %>% 
  group_by(week_start) %>% 
  mutate(
    lower_q = quantile(fcast, 0.025),
    upper_q = quantile(fcast, 0.975)
  ) %>%
  select(week_start, lower_q, upper_q) %>% 
  ungroup() %>% 
  slice(1:13)


flu_test %>% 
  inner_join(sb_quantiles, by = c("week_start")) %>%
  ggplot(aes(week_start, ilitotal)) +
  geom_ribbon(aes(ymin = lower_q, ymax = upper_q), fill = "grey") +
  geom_line()

# basic bootstrap interval
flu_test %>% 
  inner_join(sb_quantiles, by = c("week_start")) %>%
  mutate(
    fcast = predict(gam_mod, newdata = .),
    lower = 2 * fcast - upper_q,
    upper = 2 * fcast - lower_q
  ) %>%
  ggplot(aes(week_start, ilitotal)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey") +
  geom_line()


# flu_train_ts <- flu_train %>% 
#   select(ilitotal, lag1, lag2) %>% 
#   ts()
# 
# flu_test_ts <- flu_test %>% 
#   select(ilitotal, lag1, lag2) %>% 
#   ts()

# alternatively use tsboot()

nboot <- 5
err_fun <- function(fit_series, test_series){
  fit <- gam(ilitotal ~ s(lag1) + s(lag2),
             data = fit_series,
             method = "REML")
  #pred <- predict(fit, newdata = test_series)
  predict(fit, newdata = test_series)
  
  #c(series, rmse_vec(fit_series$ilitotal, pred))
  #rmse_vec(test_series$ilitotal, pred)
}

bootpred <- tsboot(
  flu_train,
  statistic = err_fun,
  R = nboot,
  sim = "geom",
  l = 5,
  test_series = flu_test # needs to be NAMED!!!!!
)

bootpred
bootpred$t0
bootpred$t
err_fun(flu_train, flu_test)

boot.ci(bootpred, type = "basic")

flu_test %>% 
  mutate(pred = predict(gam_mod, newdata = .)) %>% 
  rmse(ilitotal, pred)

as.data.frame(bootpred$t)
map_dbl(as.data.frame(bootpred$t), ~quantile(., 0.025))
