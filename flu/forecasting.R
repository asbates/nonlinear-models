
# this file looks at using the forecast package for forecasting the flu data

library(tidyverse)
library(forecast)

theme_set(theme_bw())

flu_df <- read_csv("data/ilinet-calif-up-to-2019-03-31.csv")

flu_train_df <- filter(flu_df, week_start < "2019-01-01")
flu_test_df <- filter(flu_df, week_start >= "2019-01-01")


flu <- ts(flu_df$ilitotal, start = c(2010, 40), frequency = 52)
flu_train <- subset(flu, end = nrow(flu_train_df))
flu_test <- subset(flu, start = nrow(flu_train_df) + 1)

# plotting functions
# note in particualar seasonplot and lagplot
autoplot(flu)
ggseasonplot(flu)
ggsubseriesplot(flu)
gglagplot(flu)
gglagchull(flu)
ggAcf(flu)
ggPacf(flu)




# flu_ts_train <- flu_ts[1:nrow(flu_train)]
# flu_ts_test <- flu_ts[(nrow(flu_train) + 1):nrow(flu)]


# still has some autocorrelation left

#----- ARIMA -----
arima_mod <- auto.arima(flu_train)

# this took a long time to fit so let's save it
write_rds(arima_mod, "flu/auto_arima_model.rds")

checkresiduals(arima_mod)

arima_fcast <- forecast(arima_mod, h = 13)
autoplot(flu) +
  autolayer(arima_fcast, PI = FALSE)


# ---- seasonal, trend decomposition via loess -----
# not in the forecast package
stl_mod <- stl(flu_train, s.window = "periodic")

autoplot(stl_mod)

# stl decomposition and forecast on seasonally adjusted series
stlf_mod <- stlf(flu_train, h = 13)

# forecasts use ets (exponential smoothing)
stlf_mod$model

# terrible!
autoplot(flu) +
  autolayer(stlf_mod, PI = FALSE)


# ----- regression on fourier coefficients with arima errors -----
# this code taken from Forecasting: Principles and Practice chapter 12.1
# it determines the optimal number of fourier terms to use

fourier_mod <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(flu_train, xreg=fourier(flu_train, K=K), seasonal=FALSE)
  if(fit[["aicc"]] < fourier_mod[["aicc"]]) {
    fourier_mod <- fit
    best_k <- K
  }
}

# best k is 8 (num fourier_terms)

fourier_fcast <- forecast(bestfit, xreg=fourier(flu_train, K = best_k, h = 13))
autoplot(flu) +
  autolayer(fourier_fcast, PI = FALSE)


#  ------ TBATS -----------
# automated model using fourier terms with ets model and box-cox transform
# seasonality can change slowly over time

tbats_mod <- tbats(flu_train)

tbats_mod
write_rds(tbats_mod, "flu/tbats_model.rds")

tbats_fcast <- forecast(tbats_mod, h = 13)
autoplot(flu) +
  autolayer(tbats_fcast, PI = FALSE)


# ------- neural net --------
# single layer neural net fit to lags
# fits from multiple random starting weights averaged together

nnet_mod <- nnetar(flu_train)
# 1 lag, 1 seasonal lag, 2 units in hidden layer
nnet_mod

nnet_fcast <- forecast(nnet_mod, h = 13)

autoplot(flu) +
  autolayer(nnet_fcast, PI = FALSE)



# -------- bagging ---------
# uses ets as default but can specify modeling function via 'fn' argument
# if leave as ets, get warnings that seasonality is ignored b/c
#  ets only handles frequency up to 24

# fourier regression with arima error as before
bagged_fourier <- baggedModel(
  flu_train,
  bootstrapped_series = bld.mbb.bootstrap(flu_train, 50),
  fn = auto.arima,
  xreg = fourier(flu_train, K = 8),
  seasonal = FALSE
)

write_rds(bagged_fourier, "flu/bagged_fourier.rds")

bagged_fourier_fcast <- forecast(bagged_fourier,
                                 xreg = fourier(flu_train, K = 8, h = 13))
autoplot(flu) +
  autolayer(bagged_fourier_fcast, PI = FALSE)


# tbats
bagged_tbats <- baggedModel(
  flu_train,
  bootstrapped_series = bld.mbb.bootstrap(flu_train, 50),
  fn = tbats
)

write_rds(bagged_tbats, "flu/bagged_tbats.rds")

bagged_tbats_fcast <- forecast(bagged_tbats, h = 13)

autoplot(flu) +
  autolayer(bagged_tbats_fcast, PI = FALSE)



