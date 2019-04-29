
# this file take a closer look at using GAMs for time series than what i had
#  previously done
# i also used it to play around with the gratia package for ggplot mgcv plotting

# devtools::install_github('gavinsimpson/gratia')
library(tidyverse)
library(mgcv)
library(gratia)
library(forecast)

theme_set(theme_bw())

flu <- read_csv("data/ilinet-calif-up-to-2019-03-31.csv")
flu_train <- filter(flu, week_start < "2019-01-01")
flu_test <- filter(flu, week_start >= "2019-01-01")


flu %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line()

# model I had previously used
old_mod <- gam(
  ilitotal ~ s(year) + s(week),
  data = flu_train,
  method = "REML"
)


# plots with gratia
draw(old_mod)
draw(old_mod, scales = "fixed") # all smooths on same scale (y-axis)

# diagnostics
appraise(old_mod)

# or specify a particular plot
qq_plot(old_mod)

gam.check(old_mod) # probably shouldn't have year in there


# really should have done tensor product smooths b/c year, week are different
#  scales
te_mod <- gam(
  ilitotal ~ te(year) + te(week),
  data = flu_train,
  method = "REML"
)

# looks about the same
draw(te_mod)
appraise(te_mod)

gam.check(te_mod) # neither should be there


# should there be an interaction?
te_interact_mod <- gam(
  ilitotal ~ te(year) + te(week) + ti(year, week),
  data = flu_train,
  method = "REML"
)

draw(te_interact_mod)
appraise(te_interact_mod)
gam.check(te_interact_mod)


week_only <- gam(
  ilitotal ~ s(week),
  data = flu_train,
  method = "REML"
)

draw(week_only)
appraise(week_only)
gam.check(week_only) # seems OKish

# not very good
flu_train %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = fitted(week_only)), color = "blue")


forecast::Acf(residuals(week_only))


# using gamm with 2 m's to account for correlation structure

# cyclic cubic spline
cc_mod <- gamm(
  ilitotal ~ s(year) + s(week, bs = "cc", k = 52),
  data = flu_train
)

draw(cc_mod$gam)

flu_train %>% 
  mutate(resid = residuals(cc_mod$gam)) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 30)

flu_train %>% 
  mutate(resid = residuals(cc_mod$gam)) %>% 
  ggplot(aes(sample = resid)) +
  geom_qq()

ggAcf(residuals(cc_mod$lme))
ggPacf(residuals(cc_mod$lme))

summary(cc_mod$gam)

gam.check(cc_mod$gam)
par(mfrow = c(2,2))
gam.check(cc_mod$gam)
par(mfrow = c(1,1))

# with AR(1) correlation structure
cc_ar1_mod <- gamm(
  ilitotal ~ s(year) + s(week, bs = "cc", k = 52),
  data = flu_train,
  correlation = corARMA(form = ~ 1|year, p = 1)
)

draw(cc_ar1_mod$gam)

flu_train %>% 
  mutate(resid = residuals(cc_ar1_mod$gam)) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 30)

flu_train %>% 
  mutate(resid = residuals(cc_ar1_mod$gam)) %>% 
  ggplot(aes(sample = resid)) +
  geom_qq()

ggAcf(residuals(cc_ar1_mod$lme, type = "normalized"))
ggPacf(residuals(cc_ar1_mod$lme, type = "normalized"))

par(mfrow = c(2,2))
gam.check(cc_ar1_mod$gam)
par(mfrow = c(1,1))

# should be checking residuals of lme
flu_train %>% 
  mutate(resid = residuals(cc_ar1_mod$lme)) %>% 
  ggplot(aes(x = resid)) +
  geom_histogram(bins = 30)

flu_train %>% 
  mutate(resid = residuals(cc_ar1_mod$lme)) %>% 
  ggplot(aes(sample = resid)) +
  geom_qq()

# AR(2) correlation structure
cc_ar2_mod <- gamm(
  ilitotal ~ s(year) + s(week, bs = "cc", k = 52),
  data = flu_train,
  correlation = corARMA(form = ~ 1|year, p = 2)
)

draw(cc_ar2_mod$gam)

par(mfrow = c(2,2))
gam.check(cc_ar2_mod$gam)
par(mfrow = c(1,1))

ggAcf(residuals(cc_ar2_mod$lme))
ggPacf(residuals(cc_ar2_mod$lme))


anova(cc_mod$lme, cc_ar1_mod$lme, cc_ar2_mod$lme)  # ar(1)


cc_ar1_mod2 <- gamm(
  ilitotal ~ s(year) + s(week, bs = "cc", k = 52),
  data = flu_train,
  correlation = corARMA(form = ~ week|year, p = 1)
)

draw(cc_ar1_mod2$gam)


# with time

flu_train2 <- flu_train %>% 
  mutate(t = 1:nrow(.))

ccmod2 <- gamm(
  ilitotal ~ s(t) + s(week, bs = "cc", k = 52),
  data = flu_train2
)

draw(ccmod2$gam)

acf(residuals(ccmod2$lme))
pacf(residuals(ccmod2$lme))



ccar1mod2 <- gamm(
  ilitotal ~ s(t) + s(week, bs = "cc", k = 52),
  data = flu_train2,
  correlation = corARMA(form = ~ 1|year, p = 1)
)

draw(ccmod2$gam)  # same

acf(residuals(ccar1mod2$lme))
pacf(residuals(ccar1mod2$lme))


ccar2mod2 <- gamm(
  ilitotal ~ s(t) + s(week, bs = "cc", k = 52),
  data = flu_train2,
  correlation = corARMA(form = ~ 1|year, p = 2)
)


acf(residuals(ccar2mod2$lme))
pacf(residuals(ccar2mod2$lme))

anova(ccmod2$lme, ccar1mod2$lme, ccar2mod2$lme)  # ar(2)


ccarma <- gamm(
  ilitotal ~ s(t) + s(week, bs = "cc", k = 52) + s(year),
  data = flu_train2,
  correlation = corARMA(form = ~ 1|year, p = 2, q = 2)
)

acf(resid(ccarma$lme))
pacf(resid(ccarma$lme))


flu_train %>% 
  mutate(fit = fitted(cc_ar1_mod$lme)) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = fit), color = "blue")


flu_train %>% 
  mutate(fit = fitted(ccar1mod2$lme)) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = fit), color = "blue")


# cyclic p-spline smooth

flu_train <- flu_train %>% 
  mutate(time = as.numeric(week_start) / 1000)

cp_mod <- gamm(
  ilitotal ~ s(time) + s(week, bs = "cp", k = 52),
  data = flu_train
)

acf(residuals(cp_mod$lme))
pacf(residuals(cp_mod$lme))

cp_ar1_mod <- gamm(
  ilitotal ~ s(time) + s(week, bs = "cp", k = 52),
  data = flu_train,
  correlation = corAR1(form = ~ 1|year)
)

acf(residuals(cp_ar1_mod$lme))
pacf(residuals(cp_ar1_mod$lme))


# gaussian process smooth

gp_mod <- gam(
  ilitotal ~ s(time, bs = "gp") + s(week, bs = "cc", k = 52),
  data = flu_train
)

draw(gp_mod)
appraise(gp_mod)


gp_ar1 <- gamm(
  ilitotal ~ s(time, bs = "gp") + s(week, bs = "cc", k = 52),
  data = flu_train,
  correlation = corARMA(form = ~ 1|year, p = 2)
)

acf(residuals(gp_ar1$lme))
pacf(residuals(gp_ar1$lme))


gp_log_mod <- gam(
  log(ilitotal) ~ s(time, bs = "gp") + s(week, bs = "cc", k = 52),
  data = flu_train
)

draw(gp_log_mod)
appraise(gp_log_mod)

acf(residuals(gp_log_mod))
pacf(residuals(gp_log_mod))


gp_log_ar1 <- gamm(
  log(ilitotal) ~ s(time, bs = "gp") + s(week, bs = "cc", k = 52),
  data = flu_train,
  correlation = corARMA(form = ~ 1|year, p = 1)
)

acf(residuals(gp_log_ar1$lme))
pacf(residuals(gp_log_ar1$lme))


flu_train %>% 
  mutate(gpfit = exp(fitted(gp_log_mod)),
         gpar1fit = exp(fitted(gp_log_ar1$gam))) %>% 
  ggplot(aes(week_start, ilitotal)) +
  geom_line() +
  geom_line(aes(y = gpfit), color = "blue") +
  geom_line(aes(y = gpar1fit), color = "green")

# still underestimating the peaks