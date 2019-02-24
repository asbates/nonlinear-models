
# this is a simple example to get a visual comparison spline methods
# here we look at 'regular', natural, and smoothing splines
# for a discussion see https://asbates.rbind.io as this is really
#  just some prep for a blog post

library(tidyverse)
library(splines)

theme_set(theme_bw())
turq <- "#00CED1"

# generate some data
set.seed(42)
x <- rnorm(500)
y <- 1 + 2*x + 3*x^2 + 4*x^3 + rnorm(500, sd = 5)
true_y <- 1 + 2*x + 3*x^2 + 4*x^3
df <- tibble(x, y, true_y)


ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, true_y), color = turq)


# fit 3 types of splines
b_spline <- lm(y ~ bs(x, df = 6), data = df)
natural_spline <- lm(y ~ ns(x, df = 6), data = df)
smooth_spline <- smooth.spline(df$x, df$y, cv = TRUE)


ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, true_y), color = turq) +
  geom_line(aes(x, b_spline$fitted.values)) +
  geom_line(aes(x, natural_spline$fitted.values)) +
  geom_line(aes(x, smooth_spline$y))


plot(x, y, pch = 16)
lines(smooth_spline)



# append to original data frame
model_df <- df %>% 
  bind_cols(b_spline = b_spline$fitted.values,
            natural_spline = natural_spline$fitted.values,
            smooth_spline = fitted.values(smooth_spline)) %>% 
  gather(true_y, b_spline, natural_spline, smooth_spline, 
         key = "model", value = "fit")

model_df

ggplot(model_df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, fit, color = model))

