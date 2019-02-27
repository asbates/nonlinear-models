

# this is a simple script to see thin plate splines in action with the
#  Fields package.
# see my blog https://asbates.rbind.io for a discussion



library(fields)
library(dplyr)
library(lattice)

set.seed(42)
df <- expand.grid(x = -10:10, y = -10:10) %>% 
  mutate(z = y + x^2 + y^2 - x*y - x^2*y + rnorm(n(), 100, 20))

wireframe(z ~ x*y, df)

model_x <- df %>% select(x,y)
model_y <- df %>% select(z)

fit <- Tps(model_x, model_y)

fit
class(fit)

methods(class = "Krig")

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

persp(predictSurface(fit))

?predictSE.Krig
predictSE(fit)

df <- df %>% 
  mutate(true_z = y + x^2 + y^2 - x*y - x^2*y)

pred_tps <- predict(fit)


rmse_tps <- sqrt( mean( (pred_tps - df$true_z)^2 ) )

library(randomForest)
fit_rf <- randomForest(z ~ x + y, df)
pred_rf <- predict(fit_rf)
rmse_rf <- sqrt( mean( (pred_rf = df$true_z)^2 ) )

rmse_tps 
rmse_rf



