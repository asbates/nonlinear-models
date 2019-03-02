
# simple example to explore the earth package implementation of MARS


library(earth)


mars <- earth(O3 ~., data = ozone1)
mars
summary(mars)

methods(class = "earth")

coef(mars)
fitted(mars)
evimp(mars)

plot(mars)

plot(evimp(mars))

plotmo(mars)


oz <- ozone1[, c("O3", "wind", "humidity", "temp")]
mars <- earth(O3 ~., data = oz, degree = 2)

