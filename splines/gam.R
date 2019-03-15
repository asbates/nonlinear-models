

#this illustrates how to fit GAMs with different types of
#terms and interactions


library(tidyverse)
library(mgcv)
library(AmesHousing)


ames <- make_ames() %>% 
  select(Sale_Price,
         Lot_Area,
         Bldg_Type,
         Gr_Liv_Area,
         Bedroom_AbvGr,
         Full_Bath,
         Garage_Cars)

names(ames) <- tolower(names(ames))

# ----  plots to see relationship with sale price ----

# sale price vs. lot area
ggplot(ames, aes(lot_area, sale_price)) +
  geom_point()

ggplot(ames, aes(lot_area, sale_price)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# sale price vs above ground square feet
ggplot(ames, aes(gr_liv_area, sale_price)) +
  geom_point()

ggplot(ames, aes(gr_liv_area, sale_price)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


ames <- ames %>% 
  mutate(log_sale_price = log(sale_price),
         log_lot_area = log(lot_area),
         log_sf = log(gr_liv_area))


# sale price and lot size as is
linear_lot_area <- gam(sale_price ~ s(lot_area),
                       data = ames,
                       method = "REML")

plot(linear_lot_area)

gam.check(linear_lot_area)


# log sale price vs. log lot area
log_log_lot_area <- gam(log_sale_price ~ s(log_lot_area),
                    data = ames,
                    method = "REML")

plot(log_log_lot_area)

gam.check(log_log_lot_area)


log_log_lot_high_k <- gam(log_sale_price ~ s(log_lot_area, k = 30),
                          data = ames,
                          method = "REML")

gam.check(log_log_lot_high_k)


# separate smooths for lot area and above ground sf
lot_sf <- gam(sale_price ~ s(lot_area) + s(gr_liv_area),
              data = ames,
              method = "REML")

plot(lot_sf)
gam.check(lot_sf)

log_lot_sf <- gam(log_sale_price ~ s(log_lot_area) + s(log_sf),
                  data = ames,
                  method = "REML")

gam.check(log_lot_sf)


# interact lot area and square footage
log_lot_sf_interact <- gam(log_sale_price ~ s(log_lot_area, log_sf),
                           data = ames,
                           method = "REML")

gam.check(log_lot_sf)


# take into account different building types
log_lot_sf_type <- gam(log_sale_price ~ s(log_lot_area) + s(log_sf) + bldg_type,
                       data = ames,
                       method = "REML")

plot(lot_lot_sf_type)
gam.check(log_lot_sf_type)


# with square foot smooths by building type
log_sf_each_type <- gam(log_sale_price ~ s(log_sf, by = bldg_type),
                        data = ames,
                        method = "REML")



# separate smooths by building type for lot size and square foot 
log_lot_sf_each_type <- gam(
  log_sale_price ~ s(log_lot_area, by = bldg_type) + s(log_sf, by = bldg_type),
  data = ames,
  method = "REML"
)

plot(log_lot_sf_each_type)
gam.check(log_lot_sf_each_type)


# with additional types of terms
mixed_fit <- gam(
  log_sale_price ~ s(log_lot_area) + s(log_sf, by = bldg_type) +
    bedroom_abvgr + full_bath + garage_cars,
  data = ames,
  method = "REML"
)

gam.check(mixed_fit)




