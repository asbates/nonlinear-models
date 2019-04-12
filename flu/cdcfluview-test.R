
# this is a test file to play with the cdcfluview package
# i previously looked at flu data from CDC FluView and was not very pleased
#   with the process or the data
# after looking around the CDC website some more and finding FluSight, a flu
#   forecasting challenge, i came across the cdcfluview package
# it should make things a lot easier


# devtools::install_github("hrbrmstr/cdcfluview")

library(cdcfluview)
library(tidyverse)

ili <- ilinet("state")
ili

head(ili, 100) %>% View()

# convert between MMWR week (Morbidity and Mortality Weekly Report) and dates
# this is really cool! 
# the date format bothered me most about the data i looked at before
mmwr_week(ili$week_start)
mmwr_week_to_date(2018, 40)

# note that weeks here start on Sundays

ili %>% 
  select(week_start) %>% 
  summarise(min(week_start), max(week_start))

# as of april 12, data goes up to march 31

ili %>% 
  filter(region == "California") %>% 
  ggplot(aes(week_start, total_patients)) +
  geom_line()

national_ili <- ilinet("national")

national_ili %>% 
  ggplot(aes(week_start, total_patients)) +
  geom_line()

# ooh there's more data here
# but there are several flat splots (zeros) in the early years
# that seems a bit questionable to me
national_ili %>% 
  summarise(min(week_start), max(week_start))

# deaths from flu and pneumonia
#  this is what i looked at before
mortal <- pi_mortality("national")

mortal %>% head(100) %>% View()

levels(as.factor(mortal$geo_description))

# wait 'wk_start' instead of 'week_start'? why?
mortal %>% 
  ggplot(aes(wk_start, number_influenza)) +
  geom_line()


# going to submit an issue for inconsistency of naming
# https://github.com/hrbrmstr/cdcfluview/issues/21





