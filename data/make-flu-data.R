

library(cdcfluview)
library(tidyverse)

ili <- ilinet("state")

calif <- ili %>% 
  filter(region == "California", week_start < "2019-04-01") %>% 
  select(week_start,
         year,
         week,
         unweighted_ili,
         ilitotal,
         num_of_providers,
         total_patients)

write_csv(calif, "data/ilinet-calif-up-to-2019-03-31.csv")
