

library(cdcfluview)
library(tidyverse)

ili <- ilinet("state")

calif <- ili %>% 
  filter(region == "California") %>% 
  select(week_start,
         unweighted_ili,
         ilitotal,
         num_of_providers,
         total_patients)

write_csv(calif, "data/ilinet-calif-up-to-2019-03-31.csv")
