if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(dplyr)

meta <- read.csv("Data/bike count/bike_metadata.csv")
count <- read.csv("Data/bike count/bike_count_data.csv")

# find how many flow_detector_id each city has
meta %>% mutate(year = substr(as.character(start_time),1,4)) %>% 
  group_by(name,make) %>% 
  summarise(n = n())

count_number <- count %>% 
  mutate(year = substr(as.character(start_time),1,4)) %>% 
  group_by(flow_detector_id) %>% 
  summarise(year_start = min(year),
            year_end = max(year))

meta_count <- left_join(meta, count_number, by="flow_detector_id")

pdx_counter <- meta_count %>% filter(name=="Multnomah") %>% 
  select(flow_detector_id, start_time, end_time, year_start, year_end, make)

pdx_counter_start <- meta_count %>% filter(name=="Multnomah") %>% 
  group_by(name,year_start,make) %>% 
  summarise(n_start=n())
pdx_counter_end <- meta_count %>% filter(name=="Multnomah") %>% 
  group_by(name,year_end,make) %>% 
  summarise(n_end=n())

