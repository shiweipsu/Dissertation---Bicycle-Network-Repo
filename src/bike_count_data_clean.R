# load data

if(!require("pacman")){install.packages("pacman"); library(pacman)}
p_load(dplyr)

count <- read.csv("Data/bike count/bike_count_data.csv")
meta <- read.csv("Data/bike count/bike_metadata.csv")

# find the start year of each flow_detector_id
count$year <- substring(as.character(count$start_time),1,4)

sty <- count %>% 
  group_by(flow_detector_id) %>% 
  summarise(year = min(year))

meta <- meta %>% left_join(sty)

meta_seg <- meta %>% 
  group_by(name,segment_area_id) %>% 
  summarise(year = min(year, na.rm=T))

# find number of counts at different time
table(meta[meta$year<=2011,]$name) 
table(meta[meta$year<=2017,]$name)

table(meta_seg[meta_seg$year<=2012,]$name) 
table(meta_seg[meta_seg$year<=2017,]$name) 


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

