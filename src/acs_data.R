if(!require("pcacman")){install.packages("pacman"); library(pacman)}
p_load(readxl, stringr, tidyr, dplyr, ggplot)


acs_bike_big <- read_excel("Data/ACS/supplemental-table3 (1).xlsx", skip = 5) %>% mutate(type="Big")
acs_bike_median <- read_excel("Data/ACS/supplemental-table2 (1).xlsx", skip = 5) %>% mutate(type="Mid")
acs_bike_small <- read_excel("Data/ACS/supplemental-table1 (1).xlsx", skip = 5) %>% mutate(type="Small")

acs_bike <- rbind(acs_bike_big, acs_bike_median, acs_bike_small)
colnames(acs_bike) <- c("city", "bicycle_number", "bicycle_perc","margin_error","type")
acs_bike$bicycle_perc <- as.numeric(acs_bike$bicycle_perc)
acs_bike <- acs_bike %>% 
  separate(city, c("city", "state"), sep=", ")
  
  
summary(filter(acs_bike, state=="California")$bicycle_perc)
hist(filter(acs_bike, state=="California")$bicycle_perc)

acs_bike_30 <- acs_bike %>% 
  arrange(desc(bicycle_perc)) %>% slice(1:30) %>% 
  mutate(City = paste(city, state, sep=", "))

acs_bike_big <- acs_bike %>% filter(type=="Big") %>% 
  arrange(desc(bicycle_perc)) %>% slice(1:30) %>% 
  mutate(City = paste(city, state, sep=", "))

acs_bike_mid <-  acs_bike %>% filter(type=="Mid") %>% 
  arrange(desc(bicycle_perc)) %>% slice(1:30) %>% 
  mutate(City = paste(city, state, sep=", "))

acs_bike_sm <- acs_bike %>% filter(type=="Small") %>% 
  arrange(desc(bicycle_perc)) %>% slice(1:20) %>% 
  mutate(City = paste(city, state, sep=", "))

acs

write.csv(acs_bike_30, "ACS/acs_bike_30.csv")

acs_bike_ca <- read.csv("ACS/ACS_16_5YR_S0801_CA/ACS_16_5YR_S0801_with_ann.csv", skip = 1) %>% 
  select(1,3, 64)
colnames(acs_bike_ca) <- c("GEOID","Census_Tract", "bike")
acs_bike_alameda <- acs_bike_ca %>% slice(1:154) %>% mutate(bike = as.numeric(as.character(bike)))
hist(acs_bike_alameda$bike)

acs_bike_or <- read.csv("ACS/ACS_16_5YR_S0801_OR/ACS_16_5YR_S0801_with_ann.csv", skip = 1) %>% 
  select(1,3, 64)
colnames(acs_bike_or) <- c("GEOID","Census_Tract", "bike")
acs_bike_or$bike <- as.numeric(as.character(acs_bike_or$bike))
hist(acs_bike_or$bike)
write.csv(acs_bike_or, "ACS/acs_bike_multnomah.csv")
