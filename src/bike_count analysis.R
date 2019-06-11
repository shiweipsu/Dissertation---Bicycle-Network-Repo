library(pacman)
p_load(foreign, dplyr, tidyr)

# prepare data
## intersect 05 mile service area with census tract, and manipulate proportion of service area by census tract
slice <- read.dbf("src/data/censustrt_05miservice_intsct.dbf")

# block group - TRBG; service area - FacilityID
merge_count_by_tract <- slice %>% 
  group_by(TRACT, FacilityID) %>% 
  summarise(Area_ft = sum(Area_ft), Area_tract=mean(Area_tract), Area_05mi=mean(Area_05mi)) %>% 
  mutate(Area_mi = Area_ft/27878400,prop = round(Area_mi/Area_tract,3), prop_div = round(Area_mi/Area_05mi,3)) %>% 
  filter(prop>0.001)

count <- read.dbf("src/data/BikeCount1117.dbf") %>% mutate(FacilityID = seq(1:143))

# clean ACS data
# need population, race, elder, gender, education, income, vehicle ownership information
sex_age <- read.csv("Data/ACS/PDX/Censustract/ACS_17_5YR_S0101_with_ann.csv") %>% 
  filter(GEO.id!="Id") %>% 
  select(GEO.id2, HC01_EST_VC01, HC03_EST_VC01, HC01_EST_VC33) %>% 
  rename(tot_pop = HC01_EST_VC01, male = HC03_EST_VC01, elder = HC01_EST_VC33)

race <- read.csv("Data/ACS/PDX/Censustract/ACS_17_5YR_B02001_with_ann.csv") %>% 
  filter(GEO.id!="Id") %>% 
  select(GEO.id2, HD01_VD01, HD01_VD02) %>% 
  rename(tot_pop=HD01_VD01, white = HD01_VD02)

income <- read.csv("Data/ACS/PDX/Censustract/ACS_17_5YR_S1901_with_ann.csv")%>% 
  filter(GEO.id!="Id") %>% 
  select(GEO.id2, HC01_EST_VC01, HC01_EST_VC13, HC01_EST_VC15) %>%
  rename(hh = HC01_EST_VC01, median_inc = HC01_EST_VC13, mean_inc = HC01_EST_VC15)

edu <-  read.csv("Data/ACS/PDX/Censustract/ACS_17_5YR_S1501_with_ann.csv")%>% 
  filter(GEO.id!="Id") %>% 
  select(GEO.id2, HC01_EST_VC02, HC01_EST_VC05, HC01_EST_VC06, 
         HC01_EST_VC08, HC01_EST_VC12, HC01_EST_VC13, HC01_EST_VC14, HC01_EST_VC15 ) %>%
  rename(pop_24 = HC01_EST_VC02, pop_24_col = HC01_EST_VC05, pop_24_bac = HC01_EST_VC06, 
         pop_25 =HC01_EST_VC08, pop_25_col = HC01_EST_VC12, pop_25_ass= HC01_EST_VC13,
         pop_25_bac = HC01_EST_VC14, pop_25_grd = HC01_EST_VC15) %>% 
  mutate_at(vars(starts_with("pop")), funs(as.numeric(as.character(.)))) %>% 
  mutate(col_24 = pop_24_col + pop_24_bac,
         col_25 = pop_25_col + pop_25_ass + pop_25_bac + pop_25_grd)

veh <-  read.csv("Data/ACS/PDX/Censustract/ACS_17_5YR_B08201_with_ann.csv")%>% 
  filter(GEO.id!="Id") %>% 
  select(GEO.id2, HD01_VD01, HD01_VD03) %>% 
  rename(hh = HD01_VD01, no_veh = HD01_VD03) 
  
ACS_17 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(sex_age, race, edu, veh,income)) %>% 
  mutate_each(funs(as.numeric(as.character(.))),-GEO.id2) %>% 
  select(-c(8,9,11:14)) %>% mutate(TRACT=substr(GEO.id2, 6,11))

fac_ACS <- left_join(merge_count_by_tract, ACS_17) %>% 
  mutate_at(vars(hh:no_veh),funs(.*prop)) %>% 
  mutate_at(vars(median_inc:mean_inc),funs(.*prop_div)) %>% 
  group_by(FacilityID, Area_05mi) %>% 
  summarise_at(vars(hh:mean_inc), funs(sum(.,na.rm=T)))

count_ACS <- count %>% 
  select(FacilityID, X2011, X2017) %>% 
  merge(fac_ACS)

# edge_buff05 intsct
intsct <- read.dbf("src/data/edge_buff05_intsct.dbf")
intsct_sevnolts <- intsct %>% 
  mutate(LTS = ifelse(service %in% c("drive-through", "driveway","driveway;parking_aisle","parking","parking_aisle"),0,LTS))

LTS <- intsct_sevnolts %>% 
  group_by(FacilityID, LTS) %>% 
  summarise(length=sum(as.numeric(as.character(length)))) %>% 
  ungroup() %>% 
  spread(LTS,length) 
colnames(LTS) <- c("FacilityID", "LTS0", "LTS1", "LTS2", "LTS3", "LTS4")

LTS <- LTS %>% 
  mutate(link = rowSums(LTS[,c("LTS1", "LTS2", "LTS3", "LTS4")], na.rm=TRUE),
         low_link = rowSums(LTS[,c("LTS1", "LTS2")], na.rm=TRUE),
         high_link = rowSums(LTS[,c("LTS3", "LTS4")], na.rm=TRUE),
         low_perc = low_link/link,
         high_perc = high_link/link)

# count_edge intsec
count_edge <- read.dbf("src/data/count_edge_intsct.dbf")
count_edge <- count_edge %>% group_by(Id) %>% 
  summarise( n=n(),LTS_avg = mean(LTS), LTS_high=sum(LTS>2)/n)
count <- left_join(count, count_edge[c("Id","LTS_avg","LTS_high")], by="Id")

# read UNA result
una <- read.dbf("src/data/UNA_result_Featureclass.dbf") %>% mutate(FacilityID = seq(1,143)) %>% 
  select(FacilityID, Betweennes, Straightne, Closeness)

# get number of nodes within each buffer zone
node <- read.dbf("src/data/censustrt_05mi_nodecount.dbf") %>% 
  group_by(FacilityID) %>% summarise(count = sum(Join_Count))

# clean crime data
crime <- read.csv("src/data/crime_portland.csv") %>% filter(Neighborhood!="") %>% 
  mutate(year_char = as.character(Occur.Date),
         year = substr(year_char, nchar(year_char)-3, nchar(year_char))) %>% 
  filter(year==2017) %>% group_by(Neighborhood) %>% summarise(n=n()) %>% 
  mutate(Neighborhood = toupper(Neighborhood))
# nbo <- read.dbf("src/data/Neighborhoods_pdx.dbf") %>% 
  # left_join(crime, by=c("NAME"="Neighborhood")) %>% select(-starts_with("mfh"), -starts_with("sfh")) 
# write.dbf(nbo, "src/data/Neighborhoods_pdx.dbf")
count_crime <- read.dbf("src/data/nbo_count_intsct.dbf")
count <- left_join(count, count_crime[,c("Id","n")], by="Id")


all <- Reduce(function(x,y) merge(x,y,by="FacilityID",all=TRUE) ,list(count[,c("FacilityID","n","LTS_avg","LTS_high")],
                                                                      count_ACS, LTS, una, node))%>% 
  mutate(pop_den = tot_pop/Area_05mi, noveh_perc = no_veh/hh, elder_perc = elder/tot_pop, male_perc = male/tot_pop,
         white_perc = white/tot_pop, col1 = col_24/pop_24, col2 = col_25/pop_25)
data <- na.omit(all)
model <- glm.nb(X2017~Betweennes + Closeness + pop_den + noveh_perc + elder_perc + white_perc + col1 + col2
            + median_inc +  LTS_avg + low_perc + Area_05mi + n, data=data)
model <- glm.nb(X2017~ Closeness + pop_den + col1 + LTS_avg + low_perc, data=data)
summary(model)
library(MASS)
summary(stepAIC(model))

