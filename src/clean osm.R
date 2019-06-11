library(pacman)
p_load(foreign,dplyr,sf)

# merge OSM highway and cycleway tag
edges <- st_read("src/data/portland_network_osm - Copy/edges/edges.shp")
# edges <- read.dbf("src/data/portland_network_osm - Copy/edges/edges.dbf")

edges <- edges %>% 
  mutate(cycleway = ifelse(cycleway=="track","cycletrack",as.character(cycleway))) %>% 
  mutate(cycleway = ifelse(is.na(cycleway)&(highway=="path"|highway=="pedestrian"|highway=="cycleway"), 
                           as.character(highway), as.character(cycleway))) %>% 
  mutate(highway = ifelse((highway=="path"|highway=="pedestrian"|highway=="cycleway"),"cycleway", as.character(highway)))

sum(!is.na(edges$cycleway)) # 37084 have bike facility
table(edges$cycleway)

edges$speed <- ifelse(is.na(edges$maxspeed),NA, as.numeric(substr(edges$maxspeed,1,2)))
edges$lanes <- as.numeric((edges$lanes))

table(edges$lanes)
table(edges$speed)
table(edges$highway)
table(edges$service)

# where tags are not avaialble, lanes and speed limits are inferred from functional classification (Wasserman et al.)
edges <- edges %>% 
  mutate(lanes = ifelse(!is.na(lanes), lanes,case_when(highway=="residential"&oneway=="False" ~2,
                                                       highway=="unclassified"&oneway=="False" ~2,
                                                       highway=="tertiary"& oneway=="False"~3,
                                                       highway=="track"&oneway=="False"~2,
                                                       (highway=="secondary"|highway=="primary")&oneway=="False" ~4,
                                                       highway=="trunk"&oneway=="False"~6,
                                                       highway=="residential"&oneway=="True" ~1,
                                                       highway=="unclassified"&oneway=="True" ~1,
                                                       highway=="tertiary"& oneway=="True"~1.5,
                                                       highway=="track"&oneway=="True"~1,
                                                       (highway=="secondary"|highway=="primary")&oneway=="True" ~2,
                                                       highway=="trunk"&oneway=="True"~3,
                                                       TRUE ~ 2)))

# update speed limit
edge_spl <- st_read("src/data/edge_join_SpeedLimite.dbf")
edge_spl$speed <- ifelse(is.na(edge_spl$maxspeed),NA, as.numeric(substr(edge_spl$maxspeed,1,2)))
edge_spl$speedlim <- as.numeric(as.character(edge_spl$SpeedLim_1))
table(edge_spl$speedlim - edge_spl$speed)

edge_spl <- edge_spl %>% 
  mutate(speedlim = ifelse(!is.na(speedlim), speedlim,case_when(highway=="residential" ~25,
                                                       highway=="unclassified" ~25,
                                                       highway=="tertiary" ~30,
                                                       highway=="track"~30,
                                                       highway=="secondary" ~35,
                                                       highway=="primary"~45,
                                                       highway=="trunk"~65,
                                                       TRUE ~ 2)))


# for streets with bike lanes: lanes<=2&speed<25 ->LTS1; lanes<=4&speed<=35->LTS2; 
edges_cyc <- edge_spl %>% 
  filter(!is.na(cycleway)) %>% 
  mutate(LTS2 = case_when(lanes<=2&speedlim<25&oneway=="False" ~1,
                         lanes<=1&speedlim<25&oneway=="True" ~1,
                         lanes<=4&speedlim<=35&oneway=="False" ~2,
                         lanes<=2&speedlim<=35&oneway=="True" ~2,
                         (lanes>=5&speedlim<40)|(lanes<5&speedlim>=40) ~3,
                         lanes>=3&oneway=="True"&speedlim<=25 ~ 3,
                         lanes>=3&oneway=="True"&speedlim>25 ~ 4,
                         lanes>=5&speedlim>=40 ~4))
table(edges_cyc$LTS2)

edges_nocyc <- edge_spl %>% 
  filter(is.na(cycleway)) %>% 
  mutate(LTS2 = case_when(lanes<=3&speedlim<=25&oneway=="False" ~1,
                         lanes<=1.5&speedlim<=25&oneway=="True" ~1,
                         lanes<=2&speedlim<=25&oneway=="False"&highway!="residential" ~2,
                         lanes<=1&speedlim<=25&oneway=="True"&highway!="residential" ~2,
                         lanes<=3&speedlim==30&oneway=="False" ~2,
                         (lanes==2|lanes==3)&speedlim==30&oneway=="False"&highway=="residential" ~2,
                         (lanes==1|lanes==1.5)&speedlim==30&oneway=="True"&highway=="residential" ~2,
                         (lanes==2|lanes==3)&speedlim==30&oneway=="False"&highway!="residential" ~3,
                         (lanes==1|lanes==1.5)&speedlim==30&oneway=="True"&highway!="residential" ~3,
                         (lanes==4|lanes==5)&speedlim<=25&oneway=="False" ~3,
                         lanes==2&speedlim<=25&oneway=="True" ~3,
                         (lanes==4|lanes==5)&speedlim==30&oneway=="False" ~4,
                         lanes>=2&speedlim==30&oneway=="True" ~4,
                         lanes>=6 ~ 4,
                         lanes>=3&oneway=="True" ~ 4,
                         speedlim>=35 ~ 4))
table(edges_nocyc$LTS2)

a<- filter(edges_nocyc, is.na(LTS))

edge <- rbind(edges_cyc, edges_nocyc)

table(edge$LTS2)

st_write(edge, "src/data/portland_network_osm - Copy/edges/edge_2.shp")

# write.dbf(edge, "src/data/portland_network_osm - Copy/edges/edges.dbf")
