library(pacman)
p_load(foreign,dplyr)

# merge OSM highway and cycleway tag
edges <- read.dbf("src/data/portland_network_osm - Copy/edges/edges.dbf")

edges1 <- edges %>% 
  mutate(cycleway = ifelse(cycleway=="track","cycletrack",as.character(cycleway))) %>% 
  mutate(cycleway = ifelse(is.na(cycleway)&(highway=="path"|highway=="pedestrian"|highway=="cycleway"), 
                           as.character(highway), as.character(cycleway))) 
table(edges1$cycleway)
table(edges1$service)

write.dbf(edges1, "src/data/portland_network_osm - Copy/edges/edges.dbf")
