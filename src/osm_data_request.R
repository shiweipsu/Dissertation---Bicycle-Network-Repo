if(!require("pacman")){install.packages("pacman");library(pacman)}
p_load(osmdata,sf, dplyr)

pdx<- getbb("Portland or, us") %>% 
  opq() %>% 
  #add_osm_feature (key = 'highway', value = 'cycleway') %>%
  add_osm_feature (key = 'cycleway') %>%
  add_osm_feature (key = 'name') #%>%
  #osmdata_sf () %>% 
  #trim_osmdata(getbb("Portland or, us", format_out="polygon"))

pdx_sf <- osmdata_sf(pdx)
unlist (lapply (pdx_sf, nrow) [4:8])

line <- pdx_sf$osm_lines
table(line$maxspeed) # speed limit
table(line$lanes) # number of lanes, need further confirm

pdx_sp <- osmdata_sp(pdx)
sp::plot(pdx_sp$osm_lines)
