library(pacman)
p_load(sf)

metro <- read.csv("Data/metro_ohas_2011.csv")
pdx_b <- st_read("src/data/pdx_boundary.shp")

plot(pdx_b)
points(metro$ThisXCORD , metro$ThisYCORD, type="p", col="red", cex=1)
