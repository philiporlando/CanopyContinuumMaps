# created by Philip Orlando
# Sustainable Atmospheres Research Lab
# 2018-03-08
# leaflet map for site selection in albuquerque

# load the necessary packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

p_load(readr
       #,ggplot2
       ,plyr
       ,dplyr
       #,rlang
       ,tibble
       #,digest
       #,tidyr
       ,stringr
       ,magrittr
       ,rgeos
       ,rgdal
       #,sp
       ,leaflet
       ,sf
       ,raster
       #,mapview
       ,devtools
)


devtools::install_github("tidyverse/ggplot2") # geom_sf compatible
require(ggplot2) 

# reading in shapefiles for the entire US
#urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")
urban_areas <- st_read(dsn = "./data/tigerline/tl_2017_us_uac10.shp") 

# pulling out only Albuquerque, NM shapefiles
#albuq <- subset(urban_areas, str_detect(NAME10, "Albuquerque, NM"))
#sacra <- subset(urban_areas, str_detect(NAME10, "Sacramento, CA"))

albuq <- urban_areas %>% filter(NAME10 == "Albuquerque, NM") %>%
  st_transform(6528)

grid_2km <- st_make_grid(albuq, cellsize = c(2000, 2000)) %>%
  st_sf(grid_id = 1:length(.))

grid_lab <- st_centroid(grid_2km) %>% cbind(st_coordinates(.))

ggplot() + 
  geom_sf(data = albuq, fill = "white", lwd = 0.5) + 
  geom_sf(data = grid_2km, fill = 'transparent', lwd = 0.3) + 
  geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) + 
  coords_sf(datum = NA) +
  labs(x = "") +
  labes(y = "")
