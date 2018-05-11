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
       ,ggplot2
       ,plyr
       ,dplyr
       ,tidyr
       ,stringr
       ,magrittr
       ,rgeos
       ,rgdal
       #,sp
       ,leaflet
       ,sf
       ,raster
       ,mapview
)

devtools::install_github("tidyverse/ggplot2")
require(ggplot2)

# reading in shapefiles for the entire US
#urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")
urban_areas <- st_read(dsn = "./data/tigerline/tl_2017_us_uac10.shp") 

# pulling out only Albuquerque, NM shapefiles
#albuq <- subset(urban_areas, str_detect(NAME10, "Albuquerque, NM"))
#sacra <- subset(urban_areas, str_detect(NAME10, "Sacramento, CA"))

albuq <- urban_areas %>% filter(NAME10 == "Albuquerque, NM") %>%
  st_transform(6528)

#proj4string(albuq)

#epsg_6528 <- "+proj=tmerc +lat_0=31 +lon_0=-106.25 +k=0.9999 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs "

# transform to a projection in meters
albuq <- spTransform(albuq, CRSobj = CRS(epsg_6528))
proj4string(albuq)

grid_2km <- st_make_grid(albuq, cellsize = c(2000, 2000)) %>%
  st_sf(grid_id = 1:length(.))


# determine the bounding box of the city boundary
bbox(albuq)
e <- extent(bbox(albuq))

bb <- bbox(albuq)
cs <- c(2000, 2000) # cell size is 2km x 2km

cc <- bb[,1] + (cs/2) # cell offset # huh?!
cd <- ceiling(diff(t(bb))/cs) # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)



sp_grd <- SpatialGridDataFrame(grd, 
                               data=data.frame(id=1:prod(cd)),
                               proj4string = CRS(proj4string(albuq)))
summary(sp_grd)
  


over(albuq, sp_grd)

spplot(sp_grd, "id",
       panel = function(...) {
         panel.gridplot(..., border = "black", col.regions="white")
         sp.polygons(albuq)
         panel.text(...)
       })



################################################

bbox(sacra)
sac_e <- extent(bbox(sacra))


# convert to a raster object
r <- raster(e)
sac_r <- raster(sac_e)

# divide this raster into a 10 by 10 grid
dim(r) <- c(10, 10) ## spatially explicit cell sizes are ideal, but this works for now... 
projection(r) <- crs(proj4string(albuq))

dim(sac_r) <- c(10, 10)
projection(sac_r) <- crs(proj4string(sacra))


# add label ID to our grid cells
r <- setValues(r, 1:ncell(r))
sac_r <- setValues(sac_r, 1:ncell(sac_r))

# reconvert back to shapefile with the goal of creating a popup of the cell ID for each polygon
shape <- rasterToPolygons(r, dissolve = TRUE)
sac_py <- rasterToPolygons(sac_r, dissolve = TRUE)

# clip the grid cells that contain the Albuquerque polygon
p <- shape[albuq, ]
sac_p <- sac_py[sacra, ]

# trim the grid perimeter to match the Albuquerque polygon
#map <- gIntersection(p, albuq, byid = TRUE, drop_lower_td = TRUE)
map <- gIntersection(shape, albuq, byid = TRUE, drop_lower_td = TRUE)


map_sac <- gIntersection(sac_p, sacra, byid = TRUE, drop_lower_td = TRUE)


spy = SpatialPolygonsDataFrame(map
                               , data = data.frame(ID = p@data$layer)
                               , match.ID = FALSE)


spy_sac = SpatialPolygonsDataFrame(map_sac
                               , data = data.frame(ID = sac_p@data$layer)
                               , match.ID = FALSE)


## display data
m1 = mapview(spy
             , map.types = "OpenStreetMap" # see mapviewGetOption("basemaps")
             , col.regions = "transparent"
             , alpha.regions = .05)
m1

## find centroid coordinates
cnt = rgeos::gCentroid(spy, byid = TRUE)
crd = data.frame(coordinates(cnt))



## add text labels
m2 = m1@map %>%
  addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd
                      , label = as.character(p@data$layer)
                      , labelOptions = labelOptions(noHide = TRUE
                                                    , direction = 'top'
                                                    , textOnly = TRUE)) 
m2


m3 = mapview(spy_sac
             , map.types = "OpenStreetMap"
             , col.regions = "transparent"
             , alpha.regions = 0.05)

m3


cnt_sac = rgeos::gCentroid(spy_sac, byid = TRUE)
crd_sac = data.frame(coordinates(cnt_sac))

m4 = m3@map %>%
  addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd_sac
                      , label = as.character(sac_p@data$layer)
                      , labelOptions = labelOptions(noHide = TRUE
                                                    , direction = 'top'
                                                    , textOnly = TRUE)) 
m4
