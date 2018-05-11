# created by Philip Orlando
# Sustainable Atmospheres Research Lab
# 2018-03-23
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
       ,tidycensus
       ,tidyverse
       ,RPostgres
)

# geog575 db
host <- "learn-pgsql.rc.pdx.edu"
db <- "porlando"
user <- "porlando"
pw <- scan("./batteries.pgpss", what = "")

con <- dbConnect(drv = RPostgres::Postgres()
                 ,dbname = db
                 ,host = host
                 ,port = 5432
                 ,password = pw
                 ,user = user)


#dbListTables(conn = con)

# grab our census key in the root directory
KEY <- scan("census_key.pgpss", what = "")
census_api_key(KEY, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# reading in census data
albuq_acs <- get_acs(year = "2016"
                     ,geography = "block group" 
                     ,variables = "B00001_001E" # unweighted total population
                     ,state = "NM"
                     ,county = "Bernalillo"
                     ,key = KEY
                     ,output = "wide"
                     ,geometry = TRUE
                     )

# In acse get_acs() isn't working...
write.csv(albuq_acs, "./data/acs/albuq_acs.csv")

mapview(albuq_acs["B00001_001E"], col.regions = sf.colors(10))

# convert from sf to sp class
albuq_sp <- as(albuq_acs, "Spatial")
class(albuq_sp)
summary(albuq_sp)
head(albuq_sp@data$B00001_001E)

#mapview(albuq_sp)
albuq_sp <- spTransform(albuq_sp, CRSobj = CRS(epsg_26913))

# upgrade R to 3.4.2 for get_acs() to run!
# web map
wgs_84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Albuquerque, NM, UTM 13N, meters
epsg_26913 <- "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Sacramento, CA, 

# reading in shapefiles for the entire US
urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")

# pulling out only Albuquerque, NM shapefiles
albuq <- subset(urban_areas, str_detect(NAME10, "Albuquerque, NM"))
sacra <- subset(urban_areas, str_detect(NAME10, "Sacramento, CA"))

# spatial reference system proj4

# project geography to geometry in meters
albuq <- spTransform(albuq, CRSobj = CRS(epsg_26913))

# determine the bounding box of the city boundary
bb <- bbox(albuq)

# specify the desired cell size
cs <- c(2000, 2000) # 2km x 2km

# cell offset and starting point (bottom right corner?)
cc <- bb[, 1] + (cs/2) # cell offset, centering bottom right corner at the halfway point

# number of cells per direction
cd <- ceiling(diff(t(bb))/cs)

# convert to GridTopology
grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
grd

# convert to spatial grid dataframe
sp_grd <- SpatialGridDataFrame(grd, data = data.frame(id=1:prod(cd)),
                               proj4string = CRS(proj4string(albuq)))




summary(sp_grd)
class(sp_grd)

# convert from SpatialGridDataFrame to polygons for gIntersection() with city boundary
grid_poly <- as(sp_grd, "SpatialPolygonsDataFrame")


# project back to geography
grid_poly <- spTransform(grid_poly, CRSobj = CRS(wgs_84))
albuq <- spTransform(albuq, CRSobj = CRS(wgs_84))

# clip the grid cells that contain the Albuquerque polygon
p <- grid_poly[albuq, ] # necessary for labels

# reproject to meters for areal interpolation
grid_sp <- spTransform(p, CRSobj = CRS(epsg_26913))
albuq <- spTransform(albuq, CRSobj = CRS(epsg_26913))

# convert all sp to sf class to write to db
grid_sf <- st_as_sf(grid_sp)
albuq_sf <- st_as_sf(albuq)
acs_sf <- st_as_sf(albuq_sp)

# 

# intersect p grid cells with acs data
test <- gIntersection(p, albuq_sp)
test_map <- mapview(test)
test_map


for(cell in p@data$id) {
  print(cell)
  for(bg in albuq_sp@data$GEOID) {
    print(bg)
    #gIntersects(cell, bg)
  }
}


# intersect grid and city polygons
map <- gIntersection(grid_poly, albuq, byid = TRUE, drop_lower_td = TRUE)

# convert into spatial polygons dataframe to include the p@data$id layer
spy = SpatialPolygonsDataFrame(map
                               , data = data.frame(ID = p@data$id)
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


mapview(grid, color~pop)

## add text labels
m2 = m1@map %>%
  
addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd
                    , label = as.character(p@data$id)
                    , labelOptions = labelOptions(noHide = TRUE
                                                  , direction = 'top'
                                                  , textOnly = TRUE))
m2




