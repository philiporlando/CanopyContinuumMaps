# created by Philip Orlando
# Sustainable Atmospheres Research Lab
# 2018-03-26
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
       ,RColorBrewer
       ,classInt
       ,htmltools
       ,scales
)

# geography projection
wgs_84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Albuquerque, NM, UTM 13N, meters
epsg_26913 <- "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "


# connect to our geog575 database
host <- "learn-pgsql.rc.pdx.edu"
db <- "porlando"
user <- "porlando"
#pw <- scan("./batteries.pgpss", what = "") # stores password in environment...

con <- dbConnect(drv = RPostgres::Postgres()
                 ,dbname = db
                 ,host = host
                 ,port = 5432
                 ,password = scan("./batteries.pgpss", what = "") # doesn't store password in environment
                 ,user = user)

# test the connection
#dbListTables(conn = con)

# grab our census key in the root directory
KEY <- scan("census_key.pgpss", what = "")
census_api_key(KEY, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# reading in census data
acs <- get_acs(year = "2016"
                     ,geography = "block group" 
                     ,variables = "B00001_001E" # unweighted total population
                     ,state = "NM"
                     ,county = c("Bernalillo"
                                 ,"Sandoval"
                                 ,"Torrance"
                                 ,"Valencia") # counties taken from wiki albuq metro area page
                     ,key = KEY
                     ,output = "wide"
                     ,geometry = TRUE
)


acs <- st_transform(acs, epsg_26913)

# figure out a better way to extract the CRS object from sf class
summary(acs$geometry)

# reading in shapefiles for the entire US
urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")

# pulling out only Albuquerque, NM shapefiles
albuq <- subset(urban_areas, str_detect(NAME10, "Albuquerque, NM"))

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

grid <- grid_poly[albuq, ]

grid <- st_as_sf(grid)
albuq <- st_as_sf(albuq)

#asdf <- data.frame("asdf")
#area <- st_area(st_intersection(acs, grid))

# create any empty dataframe
output_names <- c("grid_cell_id", "bg_name", "weighted_pop", "bg_pop", "relative_area", "bg_area", "cell_area")
output_df <- data.frame(matrix(ncol = length(output_names), nrow = 0))
colnames(output_df) <- output_names

# iterate through each grid cell to get the area-weighted population from bg data
for(i in 1:nrow(grid)) {
  
  # capture each row of our grid dataframe
  cell <- grid[i, ]
  
  #print(cell$id)
  cell_id <- cell$id
  cell_area <- as.numeric(st_area(cell$geometry))
  
  # iterate through each block group
  for(j in 1:nrow(acs)) {

    # capture each row of our bg data
    bg <- acs[j, ]
    
    overlap <- st_intersects(cell$geometry, bg$geometry)[[1]]

    if(!is_empty(overlap)) {
      
      bg_area <- as.numeric(st_area(bg$geometry))
      relative_area <- as.numeric(st_area(st_intersection(cell$geometry, bg$geometry)))
      bg_pop <- bg$B00001_001E
      weighted_pop <- bg_pop*(relative_area/bg_area)
      bg_name <- bg$NAME
      
      #bg_cell <- st_intersects(cell$geometry, bg$geometry)
      print(paste(cell_id, bg_name, weighted_pop, bg_pop))
      
      output_df = rbind(output_df
                        ,data.frame(grid_cell_id = cell_id
                                    ,bg_name = bg_name
                                    ,weighted_pop = weighted_pop
                                    ,bg_pop = bg_pop
                                    ,relative_area = relative_area
                                    ,bg_area = bg_area
                                    ,cell_area = cell_area
                        )
                        ,stringsAsFactors=FALSE)
      
    } 
    
    
  }
  
}

# sum each grid cell's weighted pop for each intersecting bg
grid_pop <- output_df %>% 
  group_by(grid_cell_id) %>%
  summarise(population = sum(weighted_pop))

# append pop data back to the original grid data
grid <- left_join(grid, grid_pop, by = c("id" = "grid_cell_id"))


# convert to sp class for leaflet?
grid_sp <- as(grid, "Spatial")
projection(grid_sp)
grid_sp <- spTransform(grid_sp, CRSobj = CRS(wgs_84))

spy = SpatialPolygonsDataFrame(grid_sp
                               ,data = data.frame(ID = grid_sp@data$id)
                               ,match.ID = TRUE
                               #,fillColor = colcode
                               )

m1 = mapview(spy
             ,map.types = "OpenStreetMap"
             ,col.regions = "transparent"
             ,alpha.regions = 0.05
             ,zcol = "Population"
             )
m1



# color code
levels <- 10
plotclr <- brewer.pal(levels, "RdYlGn")
class <- classIntervals(grid_sp@data$population, n = levels, style = "quantile")
colcode <- findColours(class, rev(plotclr))

# popup
# pop1 <- paste0("<b>ID:</b> "
#               ,grid_sp@data$id
#               ,"<br /> <b>Population:</b> "
#               ,round(grid_sp@data$population, 2))


labels <- sprintf(
  "<strong>ID: %s</strong><br/> Population Density: %s 2 km<sup>-2</sup>"
  ,grid_sp@data$id, comma(round(grid_sp@data$population, 0))
) %>% 
  lapply(HTML)



m2 = leaflet() %>%
  
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%

  addPolygons(data = grid_sp
              ,color = "black"
              ,fillColor = colcode
              ,fillOpacity = 0.20
              ,weight = 0.5
              #,popup = pop1
              # interaction
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                ,dashArray = ""
                ,fillOpacity = 0.7
                ,bringToFront = TRUE
                )
              ,label = labels
              ,labelOptions = labelOptions(
                style = list("font-weight" = "normal"
                             , padding = "3px 8px"
                             )
                ,textsize = "15px"
                ,direction = "auto"
                )
              ) %>%
  addScaleBar() %>%
  addLegend(position = c("bottomright"))
  
  # addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd
  #                     , label = as.character(p@data$id)
  #                     , labelOptions = labelOptions(noHide = TRUE
  #                                                   , direction = 'top'
  #                                                   , textOnly = TRUE))
  
m2


# spy2 = SpatialPolygonsDataFrame(grid_sp
#                                ,data = data.frame(ID = grid_sp@data$id)
#                                ,match.ID = TRUE
# )



## find centroid coordinates
cnt = rgeos::gCentroid(spy, byid = TRUE)
crd = data.frame(coordinates(cnt))

## add text labels
# m2 = m1@map %>%
#   
#   addPolygons(data = grid_sp
#               ,color = "black"
#               ,fillColor = colcode
#               ,fillOpacity = 0.20
#               ,weight = 0.5
#               ,popup = pop1
#   ) %>%
#   addScaleBar() %>%
#   
#   
#   addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd
#                       , label = as.character(grid_sp@data$id)
#                       , labelOptions = labelOptions(noHide = TRUE
#                                                     , direction = 'top'
#                                                     , textOnly = TRUE))
# 
# m2


