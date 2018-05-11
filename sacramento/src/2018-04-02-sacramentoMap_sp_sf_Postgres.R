# created by Philip Orlando
# Sustainable Atmospheres Research Lab
# 2018-03-26
# leaflet map for site selection in sacrauerque

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
       ,sp
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
       ,htmlwidgets
)

# geography projection
wgs_84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Sacramento, CA, UTM 10S, meters
epsg_26911 <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# grab our census key in the root directory
KEY <- scan("census_key.pgpss", what = "")
census_api_key(KEY, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# reading in census data
acs <- get_acs(year = "2016"
               ,geography = "block group" 
               ,variables = "B00001_001E" # unweighted total population
               ,state = "CA"
               ,county = c("Sacramento"
                           ,"El Dorado"
                           ,"Nevada"
                           ,"Placer"
                           ,"Sutter"
                           ,"Yolo"
                           ,"Yuba"
                           #,"Douglas" # Nevada
                           ) # counties taken from wiki albuq metro area page
               ,key = KEY
               ,output = "wide"
               ,geometry = TRUE
)

# reproject into meters
acs <- st_transform(acs, epsg_26911)

# reading in shapefiles for the entire US
urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")

# filtering out only Sacramento, CA shapefiles
sacra <- subset(urban_areas, str_detect(NAME10, "Sacramento, CA"))
#wood <- subset(urban_areas, str_detect(NAME10, "Woodland, CA"))

# reprojecting to meters
sacra <- spTransform(sacra, CRSobj = CRS(epsg_26911))

# determine the bounding box of the city boundary
bb <- bbox(sacra)

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
                               proj4string = CRS(proj4string(sacra)))


# convert from SpatialGridDataFrame to polygons for gIntersection() with city boundary
grid_poly <- as(sp_grd, "SpatialPolygonsDataFrame")

# clipping grid by the city boundary
grid <- grid_poly[sacra, ]

# convert to sf object
grid <- st_as_sf(grid)
sacra <- st_as_sf(sacra)


# create any empty dataframe
output_names <- c("grid_cell_id", "bg_name", "weighted_pop", "bg_pop", "relative_area", "bg_area", "cell_area")
output_df <- data.frame(matrix(ncol = length(output_names), nrow = 0))
colnames(output_df) <- output_names

# iterate through each grid cell to get the area-weighted population from the block group data
for(i in 1:nrow(grid)) {
  
  # capture each row of our grid dataframe
  cell <- grid[i, ]
  
  # store cell id variable
  cell_id <- cell$id
  
  # determine the grid cell area (4 km^2)
  cell_area <- as.numeric(st_area(cell$geometry))
  
  # iterate through each block group
  for(j in 1:nrow(acs)) {
    
    # capture each row of our block group data
    bg <- acs[j, ]
    
    # determine if a block group intersects with a grid cell
    overlap <- st_intersects(cell$geometry, bg$geometry)[[1]]
    
    # calculate relative 
    if(!is_empty(overlap)) {
      
      bg_area <- as.numeric(st_area(bg$geometry))
      relative_area <- as.numeric(st_area(st_intersection(cell$geometry, bg$geometry)))
      bg_pop <- bg$B00001_001E
      weighted_pop <- bg_pop*(relative_area/bg_area)
      bg_name <- bg$NAME
      
      # print each row to console
      print(paste(cell_id, bg_name, weighted_pop, bg_pop))
      
      # store each row in our output dataframe
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


map <- leaflet() %>%
  
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

  # addLegend(position = c("bottomright")
  #           ,colors = plotclr
  #           ,labels = c(as.character(class$brks))
  #           ,opacity = 0.90
  #           ,title = "Population Density"
  #           ) %>%
  
addScaleBar()
# addLabelOnlyMarkers(lng = ~ x, lat = ~ y, data = crd
#                     , label = as.character(p@data$id)
#                     , labelOptions = labelOptions(noHide = TRUE
#                                                   , direction = 'top'
#                                                   , textOnly = TRUE))

map

