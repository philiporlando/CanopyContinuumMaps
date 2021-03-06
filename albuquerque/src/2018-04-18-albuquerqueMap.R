# created by Philip Orlando
# Sustainable Atmospheres Research Lab
# 2018-04-18
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
       #,rPython
)



# geography projection
wgs_84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

# Albuquerque, NM, UTM 13N, meters
epsg_26913 <- "+proj=utm +zone=13 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

## depreciated read function!
## since each file has the same header, we only need to create one read function for this!
# read.airnow.daily <- function(fpath) {
#   x <- read_csv(fpath, 
#                 col_types = cols(`1st Max Hour` = col_number(), 
#                                  `1st Max Value` = col_number(), `Arithmetic Mean` = col_number(), 
#                                  `Date of Last Change` = col_date(format = "%Y-%m-%d"), 
#                                  Latitude = col_number(), Longitude = col_number(), 
#                                  `Method Code` = col_character(), 
#                                  `Observation Count` = col_character(), 
#                                  `Observation Percent` = col_character(), 
#                                  POC = col_character(), `Parameter Code` = col_character()))
#   
#   x$month <- format(x$`Date Local`, "%m")
#   
#   ## create a season variable based on the extraced month:
#   x$season <- ifelse(x$month < "03" & x$month >= "01" | x$month == "12", "Winter",
#                      ifelse(x$month >= "03" & x$month < "06", "Spring",
#                             ifelse(x$month >= "06" & x$month < "09", "Summer",
#                                    ifelse(x$month >= "09" & x$month < "12", "Autumn",
#                                           "Other"))))
#   return(x)
# }


# read function for epa airnow data (manually downloaded from their map tool)
read.airnow.daily <- function(fpath) {
  x <- read.csv(fpath)
  return(x)
}

# provide path to airnow directory
pm25.path.daily <- "./data/airnow/"

# list files from this directory
pm25_files_daily <- list.files(path = pm25.path.daily, pattern = "\\.csv$",
                               all.files=FALSE, full.names = TRUE,
                               ignore.case = FALSE)

# read in all the airnow files
pm25_master_daily <- ldply(pm25_files_daily, read.airnow.daily)

# subset our state of interest
albuq_airnow <- subset(pm25_master_daily, State.Name == "New Mexico")

# retrieve only the rows with distinct site numbers
albuq_sites <- albuq_airnow %>% distinct(Site.Number, .keep_all = TRUE)

# create coordinates
albuq_coords <- albuq_sites[, c(7,6)] # lon, lat

# convert to sp object
albuq_points <- SpatialPointsDataFrame(coords = albuq_coords
                                       ,data = albuq_sites
                                       ,proj4string = CRS(wgs_84)
                                       )


# call our python webscrape function to build a list of online sensors
system('python purpleair_webscrape.py', wait = TRUE) # waits until all data is scraped

# provide path to existing purpleair directory
other_pa_path <- ("./data/existing_purple_air/")

# list files within this directory
other_pa_files <- list.files(path = other_pa_path, pattern = "\\.txt$",
                             all.files=FALSE, full.names = TRUE,
                             ignore.case = FALSE)

# read in all the 3rd party purpleair data
other_pa <- ldply(other_pa_files, read_csv) %>% na.omit()

# create coordinates
other_pa_coords <- other_pa[, c(5,4)] # lon then lat (x, y)

# create sp object
other_pa_points <- SpatialPointsDataFrame(coords = other_pa_coords
                                          ,data = other_pa
                                          ,proj4string = CRS(wgs_84)
                                          )

# convert to sf object
pa_points <- st_as_sf(other_pa_points)



# grab our census key in the root directory
KEY <- scan("census_key.pgpss"
            ,what = ""
)

census_api_key(KEY
               ,install = TRUE
               ,overwrite = TRUE
)

readRenviron("~/.Renviron")

# reading in census data
acs <- get_acs(year = 2016
               ,geography = "block group" 
               ,variables = "B00001_001E" # unweighted total population
               ,state = "NM"
               ,county = c("Bernalillo"
                           ,"Sandoval"
                           ,"Torrance"
                           ,"Valencia"
               ) # counties taken from wiki albuq metro area page
               ,key = KEY
               ,output = "wide"
               ,geometry = TRUE
)

# reproject into planar coordinates with meters as units
acs <- st_transform(acs
                    ,epsg_26913
)

# reading in shapefiles for the entire US
urban_areas <- readOGR(dsn = "./data/tigerline/tl_2017_us_uac10.shp")

# pulling out only Albuquerque, NM shapefiles
albuq <- subset(urban_areas # not metropolitan statistical areas...
                ,str_detect(NAME10
                            ,"Albuquerque, NM"
                )
)

# reproject in plane with meters
albuq <- spTransform(albuq
                     ,CRSobj = CRS(epsg_26913)
)

# determine the bounding box of the city boundary
bb <- bbox(albuq)

# specify the desired cell size
cs <- c(2000, 2000) # 2km x 2km

# cell offset and starting point (bottom right corner?)
cc <- bb[, 1] + (cs/2) # cell offset, centering bottom right corner at the halfway point

# number of cells per direction
cd <- ceiling(diff(t(bb))/cs)

# convert to GridTopology
grd <- GridTopology(cellcentre.offset = cc
                    ,cellsize = cs
                    ,cells.dim = cd
)
grd

# convert to spatial grid dataframe
sp_grd <- SpatialGridDataFrame(grd
                               ,data = data.frame(id=1:prod(cd)
                               )
                               ,proj4string = CRS(proj4string(albuq)
                               )
)




summary(sp_grd)
class(sp_grd)

# convert from SpatialGridDataFrame to polygons for gIntersection() with city boundary
grid_poly <- as(sp_grd
                ,"SpatialPolygonsDataFrame"
)

# clip only the grid cells that intersect with our urban boundary
grid <- grid_poly[albuq, ]

# reconvert to sf object
grid <- st_as_sf(grid)
albuq <- st_as_sf(albuq)


# find the existing purpleair sensors that intersect with our urban area
pa_points <- st_transform(pa_points, crs = st_crs(albuq))

# figure out how to use regex to exclude b sensors...
pa_points <- pa_points[albuq, ] %>% distinct(geometry, .keep_all = TRUE) # can't get rid of the B sensors yet...
pa_points 

# convert back to wgs84 for leaflet map
pa_points <- st_transform(pa_points, crs = st_crs(wgs_84))


# create any empty dataframe to store output from our for loop below
output_names <- c("grid_cell_id"
                  ,"bg_name"
                  ,"weighted_pop"
                  ,"bg_pop"
                  ,"relative_area"
                  ,"bg_area"
                  ,"cell_area"
)
output_df <- data.frame(matrix(ncol = length(output_names)
                               ,nrow = 0
)
)
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
    
    overlap <- st_intersects(cell$geometry,bg$geometry)[[1]]
    
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

grid_pop$population <- grid_pop$population/2 # convert to km2 instead of 2km2


# append pop data back to the original grid data
grid <- left_join(grid, grid_pop, by = c("id" = "grid_cell_id"))


# convert to sp class for leaflet?
grid_sp <- as(grid, "Spatial")
projection(grid_sp)
grid_sp <- spTransform(grid_sp
                       ,CRSobj = CRS(wgs_84)
)

# color code
levels <- 10
plotclr <- brewer.pal(levels
                      ,"RdYlGn"
)

class <- classIntervals(grid_sp@data$population
                        ,n = levels
                        ,style = "quantile"
)

colcode <- findColours(class
                       ,rev(plotclr)
)

# popup replaced with hover-on labels
# popup
# pop1 <- paste0("<b>ID:</b> "
#               ,grid_sp@data$id
#               ,"<br /> <b>Population:</b> "
#               ,round(grid_sp@data$population, 2))



# display hover on grid cell id and population
labels <- sprintf(
  "<strong>ID: %s</strong><br/> Population Density: %s km<sup>-2</sup>"
  ,grid_sp@data$id
  ,comma(round(grid_sp@data$population,0)
  )
) %>% 
  lapply(HTML)


# creating our first web map of the population density grid cells
m1 = leaflet() %>%
  
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
  addScaleBar() #%>%

m1

# # determine population density for each decile
# deciles <- quantile(grid_sp@data$population
#                     ,c(0.10
#                        ,0.20
#                        ,0.30
#                        ,0.40
#                        ,0.50
#                        ,0.60
#                        ,0.70
#                        ,0.80
#                        ,0.90
#                        )
#                     )

# create a list of each decile
deciles <- list(0.00
                ,0.10
                ,0.20
                ,0.30
                ,0.40
                ,0.50
                ,0.60
                ,0.70
                ,0.80
                ,0.90
                #,1.00
)

# create an empty output dataframe to capture the output of another for loop
n_deciles <- data.frame(matrix(ncol = 8, nrow = 0))
output_names <- c("decile"
                  ,"pop_den"
                  ,"n_grid"
                  ,"dec_pop"
                  ,"tot_pop"
                  ,"rel_pop"
                  ,"rel_n"
                  ,"rel_n_round"
)

colnames(n_deciles) <- output_names

# avoid R's rounding to nearest even number!!!
# round like a sane person with round2()!!!
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


# determine relative population per decile, and allocate sensors accordingly!
for(i in deciles) {
  
  tot_sensors <- 30
  
  lower_quantile <- round2(quantile(grid_sp@data$population
                             ,i
                             ,type = 5 # for deciles
                             )
                           ,0
  )
  
  upper_quantile <- round2(quantile(grid_sp@data$population
                             ,i+0.10
                             ,type = 5 # for deciles
                             )
                           ,0
  )
  
  sample <- subset(grid_sp@data
                   ,population >= lower_quantile & population < upper_quantile 
  )
  
  n_grid <- length(sample$id)              
  
  dec_pop <- round2(sum(sample$population), 0)
  tot_pop <- round2(sum(grid_sp@data$population), 0)
  rel_pop <- dec_pop/tot_pop
  
  rel_n <- rel_pop*tot_sensors
  
  rel_n_round <- round2(rel_n, 0)
  
  n_deciles <- rbind(n_deciles
                     ,data.frame(decile = i+0.10
                                 ,pop = upper_quantile
                                 ,n_grid = n_grid
                                 ,dec_pop = dec_pop
                                 ,tot_pop = tot_pop
                                 ,rel_pop = rel_pop
                                 ,rel_n = rel_n
                                 ,rel_n_round = rel_n_round
                     )
                     ,stringsAsFactors = FALSE
  )
  print(n_deciles)
  
}

n_deciles
sum(n_deciles$rel_n_round)

# store this output as a csv for later
write.csv(n_deciles, "./deciles.csv")

# doublecheck our deciles
dec <- quantile(grid_sp@data$population, prob = seq(0, 1, length = 11), type = 5)
quantile(grid_sp@data$population, prob = seq(0, 1, length = 5), type = 7)
summary(grid_sp@data$population)

# inspect population density distribution and cut points
ggplot(data = grid_sp@data, aes(population)) + 
  geom_histogram(
    #binwidth = length(population)
    binwidth = 10
    ) + 
  geom_vline(xintercept = dec, color = "firebrick") + 
  xlab("Population Density (km^-2)") + 
  ylab("Grid Cells (n)") + 
  theme_bw()

pal1 <- c("wheat1", "red3")

plot(class
     ,pal = pal1
     ,main = "Decile"
     ,xlab = "Population Density km^-2)"
     ,ylab = "Percentile"
     )

# associate each grid id to each decile (could of vectorized this probably...)
dec_10 <- subset(grid_sp@data
                 ,population <= n_deciles$pop[1]
                 )

dec_20 <- subset(grid_sp@data
                 ,population > n_deciles$pop[1] & population <= n_deciles$pop[2]
                 )

dec_30 <- subset(grid_sp@data
                 ,population > n_deciles$pop[2] & population <= n_deciles$pop[3]
                 )

dec_40 <- subset(grid_sp@data
                 ,population > n_deciles$pop[3] & population <= n_deciles$pop[4]
                 )


dec_50 <- subset(grid_sp@data
                 ,population > n_deciles$pop[4] & population <= n_deciles$pop[5]
)

dec_60 <- subset(grid_sp@data
                 ,population > n_deciles$pop[5] & population <= n_deciles$pop[6]
)

dec_70 <- subset(grid_sp@data
                 ,population > n_deciles$pop[6] & population <= n_deciles$pop[7]
)

dec_80 <- subset(grid_sp@data
                 ,population > n_deciles$pop[7] & population <= n_deciles$pop[8]
)

dec_90 <- subset(grid_sp@data
                 ,population > n_deciles$pop[8] & population <= n_deciles$pop[9]
)

dec_100 <- subset(grid_sp@data
                 ,population > n_deciles$pop[9] & population <= n_deciles$pop[10]
)

# set random seed for reproducibility
set.seed(8)

# randomly sample grid cells within each decile, where n is weighted by relative population
sample_10 <- sample(dec_10$id
                    ,size = n_deciles$rel_n_round[1]
                    ,replace = FALSE
                    )

sample_20 <- sample(dec_20$id
                    ,size = n_deciles$rel_n_round[2]
                    ,replace = FALSE
)

sample_30 <- sample(dec_30$id
                    ,size = n_deciles$rel_n_round[3]
                    ,replace = FALSE
)


sample_40 <- sample(dec_40$id
                    ,size = n_deciles$rel_n_round[4]
                    ,replace = FALSE
)


sample_50 <- sample(dec_50$id
                    ,size = n_deciles$rel_n_round[5]
                    ,replace = FALSE
)


sample_60 <- sample(dec_60$id
                    ,size = n_deciles$rel_n_round[6]
                    ,replace = FALSE
)


sample_70 <- sample(dec_70$id
                    ,size = n_deciles$rel_n_round[7]
                    ,replace = FALSE
)


sample_80 <- sample(dec_80$id
                    ,size = n_deciles$rel_n_round[8]
                    ,replace = FALSE
)


sample_90 <- sample(dec_90$id
                    ,size = n_deciles$rel_n_round[9]
                    ,replace = FALSE
)


sample_100 <- sample(dec_100$id
                    ,size = n_deciles$rel_n_round[10]
                    ,replace = FALSE
)

# append these results to a single vector
sample_id <- c(sample_10
                   ,sample_20
                   ,sample_30
                    ,sample_40
                    ,sample_50
                    ,sample_60
                    ,sample_70
                    ,sample_80
                    ,sample_90
                    ,sample_100
                    )

# filter our original grid data by the matching sample id
grid_sample <- grid_sp[grid_sp@data$id %in% sample_id, ]

# manually adjusting for adjacency (throwing our stratified random sampling out the window...)
sample_id_adj <- data.frame(sample_id) # need to convert to dataframe to use legacy code from AirAdvice days...
sample_id_adj$sample_id <- ifelse(sample_id_adj$sample_id == "142", "141"
                                  ,ifelse(sample_id_adj$sample_id == "177", "178"
                                           ,ifelse(sample_id_adj$sample_id == "252", "253"
                                                   ,ifelse(sample_id_adj$sample_id == "299", "316"
                                                           ,ifelse(sample_id_adj$sample_id == "303", "320"
                                                                   ,ifelse(sample_id_adj$sample_id == "328", "345"
                                                                           ,sample_id_adj$sample_id
                                                                           )
                                                                   )
                                                           )
                                                   )
                                           )
                                  
                                  ) 


# create a second layer of grid cells that are not adjacent (only by corners, not sides)
grid_sample2 <- grid_sp[grid_sp@data$id %in% sample_id_adj$sample_id, ]

#plot(grid_sample2)

# accessing four coordinates of the first grid cell of our polygons data from grid_sample
# head(grid_sample@polygons[[1]]@Polygons[[1]]@coords)

# #dynamically assign 70th percentile for a city's population distribution
# pop_density_threshold <- quantile(grid_sp@data$population
#                                   ,0.70
# ) # 30% most densely populated grid cells
# 
# # filter grid cells that are the top 30% most populated 
# high_pop_density <- subset(grid_sp@data
#                            ,population >= pop_density_threshold
# )
# 
# # filter grid cells that are below this threshold
# low_pop_density <- subset(grid_sp@data
#                           ,population < pop_density_threshold
# )
# 
# # set a random seed for reproducibility
# set.seed(8)
# 
# # take a pseudo-random sample of the highly populated grid cells, n = 18
# high_pop_sample <- sample(high_pop_density$id
#                           ,18
#                           ,replace = FALSE
# )
# 
# # take a pseudo-random sampe of every other grid cell, n = 12
# low_pop_sample <- sample(low_pop_density$id
#                          ,12
#                          ,replace = FALSE)

# # combine these samples into a single list of IDs
# sample_id <- append(high_pop_sample
#                     ,low_pop_sample
# )

# # filter polygons that are not in our sample_id list
# grid_sample <- grid_sp[grid_sp@data$id %in% sample_id, ]


# color code
levels <- 10
plotclr <- brewer.pal(levels, "RdYlGn")
class2 <- classIntervals(grid_sample@data$population
                         ,n = levels
                         ,style = "quantile"
)

colcode2 <- findColours(class2
                        ,rev(plotclr)
)


# population density and cell id label (target cells)
labels3 <- sprintf(
  "<strong>ID: %s</strong><br/> Population Density: %s km<sup>-2</sup>"
  ,grid_sample2@data$id
  ,comma(round(grid_sample2@data$population, 0)
  )
) %>% 
  lapply(HTML)


# labels for EPA AirData sites
labels_airnow <- sprintf(
  paste0("<strong>EPA AirData: </strong><br/>"
         ,albuq_points@data$Local.Site.Name
         )
) %>% 
  lapply(HTML)

# labels for existing purpleair sites
labels_pa <- sprintf(
  paste0("<strong>PurpleAir: </strong><br/>"
         ,pa_points$site_name
         )
) %>% 
  lapply(HTML)


# create a palette for addLegend()
legendCode <- colorBin(palette = rev(plotclr)
         ,domain = class$brks
         ,bins = round(class$brks, 2)
         ,pretty = FALSE # this will add extra bins if set to T
)


# popups were replaced by hover-on labels
#airnow_pop <- paste0("<b>Site Name:</b> ", albuq_points@data$Local.Site.Name)
#pa_pop <- paste0("<b>Site Name:</b> ", pa_points$site_name)

# create a new web map with the grid_sample layer!
m2 = leaflet() %>%
  
  addTiles(group = "OSM (default)") %>%
  
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png'
           ,group = "OSM (BlackAndWhite)") %>%
  
  # provide empty grid cells as a base layer
  # addPolygons(data = grid_sp
  #             ,color = "black"
  #             ,fillColor = "blue"
  #             ,fillOpacity = 0.00
  #             ,weight = 0.5
  #             ,group = "Total Grid Cells"
  #             ) %>%
  
  addPolygons(data = grid_sp
              ,color = "black"
              ,fillColor = colcode
              ,fillOpacity = 0.20
              ,weight = 0.5
              #,popup = pop1
              # interaction
              ,group = "Total Grid Cells"
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                ,dashArray = ""
                ,fillOpacity = 0.7
                #,bringToFront = TRUE
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

  
  # add grid cells from our sample id list
  addPolygons(data = grid_sample
              ,color = "black"
              ,fillColor = colcode2
              #,fillColor = NULL
              ,fillOpacity = 0.00001
              ,weight = 2.5
              ,opacity = 1
              #,popup = pop1
              # interaction
              ,group = "Target Grid Cells"
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                #,color = NULL
                ,dashArray = ""
                ,fillOpacity = 0.000001
                #,bringToFront = TRUE
              )
              ,label = labels2
              ,labelOptions = labelOptions(
                style = list("font-weight" = "normal"
                             ,padding = "3px 8px"
                )
                ,textsize = "15px"
                ,direction = "auto"
              )
  ) %>%
  
  # add grid cells from our sample id list
  addPolygons(data = grid_sample2
              ,color = "black"
              ,fillColor = colcode2
              #,fillColor = NULL
              ,fillOpacity = 0.00001
              ,weight = 2.5
              ,opacity = 1
              #,popup = pop1
              # interaction
              ,group = "Target Grid Cells Adjusted"
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                #,color = NULL
                ,dashArray = ""
                ,fillOpacity = 0.000001
                #,bringToFront = TRUE
              )
              ,label = labels3
              ,labelOptions = labelOptions(
                style = list("font-weight" = "normal"
                             ,padding = "3px 8px"
                )
                ,textsize = "15px"
                ,direction = "auto"
              )
  ) %>%
  
  
  # pull in regional monitoring sites
  addCircleMarkers(data = albuq_points
                   ,fillColor = "blue"
                   ,color = "blue"
                   ,weight = 1.5
                   ,fillOpacity = 0.5
                   #,popup = airnow_pop
                   ,group = "Regional Monitoring Sites"
                   ,label = labels_airnow
                   ,labelOptions = labelOptions(
                     style = list("font-weight" = "normal"
                                  , padding = "3px 8px"
                     )
                     ,textsize = "15px"
                     ,direction = "auto"
                   )
  ) %>%
  
  # pull in existing PurpleAir monitors
  addCircleMarkers(data = pa_points
                   ,fillColor = "purple"
                   ,fillOpacity = 0.25 # since there are two circles per geometry... (B sensor)
                   ,color = "purple"
                   ,weight = 1.5
                   #,popup = pa_pop
                   ,group = "Existing PurpleAir"
                   ,label = labels_pa
                   ,labelOptions = labelOptions(
                     style = list("font-weight" = "normal"
                                  , padding = "3px 8px"
                     )
                     ,textsize = "15px"
                     ,direction = "auto"
                   )
  ) %>%
  
  addLayersControl(
    baseGroups = c("OSM  (BlackAndWhite)"
                   ,"OSM (default)"
                   )
    ,overlayGroups = c("Total Grid Cells"
                       ,"Target Grid Cells"
                       ,"Target Grid Cells Adjusted"
                       ,"Regional Monitoring Sites"
                       ,"Existing PurpleAir"
                       )
    ,options = layersControlOptions(collapsed = TRUE)
    ,position = "topright"
  ) %>%
  
  # turns the unadjusted target grid cells off by default
  hideGroup("Target Grid Cells") %>%
  # add a scale bar
  addScaleBar(position = "bottomleft") %>%
  
  # 
  addLegend(pal = legendCode
            ,values = class$brks
            ,position = "bottomright"
            ,title = "<b>Population Density (km<sup>-2</sup>) </b>")

  
m2





## similar to previous map with just the adjusted grid cells and not the original random sample cells
m3 = leaflet() %>%
  
  addTiles(group = "OSM (default)") %>%
  
  #addProviderTiles(providers$CartoDB.Positron) %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png'
           ,group = "OSM (BlackAndWhite)") %>%
  
  # provide empty grid cells as a base layer
  # addPolygons(data = grid_sp
  #             ,color = "black"
  #             ,fillColor = "blue"
  #             ,fillOpacity = 0.00
  #             ,weight = 0.5
  #             ,group = "Total Grid Cells"
  #             ) %>%
  
  addPolygons(data = grid_sp
              ,color = "black"
              ,fillColor = colcode
              ,fillOpacity = 0.20
              ,weight = 0.5
              #,popup = pop1
              # interaction
              ,group = "Total Grid Cells"
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                ,dashArray = ""
                ,fillOpacity = 0.7
                #,bringToFront = TRUE
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
  
  
  # # add grid cells from our sample id list
  # addPolygons(data = grid_sample
  #             ,color = "black"
  #             ,fillColor = colcode2
  #             #,fillColor = NULL
  #             ,fillOpacity = 0.00001
  #             ,weight = 2.5
  #             ,opacity = 1
  #             #,popup = pop1
  #             # interaction
  #             ,group = "Target Grid Cells"
  #             ,highlight = highlightOptions(
  #               weight = 5
  #               ,color = "#666"
  #               #,color = NULL
  #               ,dashArray = ""
  #               ,fillOpacity = 0.000001
  #               #,bringToFront = TRUE
  #             )
  #             ,label = labels2
  #             ,labelOptions = labelOptions(
  #               style = list("font-weight" = "normal"
  #                            ,padding = "3px 8px"
  #               )
  #               ,textsize = "15px"
  #               ,direction = "auto"
  #             )
  # ) %>%
  
  # add grid cells from our sample id list
  addPolygons(data = grid_sample2
              ,color = "black"
              ,fillColor = colcode2
              #,fillColor = NULL
              ,fillOpacity = 0.00001
              ,weight = 2.5
              ,opacity = 1
              #,popup = pop1
              # interaction
              ,group = "Target Grid Cells"
              ,highlight = highlightOptions(
                weight = 5
                ,color = "#666"
                #,color = NULL
                ,dashArray = ""
                ,fillOpacity = 0.000001
                #,bringToFront = TRUE
              )
              ,label = labels3
              ,labelOptions = labelOptions(
                style = list("font-weight" = "normal"
                             ,padding = "3px 8px"
                )
                ,textsize = "15px"
                ,direction = "auto"
              )
  ) %>%
  
  
  # pull in regional monitoring sites
  addCircleMarkers(data = albuq_points
                   ,fillColor = "blue"
                   ,color = "blue"
                   ,weight = 1.5
                   ,fillOpacity = 0.5
                   #,popup = airnow_pop
                   #,bringToFront = TRUE
                   ,group = "Regional Monitoring Sites"
                   ,label = labels_airnow
                   ,labelOptions = labelOptions(
                     style = list("font-weight" = "normal"
                                  , padding = "3px 8px"
                     )
                     ,textsize = "15px"
                     ,direction = "auto"
                   )
  ) %>%
  
  # pull in existing PurpleAir monitors
  addCircleMarkers(data = pa_points
                   ,fillColor = "purple"
                   ,fillOpacity = 0.25 # since there are two circles per geometry... (B sensor)
                   ,color = "purple"
                   ,weight = 1.5
                   #,popup = pa_pop
                   #,bringToFront = TRUE
                   ,group = "Existing PurpleAir"
                   ,label = labels_pa
                   ,labelOptions = labelOptions(
                     style = list("font-weight" = "normal"
                                  , padding = "3px 8px"
                     )
                     ,textsize = "15px"
                     ,direction = "auto"
                   )
  ) %>%
  
  addLayersControl(
    baseGroups = c("OSM  (BlackAndWhite)"
                   ,"OSM (default)"
    )
    ,overlayGroups = c("Total Grid Cells"
                       #,"Target Grid Cells" # original random sample
                       ,"Target Grid Cells" # adjusted target cells renamed 
                       ,"Regional Monitoring Sites"
                       ,"Existing PurpleAir"
    )
    ,options = layersControlOptions(collapsed = FALSE)
    ,position = "topright"
  ) %>%
  
  # # turns the unadjusted target grid cells off by default
  # hideGroup("Target Grid Cells") %>%
  # # add a scale bar
  # addScaleBar(position = "bottomleft") %>%
  
  # 
  addLegend(pal = legendCode
            ,values = class$brks
            ,position = "bottomright"
            ,title = "<b>Population Density (km<sup>-2</sup>) </b>")


m3

