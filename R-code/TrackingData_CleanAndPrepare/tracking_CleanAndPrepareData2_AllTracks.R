## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Script summary ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## INPUT: This script brings in example raw tracking data from
"Yelkouan Shearwater (Courtesy: Life ARTINA project, BIOM, Croatia) "
"Other example datasets to be included"

## OUTPUT: Cleaned tracking data ready for track2KBA analysis
"Although currently only supports this for an inividual trip from an animal"

## Jono Handley, jonathan.m.handley@gmail.com / jonathan.handley@birdlife.org

## R version R version 4.2.2 (2022-10-31 ucrt) -- "Bird Hippie"

## Feb 2023

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"Had to install R version: R version 4.2.2 (2022-10-31 ucrt)"

## Options to install aniMotum package for animal track interpolation
## aniMotum: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14060
#install.packages('aniMotum', repos = c('https://ianjonsen.r-universe.dev', 'https://cloud.r-project.org'))
# may need to install aniMotum after downloading using: devtools::install_local(package.zip)
#install.packages('TMB', type = 'source')
library("aniMotum")
## sf package for spatial data analyses (i.e. vector files such as points, lines, polygons)
library(sf)
## Tidyverse for data manipulation
library(tidyverse)
## ggplot2 for plotting opionts
library(ggplot2)
## rnaturalearth package for basemaps in R
library(rnaturalearth)

## leaflet package for interactive maps in R
#install.packages("leaflet")
library(leaflet)
##
library(purrr)
library(furrr)
#install.packages("track2KBA")
library(track2KBA)
## for date time
library(lubridate)
## for stats
library(stats)
## speed filter
library(trip)
## linear interpolation
library(adehabitatLT)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in global map layer for general plotting purposes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Source a world map from the rnaturalearth R package
## see details of function to change the type of map you get
worldmap <- rnaturalearth::ne_download(scale = "large",
                                       type = "countries",
                                       category = "cultural",
                                       destdir = tempdir(),
                                       load = TRUE,
                                       returnclass = "sf")

plot(st_geometry(worldmap))
st_crs(worldmap)

#st_write(worldmap,
#         "C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Data_GlobalLayers\\World_Map_NaturalEarth\\WorldMap_NaturalEarth_ScaleLarge.shp",
#         delete_layer = T)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Specify projections / store needed CRS definitions as variables ----
## SEE: https://epsg.io/
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wgs84 <- st_crs("EPSG:4326")
## project croatia
htrs96 <- st_crs("EPSG:3765")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load in raw tracking data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"NOTE: The process of loading in raw tracking data will vary by device type and
individual user preferences for storing data. FOR THE TOOLKIT: We can ony provide
general guidance about storing data in preparation for analysis."


"BELOW outlines process of reading in data from Croatia IBA project. This code is
in here as a reminder. But it is hashed out as I will just load the data as Rdata file."

"Highlight text from section below and press Ctrl + Shift + C, to unhash text."
# ## find Yelkouan .pos files by searching the data input folder recursively:
# 
# yelkouan_pos_2019 <- list.files(path = "data-input/Shearwater tagging/Yelkouan 2019/",
#                                 pattern = ".pos",
#                                 recursive = TRUE,
#                                 include.dirs = TRUE,
#                                 full.names = TRUE)
# yelkouan_pos_2020 <- list.files(path = "data-input/Shearwater tagging/Yelkouan 2020/",
#                                 pattern = ".pos",
#                                 recursive = TRUE,
#                                 include.dirs = TRUE,
#                                 full.names = TRUE)
# yelkouan_pos_2021 <- list.files(path = "data-input/Shearwater tagging/Yelkouan 2021/",
#                                 pattern = ".pos",
#                                 recursive = TRUE,
#                                 include.dirs = TRUE,
#                                 full.names = TRUE)
# 
# colony_data <- read_excel(path = "data-input/MetaData_tagging_Artina.xlsx")
# colony_data_spat <- st_as_sf(colony_data, coords = c("lon_colony", "lat_colony"), crs = wgs84) %>% 
#   st_transform(crs = htrs96)
# colony_coords_htrs <- st_coordinates(colony_data_spat)
# colony_data$colony_X <- colony_coords_htrs[,1]
# colony_data$colony_Y <- colony_coords_htrs[,2]
# 
# 
# # reading the .pos files --------------------------------------------------
# 
# # function that fetches the data from individual files:
# 
# getposdata <- function(filelist, ...) {
#   posnames <- c("day",	"month",	"year",	"hour", "minute",	"second", 
#                 "satellites",	"latitude",	"longitude",	"altitude",	
#                 "time_offset",	"accuracy",	"voltage")
#   poscols <- "iiiiii_innnnnn__"
#   list_of_pos_tables <- lapply(filelist, FUN = read_csv, 
#                                col_types = poscols, 
#                                col_names = posnames,
#                                skip = 5)
#   for (i in 1:length(filelist)) {
#     # get id of tag from filename structure:
#     tag_id <- gsub(".*?/(Tag.*?) - (.*?)/.*", "\\1_\\2", filelist[i])
#     # get id of colony from filename structure:
#     list_of_pos_tables[[i]]$colony_code <- gsub(".*?/(Tag.*?) - (.*?)-.*", "\\2", filelist[i])
#     print(tag_id)
#     list_of_pos_tables[[i]]$bird_id <- paste0(list_of_pos_tables[[i]]$year, "_",
#                                               tag_id)
#   }
#   postable <- bind_rows(list_of_pos_tables) %>% 
#     mutate(dttm = ymd_hms(paste(year, month, day, hour, minute, second, sep = "-")))
#   return(postable)  
# }
# 
# 
# # yelkouan loading and preprocessing --------------------------------------
# 
# yelk2019 <- getposdata(yelkouan_pos_2019)
# yelk2020 <- getposdata(yelkouan_pos_2020)
# yelk2021 <- getposdata(yelkouan_pos_2021)
# 
# yelk <- bind_rows(yelk2019, yelk2020, yelk2021) %>% 
#   # filter(year != 21) %>% # misunderstanding, still a breeding bird
#   filter(latitude != 0) %>% 
#   filter(time_offset < 1000)

load("data-testing/tracking-data/Tracking_YESH_raw.Rdata")

"NOTE: These raw tracks have not had trips split. Nor have they had data near the 
colony filtered. It will be good to do this before applying the interpolation
methods available via the aniMotum R package.

CONSIDER: One option might be to use the aniMotum R package as a standalone speed 
filter of the data, then one could run the script on the somewhat pre-filtered data
i.e. erroneous locations removed for speed and erroneous locations removed from the
colony / source of tracking data start."

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get raw tracking data into similar format as Seabird Tracking Database ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## view the first two rows of data
"Consider: do we want a tibble or data.frame?"
head(yelk,2)
head(data.frame(yelk),2)

## select relevant columns of data to align with SBTD format
"Consider providing overview of different key fields. I.e. lessons from STBD intro docs."

df_sbtd <- yelk %>% dplyr::mutate(scientific_name = "Puffinus yelkouan",
                       common_name = "Yelkouan Shearwater",
                       colony_name = "Lastovo SPA",
                       device = "GPS",
                       age= "adult",
                       sex= "unknown",
                       breed_stage = "breeding",
                       breed_status = "breeding") %>% 
  dplyr::select(scientific_name,
                common_name,
                colony_name,
                device,
                bird_id,
                age,
                sex,
                breed_stage,
                breed_status,
                #"NOTE: Date / Time column from SBTD is actually two separate columns.Will need to consider appropriate action for Toolkit."
                dttm,
                latitude,
                longitude)


head(data.frame(df_sbtd),2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## view all data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## quick plot of all data for a quick overview
df_sbtd_plot <- st_as_sf(df_sbtd, coords = c("longitude", "latitude"), crs=4326) # 4326 = geographic WGS84
plot(st_geometry(df_sbtd_plot), 
     cex = 0.5, 
     pch = 1)

## number of datapoints
nrow(df_sbtd_plot)

## interactive plot
map.alldata <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = df_sbtd_plot,
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) 

map.alldata

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Add unique id and create sf object ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Review data
head(data.frame(df_sbtd),2)
unique.birds <- unique(df_sbtd$bird_id)

## add a simplified animal ID column
df_sbtd$bird_id_num <- as.numeric(factor(df_sbtd$bird_id, levels = unique(df_sbtd$bird_id)))

## create sf spatial object
df_sbtd_sf <- df_sbtd %>% 
  ## first create new columns of lon and lat again so you keep this information
  ## in tabular format.
  mutate(lon_device = longitude,
         lat_device = latitude) %>% 
  ## then conver object to sf spatial object
  st_as_sf(coords = c("longitude", "latitude"), crs = wgs84)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save raw tracking data as shapefile for viewing in GIS software ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## create new folder within current working directory where you will save data
## first create the name of the species and the file path you need
## use gsub to replace spaces within character strings (words)
species_name <- gsub(" ", "-", df_sbtd$scientific_name[1]) 
## then create the new folder within current working directory
path_to_folder <- paste("./data-testing/tracking-data/",
                        species_name,
                        sep="")

## Check if folder exists, and if it does not, then make a new folder
if (!file.exists(path_to_folder)) {
  # If it does not exist, create a new folder
  dir.create(path_to_folder)
  print(paste("Created folder:", path_to_folder))
} else {
  # do nothing, but let us know the folder exists already
  print(paste("Folder already exists:", path_to_folder))
}

## write the spatial data as a shapefile
## CAUTION: For large files, this operation can take a significant amount of time
## NOTE: For some GIS software, column names will be abbreviated upon saving
## NOTE: If you have very long file paths, this operation may fail. One solution
## is to save the shapefile elsewhere. Another solution is to instead save the file
## as a geopackage (.gpkg): simply replace the .shp text below with .gpkg
st_write(df_sbtd_sf, paste(path_to_folder, "/",
                           species_name,
                           "_AllTracks.shp", 
                           sep = ""),
         delete_layer = TRUE) 


## If the spatial data for all the tracked individuals is very large, then you
## may want to save the files individually for each bird / animal instead.

"OPTION using lists and apply function"
# Load the required libraries
library(sf)
library(purrr)
# Define the factor to split the sf object by
split_by_factor <- "bird_id_num"

# Split the sf object by the factor
split_sf <- split(df_sbtd_sf, df_sbtd_sf[[split_by_factor]])

# Define a function to save sf objects as shapefiles within the specified working
# directory
save_shapefile <- function(sf_obj) {
  st_write(sf_obj,
           paste(path_to_folder, "/",
                 species_name, "_",
                 sf_obj$bird_id[1],
                 ".shp",
                 sep = ""),
           delete_layer = T)
}

# Use the apply function to save the split sf objects as shapefiles
walk(split_sf, save_shapefile)

"OPTION using for loop"
for(i in 1:max(df_sbtd_sf$bird_id_num)){
  ## subset the data taking the track information for each unique bird tagged
  temp <- df_sbtd_sf %>% dplyr::filter(bird_id_num == i)
  ## write the spatial data. Label it by species and bird_id  
  st_write(temp, 
           paste(path_to_folder, "/",
                 species_name, "_",
                 temp$bird_id[1],
                 ".shp", 
                 sep = ""), 
           delete_layer = T)
  ## print a loop progress message
  print(paste("Loop ", i, " of ", max(df_sbtd_sf$bird_id_num), sep = ""))
  ## remove the temporary file at the end of each loop
  rm(temp)
}

"View and assess quality of individual tracks based on outputs above"


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## View the raw tracks for a unique animal ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Specify a unique number (max is total number of unique birds tracked)
length(unique(df_sbtd$bird_id_num))
i = 34
## subset the data from a unique bird
bird_track <- df_sbtd %>% dplyr::filter(bird_id_num == i)
## add a column indicating start and end of tracks
bird_track <- bird_track %>% mutate(nlocs = 1:nrow(bird_track)) %>% 
  mutate(track_segment = if_else(nlocs <= 10, "track.start","track.journey")) %>% 
  ## note: if you have a track with less than 20 points, then you will overwrite 
  ## some of the previous data.
  mutate(track_segment = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"track.end",track_segment)) %>%
  ## add a column indicating colour for start and end of tracks
  ## colours from: https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
  mutate(track_colour = if_else(nlocs <= 10, "#66c2a5","#8da0cb")) %>% 
  mutate(track_colour = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"#fc8d62",track_colour))
  

head(data.frame(bird_track),12)
tail(data.frame(bird_track),12)

"PLOT using Leaflet package"
## plot the tracks using leaflet package in R.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## label by nloc (location) number. Colour accordingly.
  addCircleMarkers(data = bird_track,
                   label = bird_track$nlocs, radius = 3,
                   fillColor = bird_track$track_colour,
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = bird_track$longitude,
               lat = bird_track$latitude, weight = 1,
               color = "white") 
map


## plot with legend
map %>% 
  addLegend(colors = unique(bird_track$track_colour),
            labels = unique(bird_track$track_segment))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initial pre-filter checks for tracking data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Are coordinates in correct format? I.e. is latitude really latitude and longitude 
really longitude?. Or have some coordinates accidently been swapped around?"

"Have you removed pre or post deployment locations from tracking data? i.e. have you
removed sections of tracking data that might represent the drive or boat ride you did
when getting to the colony or returning from the colony?"

## Ensure duplicated times in a trip are removed (artefact of some devices)
## i.e. if a timestamp is duplicated (TRUE), then don't select this data entry
## review your OVERALL data again
head(data.frame(df_sbtd),2)
## is date time column in POSIXct format? i.e. time format for R
str(df_sbtd$dttm)
## remove duplicates
df_sbtd <- df_sbtd %>% 
  ## first group data by individual animals
  group_by(bird_id) %>% 
  ## then if a timestamp is duplicated (TRUE), then don't select this data entry.
  ## only select entries where timestamps are not duplicated (i.e. FALSE)
  dplyr::filter(duplicated(dttm) == F)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Split tracks into individual trips for CPF ----
## Use functions from track2KBA
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## review your OVERALL data again
head(data.frame(df_sbtd),2)

## split the date time column to meet the requirements of data set up for track2KBA
df_sbtd <- df_sbtd %>% 
  mutate(dttm = ymd_hms(dttm), 
         date_gmt = as.Date(dttm), 
         time_gmt = format(dttm, format = "%H:%M:%S"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## track2KBA::formatFields() ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Format the key data fields to the standard used in track2KBA
dataGroup <- formatFields(
  ## your input data.frame or tibble
  dataGroup = df_sbtd, 
  ## ID of the animal you tracked
  fieldID   = "bird_id", 
  ## date in GMT
  fieldDate = "date_gmt", 
  ## time in GMT
  fieldTime = "time_gmt",
  ## longitude of device
  fieldLon  = "longitude", 
  ## latitude of device
  fieldLat  = "latitude"
)

## Check output. Output is a data.frame
head(dataGroup)
str(dataGroup)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## view all data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## quick plot of all data for a quick overview
dataGroup.plot <- st_as_sf(dataGroup, coords = c("Longitude", "Latitude"), crs=4326) # 4326 = geographic WGS84
plot(st_geometry(dataGroup.plot), 
     cex = 0.5, 
     pch = 1)

## number of datapoints
nrow(dataGroup)

## interactive plot
leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = dataGroup,
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) 


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Define colony / origin of trips ----
## This supports application of the tripSplit function
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"OPTION 1: Unique colony for all birds"
## example relates to a seabird during the breeding season, when the species
## is known to be a central place forager. Therefore, define the colony position
## based on the first longitude and latitude coordinates which SHOULD originate
## from the breeding colony
colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

##
head(colony)

## Or specify a unique Lon / Lat for a given colony
# colony <- dataGroup %>% 
#   summarise(
#     Longitude = 16.875879, 
#     Latitude  = 42.774843
#   )


"OPTION 2: Unique colonies / nests per bird"
## IF colony / nest locations vary more widely, then create unique dataframe
## for each bird / animal tracked. Specify a unique nesting location for each
## animal based on the first coordinate of the track.

colony_nest <- dataGroup %>% 
  group_by(ID) %>% 
  summarise(
    ID = first(ID),
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  ) %>% 
  data.frame()

##
head(colony_nest)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## track2KBA::tripSplit() ----
"If your data does not relate to a central place forager (CPF), OR
a time when an animal may be exhibiting central place foraging behaviours,
then skip this section and those relating to CPF data."
## Split tracking data into trips for Central Place Foragers via tripSplit() ----
"This step is often very useful to help automate the removal of location points
on land, or near the vicinty of a colony. We don't want these extra points to bias
our interpretation of the data."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Split the trips
"The user must define ecologically sensible parameters to help automate the tripSplitting
process."
## Input is a 'data.frame' of tracking data and the central-place location(s). 
## Output is a 'SpatialPointsDataFrame'.
trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony_nest, # define source location.
  innerBuff  = 3,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 1,      # hours - defines time an animal must have traveled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  nests = T,           # specify nests = T if using unique colony locations,   
  rmNonTrip  = F    # If true, points not associated with a trip will be removed / if false, points not associated with a trip will be kept
)

"NOTE: the messages that may relate to 'track .... does not return to the colony',
is actually referring to the individual trips from each animal tracked. The code
for track2KBA package needs to be revised to display an '_' between the track ID 
and the individual trip ID. So instead of reading something like 693041, it should
read 69304_1, to better refer to trip 1 of track 69304." 

## Review data after tripSplit()
head(trips,2)
str(trips)
min(trips$ColDist)
trips
table(trips$Returns)


## Simple plot of data after tripSplit
"NOTE: Will take time depending on size of dataset! Considering general plotting
etiquetter when working with data in R."
plot(st_geometry(st_as_sf(trips)), 
     cex = 0.5, 
     pch = 1)

## Split the locations into points to keep and those that will be removed (i.e.
## the points not associated with a trip) for visual plot of the tracks using leaflet package in R.
points_to_keep <- data.frame(trips) %>% 
  dplyr::filter(Returns %in% c("Yes", "No"))
##
points_to_remove <- data.frame(trips) %>% 
  dplyr::filter(!Returns %in% c("Yes", "No"))

  
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = points_to_keep,
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) %>% 
  ##
  addCircleMarkers(data = points_to_remove,
                   radius = 3,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F)
  
map


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## track2KBA::mapTrips() ----
## view data after splitting into trips ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## plot quick overview of trips recorded for individual birds (i.e. the plots show
## an overview of individual trips per bird). Only data for the first 25 birds is
## shown
mapTrips(trips = trips, colony = colony_nest)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Keep points associated with individual trips ----
## Filter the data to only keep the points associated with individual trips that
## were recognised as complete trips.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Let's first check how many trips we record as Yes vs. No before filtering
head(trips,2)
## all locations associated with Return or not
table(trips$Returns)
## summary of trips associated with Return or not
totalTripsAll <- data.frame(trips) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)
## view summary result
table(totalTripsAll$Returns)

## NOW, Filter to only include trips that return
trips <- subset(trips, trips$Returns == "Yes" )

totalTripsYes <- data.frame(trips) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)

## view summary result
table(totalTripsYes$Returns)

"Here, the user should consider if too many individual trips have been removed.
i.e. if you tracked 30 birds and you estimated to have approximately 3 trips 
recoreded per bird, then you would have a total of 90 trips. But it's likely that
on some trips, that not the entire trip was recorded (for multiple reasons).
Therefore, you might expect to rather have about 83 trips recorded across all
birds because for 7 trips data might not have indicated birds returned to the colony.
If you had a very high proportion of trips that did not return to the colony, then
it's likely that you have defined the parameters incorrectly for tripSplit and you
should reconsider better ecologically based estimates for these parameters. There
is of course the chance that there are other issues with your data."

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Check sampling interval ----
## To implement track2KBA fully, you need data approximating an even sampling interval
## i.e. location points must be regularly spaced in time.
## Determine how "gappy" the tracking data is (time intervals between location data)
## This is an important step for almost all tracking data analyses.
## If your data is not filtered / cleaned correctly, results may be spurious.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## data for summarising
head(data.frame(trips),2)
table(trips$Returns)


## Determine difference between consecutive timestamps 
## (NB: consecutive order of timestamps is critical here!)
## Doing this by tripID, not individual ID - change the group_by argument if needed
timeDiff <- trips %>% 
  data.frame() %>% 
  group_by(tripID) %>% 
  arrange(DateTime) %>% 
  mutate(delta_secs = as.numeric(difftime(DateTime, lag(DateTime, default = first(DateTime)), units = "secs"))) %>% 
  slice(2:n()) 

head(data.frame(timeDiff),2)
hist(timeDiff$delta_secs)

## plot histogram of timediff between all points
"This plot will take time depending on size of dataset!"
p4 <- ggplot(timeDiff , aes(delta_secs)) +
  geom_histogram(colour = "darkgrey", fill = "cyan", binwidth = 200)+
  theme(
    axis.text=element_text(size=14, color="black"),
    axis.title=element_text(size=16),
    panel.background=element_rect(fill="white", colour="black")) +
  ylab("n locations") + xlab("Time diff between locations (secs)")

p4

## Summarise results by tripID
SummaryTimeDiff <- timeDiff %>% 
  group_by(tripID) %>% 
  summarise(mean_timegap_secs = mean(delta_secs),
            median_timegap_secs = median(delta_secs),
            min_timegap_secs = min(delta_secs),
            max_timegap_secs = max(delta_secs)) %>%
  ## time in days
  mutate(max_timegap_days =  max_timegap_secs / 86400) %>% 
  mutate(max_timegap_days = round(max_timegap_days,2)) %>% 
  data.frame()

## View results
SummaryTimeDiff
head(SummaryTimeDiff)

## Sort data by maximum time gap first - then view.
## Consider if you have any outlier trips with massively different time gaps.
SummaryTimeDiff %>% arrange(-max_timegap_secs) %>% head(10)
## simple histogram 
hist(SummaryTimeDiff$max_timegap_days)

"HERE: Need to help users understand what might count as too big of a gap in time.
CONSIDER: What guidance / advice can we give?"


## Print warning for users to consider
warning("Consider whether the sampling interval of your tracking data is appropriate
        for formally running the track2KBA functions. Remember, the time differences
        between each of your location points should be equal (or close enough to equal) 
        across all location points and individuals tracked. If the time difference
        between location points is not equal, the outputs you generate from track2KBA
        will not be valid because the underlying kernel density analysis implemented
        within the track2KBA functions will be invalid.")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REVIEW ----
"So far, you have:
1. Read your data into R
2. Formatted your data to align with that of Seabird Tracking Database
3. Saved your data as shapefiles to view in GIS: All tracks and individual animals
4. Interactive plot of individual animals data
5. track2KBA::formatFields for formatting data for track2KBA
6. viewed all data with simple plot
7. Defined colony origin
8. Split tracks into trips via track2KBA::tripSplit
9. view data to keep or remove with simple plot
10. Kept only trips that returned
11. Assessed sampling interval of recorded data"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Average sampling interval of all data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## median of median time gaps in minutes
median(SummaryTimeDiff$median_timegap_secs)/60

"CONSIDER: What is the best way to provide advice on appropriate time gap selection
for interpolation based on results above vs. comparison with intended sampling interval."

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## aniMotum ----
## Process data with aniMotum, GPS data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First, remove trip start and end points near colony
## Why: For the interpolation, especially if using CRAWL, it's best to try and remove
## locations that are on land. That way, the speed filters can focus on points at sea
## when you estimate birds should be moving, as opposed to trying to deal with sections
## of track when the bird is actually stationary on land.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(data.frame(trips))

warning("Must update GitHub tutorial which specifies distance incorrectly")
## remove trip start and end points near colony - distance is in m (unlike innerBuff where distance was in km)
tracks <- trips[trips$ColDist > 3*1000, ]

## interactive plot - review where the individual colony location records
## were deemed to be.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = data.frame(tracks),
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot the colony locations from birds
  addCircleMarkers(data = data.frame(colony_nest),
                   radius = 5,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F) 

map

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## aniMotum filter: individual trip ----
## Subset data for individual trip from a bird ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Specify a unique number (max is total number of unique birds tracked)
length(unique(tracks$tripID))
## example tracks to try
"278: a good track. Athough technically colony location might be wrong given buffer appied above.
305: a good track. Seems feasible.
339: a track that goes over land supposedly.
252: dodgey track with too few points most likely.
210: reasonable track, but some big gaps in data likely when birds commuting.
273: obvious location error with single point extremely far away."

i= 273
## subset the data from a unique trip
bird_track <- data.frame(tracks) %>% dplyr::filter(tripID == unique(tracks$tripID)[i])
## add a column indicating start and end of tracks
bird_track <- bird_track %>% mutate(nlocs = 1:nrow(bird_track)) %>% 
  mutate(track_segment = if_else(nlocs <= 10, "track.start","track.journey")) %>% 
  ## note: if you have a track with less than 20 points, then you will overwrite 
  ## some of the previous data.
  mutate(track_segment = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"track.end",track_segment)) %>%
  ## add a column indicating colour for start and end of tracks
  ## colours from: https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
  mutate(track_colour = if_else(nlocs <= 10, "#66c2a5","#8da0cb")) %>% 
  mutate(track_colour = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"#fc8d62",track_colour))


head(data.frame(bird_track),12)
tail(data.frame(bird_track),12)

## plot the tracks using leaflet package in R.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## label by nloc (location) number. Colour accordingly.
  addCircleMarkers(data = bird_track,
                   label = bird_track$nlocs, radius = 3,
                   fillColor = bird_track$track_colour,
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = bird_track$Longitude,
               lat = bird_track$Latitude, weight = 1,
               color = "white") 
map


## plot with legend
map %>% 
  addLegend(colors = unique(bird_track$track_colour),
            labels = unique(bird_track$track_segment))


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 1: Format the data"
"~~~~~~~~~~~~~~~~~~~~~~~"

head(bird_track,2)

## format the data into format required for aniMotum
## NOTE: The format varies for Argos, GPS and GLS data - format accordingly
bird_track_am <- bird_track %>% mutate(lc = "G") %>% 
  dplyr::select(id = "tripID",
                date = "dttm",
                lc,
                lon = "Longitude",
                lat = "Latitude")

## review the newly formatted data
head(bird_track_am,20)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 2: Fit the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

"When fitting the model, there are some useful parameters to consider"
## fit the state-space model
## SEE the help file: ?fit_ssm, to understand some of the arguments within the function
## NOTE: the function can do 3 things simultaneously: data formatting step, a pre-filtering step, and the actual model fitting
## INPUT: data.frame, tibble or sf-tibble of observations, depending on the tracking data type
fit <- fit_ssm(bird_track_am,
               ## specify what kind of model you want to fit. See details about different model types in paper.
               model = "crw",
               ## specify the speed at which data points could be considered outlier points (in m/s)
               vmax = 27,
               ## time.step in hours - specify time.step of new values to be predicted (interpolation)
               time.step = 0.5,
               ## turning angle/s in degrees. remove locations with turning angles set between intervals
               ## default values are not 0,0 (which will not do anything), but rather 15,25
               ang = c(0, 0),
               ## step lengths in km - check implications for GPS vs. Argos data filtering
               ## defaults 2500,5000
               distlim = c(2500, 5000))

"NOTE: Depending on how you prefilter your data before running fit_ssm, you may
want to consider changing some of the function parameters. E.g. you might indicate
fit.to.subset = F, if you have filtered your data already and are sure all your 
locations are true locations."


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 3: Review the model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

## review the model summary
## See: https://ianjonsen.github.io/aniMotum/articles/Overview.html
"Check that converged and phHess were True. NOTE: I'm not sure what it means if they are false"
fit
"Review overall summaries and SSM details for each individual. Again, not entirely sure what all the important bits are"
summary(fit)


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 4: Review the different tabular ouputs after fitting the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

## data.frame of SSM fitted values (location estimates corresponding to the observation times)
floc.fitted <- grab(fit, what = "fitted")

## data.frame of predicted values (corresponding to locations predicted at regular time.step intervals)
floc.predicted <- grab(fit, what = "predicted")

## data.frame of original data with a column indicating which locations to keep or not
floc.data <- grab(fit, what = "data")

## review the new data frames you get and your original data
head(data.frame(floc.fitted),2)
head(data.frame(floc.predicted),2)
head(data.frame(floc.data),2)
head(data.frame(bird_track),2)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 5: Plot the different tabular ouputs after fitting the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

## plot the FITTED values over original data (i.e. locations used for fitting the model)
map %>% addCircleMarkers(data = floc.fitted,
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "lightgreen",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "lightgreen",
            labels = "fitted values")

## plot the PREDICTED values over original data (i.e. locations predcited from the model)
map %>% addCircleMarkers(data = floc.predicted,
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "cyan",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "cyan",
            labels = "predicted values")


## plot the REMOVED values over original data (i.e. locations that were removed from the prefiltering step)
map %>% addCircleMarkers(data = subset(floc.data, floc.data$keep == F),
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "red",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "red",
            labels = "removed values")


## plot the PREDICTED AND REMOVED values over original data (i.e. locations that were removed from the prefiltering step)
map %>% addCircleMarkers(data = floc.predicted,
                       #label = bird_track_gaps$nlocs, 
                       radius = 3,
                       fillColor = "cyan",
                       fillOpacity = 0.5, stroke = F) %>% 
  addCircleMarkers(data = subset(floc.data, floc.data$keep == F),
                   #label = bird_track_gaps$nlocs, 
                   radius = 5,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F)



"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 6: Visualising a model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

# plot time-series of the fitted values
plot(fit, what = "fitted", type = 1, pages = 1)

# plot time-series of the predcited values
plot(fit, what = "predicted", type = 1, pages = 1)

# plot fitted values as a 2-d track
plot(fit, what = "predicted", type = 2, pages = 1,
     ## 95 % confidence ellipses (orange-filled ellipses) around the predicted 
     ## values are also displayed, but can be faded away by choosing a low alpha value
     alpha = 0.05,
     ## Observations that failed the prefilter stage are displayed (black x’s) 
     ## by default but can be turned off with the argument outlier = FALSE)
     outlier = T)

# plot fitted values as a 2-d track
plot(fit, what = "predicted", type = 2, pages = 1,
     ## 95 % confidence ellipses (orange-filled ellipses) around the predicted 
     ## values are also displayed, but can be faded away by choosing a low alpha value
     alpha = 0.00,
     ## Observations that failed the prefilter stage are displayed (black x’s) 
     ## by default but can be turned off with the argument outlier = FALSE)
     outlier = T)

"CONSIDER: How can we help user to decide whether their data is of high enough
quality or not for a track2KBA styled analysis? Perhaps the outputs from 
grab(fit, what = predicted) can be of help? Here, see an indication of standard 
errors around predicted locations via (x.se, y.se in km)"

plot(floc.predicted$x.se)
plot(floc.predicted$y.se)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 7: Further assessment of model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://ianjonsen.github.io/aniMotum/articles/SSM_validation.html

"Does this assessment take into account all tracks simultanesouly? Or does it only
assess each track individually? What are the implications for this assessment in
the context of track2KBA? Not sure..."

# use patchwork package to arrange plot.osar options
library(patchwork)
# calculate & plot residuals
"NOTE: Computationally intensive! Takes time!!"
res.rw <- osar(fit)

(plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer())


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 8: Assess potential behaviours along track: Move_persistence_models"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://ianjonsen.github.io/aniMotum/articles/Move_persistence_models.html

## NOTE: You can fit this model in two ways

## SEE: Alternate script: tracking_CleanAndPrepareData2_AllTracks_aniMotumAllSteps


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 9: Reroute tracks that went overland back via the sea"
"~~~~~~~~~~~~~~~~~~~~~~~"

## NOTE: This will reroute the point locations only! So if you have a very detailed
## coastline, then it may appear the animals still move over land when plotting lines 
## between points. The success of the analysis is also dependent on the underlying
## basemap used. The natural earth map (used by default) is good, but not very finely
## detailed. i.e. resolution could be higher


## install packages
#install.packages("pathroutr", repos = "https://jmlondon.r-universe.dev")
library(pathroutr) # for rerouting tracks
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires) # for higher resolution natural earth map

## reroute the track using the predicted values of the previously fitted model
fit.reroute <- route_path(fit,
                  what = "predicted",
                  map_scale = 10,
                  dist = 10000,
                  append = T)

## data.frame of rerouted values 
## NOTE: Some of these locations may not be ecologically realistic anymore
## i.e. if you were to recalculate travel speeds, they may be unrealistic
## must consider trade-off of approach accordingly
floc.predicted.reroute <- grab(fit.reroute, what = "rerouted")

## review data
head(data.frame(floc.predicted),2)
head(data.frame(floc.predicted.reroute),2)

## plot original vs predicted vs re-routed
map %>% 
## Predicted
  addCircleMarkers(data = floc.predicted,
                   #label = bird_track$nlocs, 
                   radius = 5,
                   fillColor = "green",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between predicted points
  addPolylines(lng = floc.predicted$lon,
               lat = floc.predicted$lat, weight = 1,
               color = "green") %>% 
  ## RE-ROUTED
  addCircleMarkers(data = floc.predicted.reroute,
                   #label = bird_track$nlocs, 
                   radius = 3,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between re-routed points
  addPolylines(lng = floc.predicted.reroute$lon,
               lat = floc.predicted.reroute$lat, weight = 1,
               color = "red")

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 10: Reroute tracks that went overland back via the sea using pathroutr package"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://rdrr.io/github/jmlondon/pathroutr/f/vignettes/reroute_demo.Rmd

## Consider the tutorial for pathroutr

## NOTE: This is very computationally expensive when you have many data points
## and high resolution coastline data. Therefore, it may be worth subsetting 
## parts of the track that go over land and trying to reroute these parts only.
## Then you could merge these parts of the track back onto the remainder of the track

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 11: Simulate animal tracks"
"~~~~~~~~~~~~~~~~~~~~~~~"

## NOTE: This step is used more for habitat modelling / SDMs. 
## Step not required for track2KBA

st <- sim_fit(fit, what="predicted", reps=5, 
              ## cpf: is the animal exhibiting central place foraging behaviour?
              cpf=T)

plot(st, zoom=TRUE)

"NOTE: Can also reroute these simulated tracks again as above."


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## aniMotum filter: All trips ----
## Bulk filter individual trips from all birds ----
"May need to consider a way of bulk checking quality of data."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 1: Format the data"
"~~~~~~~~~~~~~~~~~~~~~~~"

all_track_am <- data.frame(tracks) %>% mutate(lc = "G") %>% 
  dplyr::select(id = "tripID",
                date = "dttm",
                lc,
                lon = "Longitude",
                lat = "Latitude")

## remove trips with <5 locations; as required for track2KBA analysis
trips_to_keep <- all_track_am %>% 
  group_by(id) %>% 
  summarise(triplocs = n()) %>% 
  dplyr::filter(triplocs > 5)

## filter out the tracks
all_track_am <- all_track_am %>% dplyr::filter(id %in% trips_to_keep$id)


##
head(all_track_am,2)
length(unique(all_track_am$id))


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 2: Fit the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

## fit the model to all data
fit_alltrack <- fit_ssm(all_track_am,
                        ## specify what kind of model you want to fit. See details about different model types in paper.
                        model = "crw",
                        ## specify the speed at which data points could be considered outlier points (in m/s)
                        vmax = 27,
                        ## time.step in hours - specify time.step of new values to be predicted (interpolation)
                        time.step = 0.5)

"NOTE: Depending on how you prefilter your data before running fit_ssm, you may
want to consider changing some of the function parameters. E.g. you might indicate
fit.to.subset = F, if you have filtered your data already and are sure all your 
locations are true locations."


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 3: Review the model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

## review the model summary
## See: https://ianjonsen.github.io/aniMotum/articles/Overview.html
"Check that converged and phHess were True. NOTE: I'm not sure what it means if they are false"
fit_alltrack
fit_alltrack %>% dplyr::filter(converged == F)
fit_alltrack %>% dplyr::filter(pdHess == F)
"Review overall summaries and SSM details for each individual. Again, not entirely sure what all the important bits are"
summary(fit_alltrack)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 9: Reroute tracks that went overland back via the sea"
"~~~~~~~~~~~~~~~~~~~~~~~"

## NOTE: This will reroute the point locations only! So if you have a very detailed
## coastline, then it may appear the animals still move over land when plotting lines 
## between points. The success of the analysis is also dependent on the underlying
## basemap used. The natural earth map (used by default) is good, but not very finely
## detailed. i.e. resolution could be higher

## reroute the track using the predicted values of the previously fitted model
fit.reroute.all <- route_path(fit_alltrack,
                          what = "predicted",
                          map_scale = 10,
                          dist = 10000,
                          append = T)

## data.frame of rerouted values 
## NOTE: Some of these locations may not be ecologically realistic anymore
## i.e. if you were to recalculate travel speeds, they may be unrealistic
## must consider trade-off of approach accordingly
floc.predicted.reroute <- grab(fit.reroute, what = "rerouted")

## review data
head(data.frame(floc.predicted),2)
head(data.frame(floc.predicted.reroute),2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Speed filter / linear interpolation ----
"For flying seabirds: CRAWL may not be best bet - linear interpolation may be better."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## review example data
head(bird_track,2)

## remove any erroneous locations due to speed use the McConnel Speed Filter 
##from the trip package
trip_obj <- bird_track %>% 
  #group_by(tripID) %>% 
  dplyr::select(x = X, 
                y = Y, 
                DateTime, 
                everything()) %>% 
  trip()

## McConnel Speedilter -----
## apply speedfilter and creat data frame
trip_obj$Filter <- speedfilter(trip_obj, max.speed = 100)  # speed in km/h
trip_obj <- data.frame(trip_obj)
head(trip_obj,2)

## plot the animMotum REMOVED values over original data AND McConnel Removed values
map %>% addCircleMarkers(data = subset(floc.data, floc.data$keep == F),
                         #label = bird_track_gaps$nlocs, 
                         radius = 7,
                         fillColor = "red",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "red",
            labels = "aniMotum removed values") %>% 
  addCircleMarkers(data = subset(trip_obj, trip_obj$Filter == F),
                   #label = bird_track_gaps$nlocs, 
                   radius = 5,
                   fillColor = "black",
                   fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "black",
            labels = "McConnel removed values")
  
## Keep only filtered coordinates - after checking dimensions of other outputs again
dim(trip_obj)
dim(bird_track)
dim(floc.data)
dim(floc.predicted)
trip_obj <- subset(trip_obj,trip_obj$Filter==TRUE)
dim(trip_obj)


## Linear interpolation -----
## Apply linear interpolation step to speed filtered only data

## create ltraj object
trip_lt <- as.ltraj(xy = bind_cols(x = trip_obj$x, 
                                   y = trip_obj$y),
                    date = trip_obj$DateTime,
                    id = trip_obj$tripID)

## Linearly interpolate/re-sample tracks every 30 minutes (specified in seconds)
trip_interp <- redisltraj(trip_lt, 1800, type="time")
head(trip_interp)


## convert back into format for track2KBA - dataframe for now
trip_interp <- ld(trip_interp) %>% 
  dplyr::mutate(Longitude = x,
                Latitude = y)
head(trip_interp,2)

## Select key data for track2KBA
"update"
#head(tracks_yelk)
#yelk_interp <- yelk_interp %>% dplyr::select(X = x,Y =y, DateTime = date, ID = id)

## update metadata that was lost during interpolation steps
"update"
#yelk_meta <- tracks_yelk %>% 
#  data.frame() %>% 
#  dplyr::select(ID, colony_code) %>% 
#  distinct(ID, colony_code)

## update for track2KBA
"update"
#yelk_interp <- left_join(yelk_interp, yelk_meta, by = "ID") %>% 
#  st_as_sf(coords = c("X", "Y"), crs = wgs84)

## plot original vs predicted from aniMotum vs re-routed from aniMotum vs speedfilter & linear interpolation
map %>% 
  ## Predicted
  addCircleMarkers(data = floc.predicted,
                   #label = bird_track$nlocs, 
                   radius = 5,
                   fillColor = "green",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between predicted points
  addPolylines(lng = floc.predicted$lon,
               lat = floc.predicted$lat, weight = 1,
               color = "green") %>% 
  ## RE-ROUTED
  addCircleMarkers(data = floc.predicted.reroute,
                   #label = bird_track$nlocs, 
                   radius = 3,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between re-routed points
  addPolylines(lng = floc.predicted.reroute$lon,
               lat = floc.predicted.reroute$lat, weight = 1,
               color = "red") %>% 
  ## Speed Filtered and Linear interpolated
  addCircleMarkers(data = trip_interp,
                   #label = bird_track$nlocs, 
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between Speed Filtered and Linear interpolated points
  addPolylines(lng = trip_interp$Longitude,
               lat = trip_interp$Latitude, weight = 1,
               color = "cyan")

