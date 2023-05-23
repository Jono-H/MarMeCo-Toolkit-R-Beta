#' # Tracking data: Load data into R and visualise
#' 
#' <!--- This is an HTML comment in RMarkdown. You can use these comments to make notes that won't get read when running the code -->
#' 
#' <!--- If you don't understand what a RMarkdown document is. Stop here. Go learn. -->
#' 
#' <!--- Equally. You must understand the difference between Markdown vs. RMarkdown -->
#' 
#' <!--- Remember, outside of the R code chunks we are now coding in HTML syntax, not R syntax -->
#' 
#' This tutorial uses example data from a project led by the BirdLife International partner in Croatia: BIOM
#' 
#' The citation for this data is: **TBC**
#' 
#' The example data can be downloaded from: **TBC - SBTD**
#' 
#' Analyses outlined in this chapter were performed in **`r sessionInfo()$R.version$version.string`**\
#' 
#' This chapter was last updated on **`r Sys.Date()`** <br>
#' 
#' <!--- In the code chunk below, we specify include = F, so that we will run the chunk but not include the chunk in the final document. We set a global argument in the code chunk of echo = T, so that in later code chunks, the code will be displayed in the RMarkdown document -->
#' 
## ----track-yelk-setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
## we also specify in the options here to set the overall working directory
## back to the root directory of the R Project we are working in. We do this
## because by default , the working directory for R code chunks is the directory 
## that contains the Rmd document. We don't want this option given our file
## set up prefers the option of having the working directory specified as that
## where the R Project is. By specifying double dots (or more), this is like saying
## go back one directory or more, as required.
knitr::opts_knit$set(root.dir = "..")

#' 
#' <br>
#' 
#' ## Description of the example dataset
#' 
#' <!--- remember, single * is italics, ** is bold -->
#' 
#' Species tracked: Yelkouan Shearwater (*Puffinus yelkouan*)
#' 
#' Life-cycle stage when birds were tracked: chick-rearing
#' 
#' Site / source population birds tracked from: Lastovo SPA, Croatia
#' 
#' Years birds were tracked over: 2019, 2020
#' 
#' Devices birds were tracked with: GPS
#' 
#' Device model type: PathTrack nanoFix GPS/UHF transmitters (≤ 5.5 g)
#' 
#' [Figure showcasing Lastovo SPA and source populations]
#' 
#' <br>
#' 
#' ## Objective of chapter
#' 
#' Support implementation of track2KBA analysis for flying seabirds during the breeding period.
#' 
#' <br>
#' 
#' ## Load packages
#' 
#' **Load required R packages:**
#' 
#' If the package(s) fails to load, you will need to install the relevant package(s).
#' 
## ----track-yelk-load-packages, include = TRUE----------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries --------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## sf package for spatial data analyses (i.e. vector files such as points, lines, polygons)
library(sf)
## Tidyverse for data manipulation
library(tidyverse)
## ggplot2 for plotting opionts
library(ggplot2)
## rnaturalearth package for geographic basemaps in R
library(rnaturalearth)
## leaflet package for interactive maps in R
library(leaflet)
## lubridate for date time
library(lubridate)
## track2kba for the analysis of important site identification
library(track2KBA)
## speed filter
library(trip)
## linear interpolation
library(adehabitatLT)
##
library(raster)
##
library(viridis)


#' 
#' <br>
#' 
#' ## Input parameters for chapter tutorial
#' 
#' Here we define input parameters needed for sections of the code later in this tutorial.
#' 
#' Depending on how your data is set up, you should not need to define any further input parameters.
#' 
## ----track-yelk-input-parameters, include = TRUE-------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Specify projections / store needed CRS definitions as variables ----
## SEE: https://epsg.io/
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## world - unprojected coordinates
wgs84 <- st_crs("EPSG:4326")

## Croatia -  projected coordinates
htrs96 <- st_crs("EPSG:3765")

## Source a world map from the rnaturalearth R package
## see details of function to change the type of map you get
worldmap <- rnaturalearth::ne_download(scale = "large",
                                       type = "countries",
                                       category = "cultural",
                                       destdir = tempdir(),
                                       load = TRUE,
                                       returnclass = "sf")


#' 
#' 
#' 
#' ## Storing, reading, and formatting raw tracking data
#' 
#' <br>
#' 
#' ### Storing raw tracking data
#' 
#' The type of device you use will dictate what format your raw tracking data is stored in.
#' 
#' Typically, we will work with **.csv** files.
#' 
#' Good file management is critical when working with large tracking datasets.
#' 
#' [As a guide, the following file structure can support efficient data management]
#' 
#' <br>
#' 
#' ### Reading raw tracking data into R / Rstudio
#' 
#' Depending on your file structure, type of raw data, and size of your overall data, we recommend reading data into R in a way that produces a single data frame (or tibble) for all your data required for a specific analysis.
#' 
#' [Example R code for reading in raw tracking data is provided in the Appendix]
#' 
#' <br>
#' 
#' ### Format of data
#' 
#' Having data standardised into a common format greatly improves reproducible research, and also the ability for data to be used in other studies.
#' 
#' The primary format we recommend is that of BirdLife International's Seabird Tracking Database:
#' -   <https://www.seabirdtracking.org/>
#' 
#' We recognise, however, that this format may not be appropriate for all analyses. Nevertheless, we encourage users to standardise their data into a common format. This will facilitate the ease through which data can be reformatted when necessary for other analyses.
#' 
#' [Decide on best way to show example datasets - either as screen shot images? Or as example data files? Or perhaps as both. Maybe just taking subsets of the data as required.]
#' 
#' <br>
#' 
#' ## Load raw tracking data
#' 
#' Below, we load the raw tracking data obtained for Yelkouan Shearwaters.
#' 
#' To see how this data was loaded into R originally, and merged to create a single data frame, see the example code in the Appendix.
#' 
#' [Example R code for reading in raw tracking data is provided in the Appendix]
#' 
#' The `load` function supports loading various R file formats. Here we are loading an `.Rdata` file. The file was previously saved with the name of `yelk`. So when we load the file, an object called `yelk` will be loaded into the working environment in R.
#' 

## Example script to merge and add in multiple csv files which have a common format:
fpath.tracks <- "C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\PROJECTS\\Marine Toolkit\\Partner_Testing\\Europe_MarineToolkit\\Data-PuffYelk-Tracking-SBTD"

track.df <- data.frame()

track.list <- list.files(path = fpath.tracks,
                         pattern = ".csv",
                         full.names = T)

for(i in 1:length(track.list)){
  temp <- read.csv(track.list[i])
  track.df <- rbind(track.df,temp)
  print(i)
}

##
head(track.df,2)


## ----track-yelk-load-data, include = TRUE--------------------------------------------------------------

## ----- set the working directory that you will pull data in from.

## Load the example data for Yelkouan Shearwaters
load("data-testing/tracking-data/Tracking_YESH_raw.Rdata")

## view the first two rows of data
## First view the data in tibble format
head(yelk,2)
## Then view the data in data frame format
head(data.frame(yelk),2)


#' 
#' > **tibble vs data frame**: we don't go into the specifics of these different data formats. The key message is that each provides a different way of interacting with, or viewing, data. Both are essentially a mechanism through which to work with tabular data. (i.e. data in rows and columns)
#' 
#' [Decide on best way to show example datasets – either screen shot images? Or as example data files? Or perhaps as both. Maybe just taking subsets of the data as required.]
#' 
#' <br>
#' 
#' ## Format data to match that of the Seabird Tracking Database
#' 
#' In the example dataset, you will notice that the data is not in the format of that relating to the seabird tracking database.
#' 
#' We can reformat the data by extracting the relevant columns of information, and by adding in any information where it might be missing.
#' 
#' 
## ----track-yelk-sbtd-format, include=TRUE--------------------------------------------------------------

## First, add relevant columns of information to align with SBTD format
## the mutate functions allows you to add a new column of information.
## add the new columns and rename the object to a more standardised name.
df_sbtd <- yelk %>% dplyr::mutate(dataset_id = "tbc",
                                  scientific_name = "Puffinus yelkouan",
                                  common_name = "Yelkouan Shearwater",
                                  site_name = "Lastovo SPA",
                                  lat_colony = "tbc",
                                  lon_colony = "tbc",
                                  device = "GPS",
                                  age= "adult",
                                  sex= "unknown",
                                  breed_stage = "chick-rearing",
                                  breed_status = "breeding",
                                  argos_quality = NA,
                                  equinox = NA) 

## Create the separate date and time columns to match the format of the SBTD, where
## these columns are provided separately
df_sbtd <- df_sbtd %>% mutate(date_gmt = date(dttm),
                              time_gmt = format(dttm, format = "%H:%M:%S"))


## review the changes you have made (i.e. the new columns you have added)
head(data.frame(df_sbtd),2)

## Now select all the relevant columns to align data with the format of the 
## seabird tracking database.
## There are 21 columns of data in the format for the SBTD.
## Remember, when you use the select function, you can also rename columns simultaneously.
df_sbtd <- df_sbtd %>% dplyr::select(dataset_id,
                                     scientific_name,
                                     common_name,
                                     site_name,
                                     ## below for example, we select the column 
                                     ## called colony_code but rename it to colony_name
                                     colony_name = colony_code,
                                     lat_colony,
                                     lon_colony,
                                     device,
                                     bird_id = bird_id,
                                     track_id = bird_id,
                                     original_track_id = bird_id,
                                     age,
                                     sex,
                                     breed_stage,
                                     breed_status,
                                     date_gmt,
                                     time_gmt,
                                     latitude,
                                     longitude,
                                     argos_quality,
                                     equinox)

## review the changes again
head(data.frame(df_sbtd),2)

#' 
#' <br>
#' 
#' ## Review of the example data so far
#' 
#' For the following columns, you may notice a few things: 
#' <br>
#' 
#' * dataset_id is specified as *tbc*. This is because until data has been loaded into the SBTD, it will not have a unique dataset identification code that would relate to the dataset stored in the SBTD.
#' 
#' * lat_colony, lon_colony are specified as *tbc*, because we still need to define what the colony coordinates would be for each of locations birds were tagged from.
#' 
#' * bird_id, track_id, original_track_id, are all specified with the same code. This is because when data is formatted to align with the format of the SBTD:
#'   * we have a code that relates to the bird that was tracked (bird_id)
#'   * we have a SBTD unique code that relates to each trip undertaken by the bird, when multiple trips are recorded (track_id). Note though, it is often the case that users do not provide data which has been pre-split into unique trips. Therefore, it is often the case that all entries relating to track_id match that of bird_id.
#'   * we have a user defined code that can relate to each trip undertaken by the bird. However, the same caveat in the case of track_id applies to this column of data too.
#' 
#' * argos_quality and equinox are both specified as NA. This is because our data relates to GPS data which does not have an argos_quality estimate (typical of PTT devices) or a measure relating to the equinox (typical of GLS devices).
#' 
#' <br>
#' 
#' ## Explore the tabular data
#' 
#' Before you plot any data, it can be a good idea to broadly explore the data. 
#' 
#' While you might know which species you tracked, and from which colonies, and from which years, it can often be worth checking over these (and other) aspects of your data.
#' 
#' Checking the data helps refresh your view on what data you have, and also helps you pick up any errors that may have arisen when inputting data.
#' 
## ----track-yelk-explore-data, include=TRUE-------------------------------------------------------------

## Reminder on what the data looks like so far
head(data.frame(df_sbtd),2)

## Review the main columns of data separately. This helps check for errors associated 
## with data entry. E.g. perhaps you typed chick-rearing and CHICK-rearing. Because
## of the difference in lower-case vs. upper-case text, you might accidentally consider
## these as separate components of your dataset.
## the table function is useful to check the unique number of entries per unique input
table(df_sbtd$scientific_name)
table(df_sbtd$site_name)
table(df_sbtd$colony_name)
table(df_sbtd$breed_status)
table(df_sbtd$breed_stage)
table(df_sbtd$age)
table(df_sbtd$sex)

## Summarise the data by species, site_name, colony_name, year, 
## breed_status, breed_stage, age, sex.
## First we add a new year column by splitting the date column so we can get information about years
df_overview <- df_sbtd %>% mutate(year = year(date_gmt)) %>% 
  ## then we group the data by relevant columns
  group_by(scientific_name, 
           site_name, 
           colony_name, 
           year,
           breed_status, 
           breed_stage,
           age, 
           sex) %>% 
  ## then we continue to summarise by the distinct number of entries per group
  summarise(n_birds = n_distinct(bird_id),
            n_tracks = n_distinct(track_id),
            n_original_tracks = n_distinct(original_track_id))

## review the summary output
df_overview


#' 
#' ## Review of summary output
#' 
#' From the summary output above we can see the following:
#' 
#' * scientific_name: we have tracking data from one species
#' * site_name: we have tracking data from one general site
#' * colony_name: we have tracking data from three colonies
#' * year: data comes from between 2019 and 2021
#' * breed_status and breed_stage: all data relates to breeding birds in the chick-rearing life-cycle stage.
#' * age and sex: data is from adult birds of unknown sex
#' * n_birds, n_tracks, n_original_tracks: because n_birds = n_tracks, it indicates that:
#'   * either the tracking data from each individual bird has not been separated into unique trips, or
#'   * the tracking data from each individual bird is only representative of a single trip to sea
#' 
#' <br>
#' 
#' ## dataGroup: define your dataGroup
#' 
#' The movement patterns and spatial distributions of highly mobile species often change across seasons. Therefore ‘track2KBA’ is designed to identify important sites for a source population year-round or during a specific seasonal life-cycle stage. It is important that input tracking data represent movements sampled from periods in which the animals in the source population are moving in a similar manner to one another. In the initial analytical functions in the ‘track2KBA’ workflow, the input argument for tracking data is ‘dataGroup’, and is intended to represent tracks of animal movements sampled from the same source population and seasonal life-cycle stage.
#' 
#' [Further guidance needed on how to define the dataGroup, plus examples]
#' 
#' ## track2KBA::formatFields()
#' 
#' This function will help format your data to align with that required of track2KBA.
#' 
#' In other words: for the track2KBA functions to work, your data needs to have certain columns named in the appropriate way. This function will help with that.
#' 
## ----track-yelk-ind-formatFields, include = TRUE-------------------------------------------------------

## review current data
head(data.frame(df_sbtd),2)

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
head(dataGroup,2)
str(dataGroup)


#' 
#' 
#' ## dataGroup for the example data
#' 
#' Typically a unique dataGroup for a breeding seabird will represent data from a unique species, tracked from a unique colony, within a unique life-cycle stage.
#' 
#' Therefore, we will subset the data to extract the information as relevant.
#' 
#' ## extract dataGroup data
#' 
## ----track-yelk-dataGroup, include=TRUE----------------------------------------------------------------


dataGroup <- dataGroup %>% mutate(year = year(DateTime)) %>% 
  dplyr::filter(scientific_name == "Puffinus yelkouan",
                colony_name == "Z" &
                breed_stage == "chick-rearing") # &
                #year == "2019")

## check you did this correctly
unique(dataGroup$scientific_name)  
unique(dataGroup$colony_name)  
unique(dataGroup$breed_stage)
unique(dataGroup$year)  

## review overall data
head(dataGroup,2)


#' 
#' <br>
#' 
#' ## Arrange data and remove duplicate entries
#' 
#' Once you have formatted your data into a standardised format and ensured that parts of your data is inputted correctly, it is also worth ensuring your data is ordered (arranged) correctly chronologically. An artifact of manipulating spatial data is that sometimes the data can become un-ordered with respect to time, or, given the way various devices interact with satellites, you can also end up with duplicated entries according to timestamps.
#' 
#' This can be a first problem, causing your track to represent unrealistic movement patterns of the animal.
#' 
#' We need to ensure our data is ordered correctly and also remove any duplicate timestamps.
#' 
## ----track-yelk-remove-duplicates, include=TRUE--------------------------------------------------------

## review your OVERALL data again
head(data.frame(dataGroup),2)

## first check how many duplicate entries you may have. If there are many, it
## is worth exploring your data further to understand why.
n_duplicates <- dataGroup %>% 
  group_by(ID) %>% 
  arrange(DateTime) %>% 
  dplyr::filter(duplicated(DateTime) == T)

## review how many duplicate entries you may have. Print the message:
print(paste("you have ",nrow(n_duplicates), " duplicate records in a dataset of ", nrow(dataGroup), " records.", sep =""))
            
## remove duplicates entries if no further exploration is deemed necessary
dataGroup <- dataGroup %>% 
  ## first group data by individual animals and unique track_ids
  group_by(ID) %>% 
  ## then arrange by timestamp
  arrange(DateTime) %>% 
  ## then if a timestamp is duplicated (TRUE), then don't select this data entry.
  ## only select entries where timestamps are not duplicated (i.e. FALSE)
  dplyr::filter(duplicated(DateTime) == F)


#' 
#' <br>
#'   
#' ## Visualise all the location data
#' 
#' Using the `leaflet` package in R, you can easily visualise your tracking data interactively within RStudio.
#' 
#' What should you look for when visualising the raw data?
#' * Are your locations in realistic places?
#' * Have you perhaps mixed up the latitude and longitude columns?
#' * Does your data cross the international date line? Do you know how to deal with this?
#' * Will you need to remove sections of the data that do not represent a time when the animal was tagged? (e.g. perhaps you set the device to start recording locations before deploying on the animal. So the tag might have recorded while you were travelling to the deployment location. Therefore, removing these sections of the track will facilitate your overall analysis.)
#' 
## ----track-yelk-visualise-data, include=TRUE-----------------------------------------------------------

## review your OVERALL data again
head(data.frame(dataGroup),2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## visualise all data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## number of datapoints
nrow(dataGroup)

## interactive plot
map.alldata <- leaflet() %>% ## start leaflet plot
  ## select background imagery
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  addCircleMarkers(data = dataGroup,
                   ## size of points
                   radius = 3,
                   ## colour of points
                   fillColor = "cyan",
                   ## transparency of points
                   fillOpacity = 0.5, 
                   ## set stroke = F to remove borders around points
                   stroke = F) 

## generate the plot
map.alldata


#' 
#' ## Review of overall plot for all data points
#' 
#' Based on the interactive plot, you can see that generally the data looks good. Generally, all the locations are in the Adriatic Sea area (something we would anticipate based on what we know about Yelkouan Shearwaters breeding in Croatia). We can conclude the following:
#' 
#' <br>
#' 
#' * Locations appear to be in realistic places.
#' * It's unlikely that we have mixed up the latitude and longitude columns.
#' * The data does not cross the international date line.
#' 
#' <br>
#' 
#' Regarding removing sections of the data that do not represent a time when the animal was tagged: Later filtering steps may remove these parts of the track if locations are near the vicinity of the colony (see details of the `tripSplit()` function. However, if there are broader location data associated with these types of locations, you will need to remove these sections of the track.
#' 
#' <br>
#' 
#' ## Add unique numeric ID for each animal
#' 
## ----track-yelk-ind-create, include=TRUE---------------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First add a simplified unique id and create the sf spatial object ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Review data
head(data.frame(dataGroup),2)

## add a simplified animal ID column - a simple number for each unique animal tracked
dataGroup$ID_num <- as.numeric(factor(dataGroup$ID, levels = unique(dataGroup$ID)))

## Review data again (tail function prints the end of the dataframe so you can
## check if the last unique number matches the number of animals you tracked.)
head(data.frame(dataGroup),2)
tail(data.frame(dataGroup),2)

## create the sf spatial object
dataGroup_sf <- dataGroup %>% 
  ## first create new columns of lon and lat again so you keep this location 
  ## information in tabular format.
  mutate(lon_device = Longitude,
         lat_device = Latitude) %>% 
  ## then convert object to sf spatial object
  st_as_sf(coords = c("lon_device", "lat_device"), crs = wgs84)

#' 
#' ## Save all the location data as a shapefile
#' 
#' Visualising all the location data in R can be a simpler starting point. You may also want to save this data as a shapefile (.shp) for viewing in GIS software such as QGIS or ArcGIS.
#' 
#' > Note: saving all data as a single shapefile can be a memory intensive task (i.e. if you have a lot of data, then your computer might take a long time to save the file, or the file will be big and slow to work with)
#' 
#' 
#' 
## ----track-yelk-all-shapefile, include=TRUE, eval = FALSE----------------------------------------------
## 
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## Save raw tracking data as shapefile for viewing in GIS software ----
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## ## Option allows for multispecies data
## ## Or the loop will only run once if you have single species data
## 
## for(i in unique(dataGroup_sf$scientific_name)){
## 
##   ## subset the data taking the track information for each unique species
##   temp_species <- dataGroup_sf %>% dplyr::filter(scientific_name == i)
## 
##   ## create new folder within current working directory where you will save data
##   ## first create the name of the species and the file path you need
##   ## also use gsub to replace spaces within character strings (words) with a "-"
##   species_name <- gsub(" ", "-", temp_species$scientific_name[1])
## 
##   ## print the name for checking
##   print(species_name)
## 
##   ## then create the new folder within current working directory
##   path_to_folder <- paste("./data-testing/tracking-data/",
##                           species_name,
##                           "/shapefiles-all-tracks",
##                           sep="")
## 
##   ## print the file path name for checking
##   print(path_to_folder)
## 
##   ## Check if folder exists, and if it does not, then make a new folder
##     if (!file.exists(path_to_folder)) {
##     # If it does not exist, create a new folder
##     dir.create(path_to_folder)
##     print(paste("Created folder:", path_to_folder))
##     } else {
##     # do nothing, but let us know the folder exists already
##     print(paste("Folder already exists:", path_to_folder))
##     }
## 
##   ## write the spatial data as a shapefile
##   ## NOTE: For some GIS software, column names will be abbreviated upon saving
##   ## NOTE: If you have very long file paths, this operation may fail. One solution
##   ## is to save the shapefile elsewhere. Another solution is to instead save the file
##   ## as a geopackage (.gpkg): simply replace the .shp text below with .gpkg
##   st_write(dataGroup_sf, paste(path_to_folder,"/",
##                              species_name,
##                              "_AllTracks.shp",
##                              sep = ""),
##            delete_layer = TRUE)
## 
##     ## remove the temporary file at the end of each loop
##   rm(temp_species)
## }
## 

#' 
#' ## Save all the location data as a plot
#' 
#' [A simple plot to look at all the point location data]
#' 
## ----track-yelk-all-plots, include=TRUE, eval = FALSE--------------------------------------------------
## 
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## Save raw tracking data as simple plot ----
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## ## Option allows for multispecies data
## ## Or the loop will only run once if you have single species data
## 
## for(i in unique(dataGroup_sf$scientific_name)){
## 
##   ## subset the data taking the track information for each unique species
##   temp_species <- dataGroup_sf %>% dplyr::filter(scientific_name == i)
## 
##   ## create new folder within current working directory where you will save data
##   ## first create the name of the species and the file path you need
##   ## also use gsub to replace spaces within character strings (words) with a "-"
##   species_name <- gsub(" ", "-", temp_species$scientific_name[1])
## 
##   ## print the name for checking
##   print(species_name)
## 
##   ## then create the new folder within current working directory
##   path_to_folder <- paste("./data-testing/tracking-data/",
##                           species_name,
##                           "/plots-all-tracks",
##                           sep="")
## 
##   ## print the file path name for checking
##   print(path_to_folder)
## 
##   ## Check if folder exists, and if it does not, then make a new folder
##     if (!file.exists(path_to_folder)) {
##     # If it does not exist, create a new folder
##     dir.create(path_to_folder)
##     print(paste("Created folder:", path_to_folder))
##     } else {
##     # do nothing, but let us know the folder exists already
##     print(paste("Folder already exists:", path_to_folder))
##     }
## 
## 
##   ## plot track information for each unique species
##   plot_alltracks <- ggplot() +
##   ## Use the world map data as the underlying basemap
##   geom_sf(data = worldmap, fill = "grey") +
##   ## Add the point data as transparent cyan circles
##   geom_point(data = dataGroup_sf, aes(x = Longitude, y = Latitude), color = "cyan", alpha = 0.5) +
##   ## plot the basemap again, but this time superimpose only the country borders over the point data
##   ## this is to help you see better which points might obviously be over land.
##   geom_sf(data = worldmap, fill = NA, color = "black") +
##   ## Set the bounding box to only include the point locations
##   coord_sf(xlim = range(dataGroup_sf$Longitude), ylim = range(dataGroup_sf$Latitude)) +
##   ## Customize the x and y axis labels
##   labs(x = "Longitude", y = "Latitude") +
##   ## add a title to the plot
##   ggtitle(paste(species_name, "\n",
##                 "points-all-animals",sep="")) +
##   theme(plot.title = element_text(hjust = 0.5))
## 
##   ## the plot
##   plot_alltracks
## 
##   ## save the plot
##   ggsave(paste(path_to_folder, "/",
##                species_name,
##                "_all-points.png",
##                sep = ""),
##          plot_alltracks,
##          ## when units in mm, then
##          width = 160, height = 160, dpi = 300, units = "mm")
## 
##   ## remove the temporary file at the end of each loop
##   rm(temp_species)
## }
## 

#' 
#' <br>
#' 
#' ## Visualise individual animal tracks
#' 
#' Once you have reviewed the overall status of the tracking data you collected, it can be worth assessing the tracks of individual animals. 
#' 
#' This can give you a better idea of the quality of the data for each individual.
#' 
#' > Visualising tracking data from individual animals can help you understand which data you might remove, or which data you might try and salvage.
#' 
#' Depending on the amount of data you have, you can often initially a perform a static exploration of tracks from each individual (i.e. a simple plot of tracks from each individual), followed by an interactive exploration of tracks from all individuals, or only data from those individuals where interactive exploration is deemed necessary.
#' 
#' Below, outlines options for visualising individual animal tracks.
#' 
#' ### Denote beginning and end of tracks for individual animals entire track
#' 
## ----track-yelk-beg-end-tracks, include=TRUE-----------------------------------------------------------

## reminder on data structure
head(data.frame(dataGroup_sf),2)
head(data.frame(dataGroup),2)


## add a column indicating start and end of tracks for each individual animal
dataGroup_sf <- dataGroup_sf %>% 
  group_by(ID_num) %>% 
  mutate(nlocs = 1:length(ID_num)) %>% 
  mutate(track_segment = if_else(nlocs <= 10, "track.start","track.journey")) %>% 
  ## note: if you have a track with less than 20 points, then you will overwrite 
  ## some of the previous data.
  mutate(track_segment = if_else(nlocs %in%   (length(ID_num)-9):(length(ID_num)),"track.end",track_segment)) %>%
  ## add a column indicating colour for start and end of tracks
  ## colours from: https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
  mutate(track_colour = if_else(nlocs <= 10, "#66c2a5","#8da0cb")) %>% 
  mutate(track_colour = if_else(nlocs %in% (length(ID_num)-9):(length(ID_num)),"#fc8d62",track_colour))


#' 
#' ### Save individual tracks as static plots
#' 
#' [A simple plot to look at all the point location data for each individual tracked]
#' 
## ----track-yelk-individual-plots, include = TRUE, eval = FALSE-----------------------------------------
## 
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## Save raw tracking data for each individual as a static plot ----
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## ## reminder on data structure
## head(data.frame(dataGroup_sf),2)
## 
## for(i in 1:max(dataGroup_sf$ID_num)){
## 
##   ## subset the data taking the track information for each unique bird tagged
##   temp_individual <- dataGroup_sf %>% dplyr::filter(ID_num == i)
## 
##   ## create new folder (if needed) within current working directory where you will save data
##   ## first create the name of the species and the file path you need
##   ## also use gsub to replace spaces within character strings (words) with a "-"
##   species_name <- gsub(" ", "-", temp_individual$scientific_name[1])
## 
##   ## print the name for checking
##   print(species_name)
## 
##   ## then create the new folder within current working directory
##   path_to_folder <- paste("./data-testing/tracking-data/",
##                           species_name,
##                           "/plots-individual-tracks",
##                           sep="")
## 
##   ## print the file path name for checking
##   print(path_to_folder)
## 
##   ## Check if folder exists, and if it does not, then make a new folder
##     if (!file.exists(path_to_folder)) {
##     # If it does not exist, create a new folder
##     dir.create(path_to_folder)
##     print(paste("Created folder:", path_to_folder))
##     } else {
##     # do nothing, but let us know the folder exists already
##     print(paste("Folder already exists:", path_to_folder))
##     }
## 
##   ## get animal id for naming plots
##   animal_id <- gsub(" ", "-", temp_individual$ID[1])
## 
## 
##   ## plot track information for each unique species
##   plot_individual_tracks <- ggplot() +
##   ## Use the world map data as the underlying basemap
##   geom_sf(data = worldmap, fill = "grey") +
##   ## Add the point data as transparent cyan circles
##   #geom_point(data = temp_individual, aes(x = lon_device, y = lat_device), color = "cyan", alpha = 0.5) +
## 
##   ## Add the point data - get colours from object
##   #geom_point(data = temp_individual, aes(x = lon_device, y = lat_device, color = track_colour), alpha = 0.5) +
## 
## 
##   ## Add the journey locations
##   geom_point(data = subset(temp_individual, track_segment == "track.journey"),
##              aes(x = Longitude, y = Latitude, color = track_colour), alpha = 0.5) +
##   ## Add the start locations
##   geom_point(data = subset(temp_individual, track_segment == "track.start"),
##              aes(x = Longitude, y = Latitude, color = track_colour), alpha = 0.5) +
##   ## Add the end locations
##   geom_point(data = subset(temp_individual, track_segment == "track.end"),
##              aes(x = Longitude, y = Latitude, color = track_colour), alpha = 0.5) +
## 
##   ## plot the basemap again, but this time superimpose only the country borders over the point data
##   ## this is to help you see better which points might obviously be over land.
##   geom_sf(data = worldmap, fill = NA, color = "black") +
##   ## Set the bounding box to only include the point locations
##   coord_sf(xlim = range(temp_individual$Longitude), ylim = range(temp_individual$Latitude)) +
##   ## Customize the x and y axis labels
##   labs(x = "Longitude", y = "Latitude") +
##   ## add a title to the plot
##   ggtitle(paste("points-individual:","\n",
##                 animal_id,
##                 sep="")) +
##   theme(plot.title = element_text(hjust = 0.5)) +
##   ## remove legend
##   theme(legend.position = "none")
## 
##   ## the plot
##   plot_individual_tracks
## 
##   ## save the plot
##   ggsave(paste(path_to_folder, "/",
##                animal_id,
##                "_points.png",
##                sep = ""),
##          plot_individual_tracks,
##          ## when units in mm, then
##          width = 160, height = 160, dpi = 300, units = "mm")
## 
##   ## print a loop progress message
##   print(paste("Loop ", i, " of ", max(dataGroup_sf$ID_num), sep = ""))
## 
##   ## remove the temporary file at the end of each loop
##   rm(temp_individual)
## }
## 

#' 
#' ### Save individual tracks as shapefiles
#' 
## ----track-yelk-individual-shapefile, include = TRUE, eval = FALSE-------------------------------------
## 
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ## Save raw tracking data for each individual as shapefile for viewing in GIS software ----
## ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## ## reminder on data structure
## head(data.frame(dataGroup_sf),2)
## 
## for(i in 1:max(dataGroup_sf$ID_num)){
## 
##   ## subset the data taking the track information for each unique bird tagged
##   temp_individual <- dataGroup_sf %>% dplyr::filter(ID_num == i)
## 
##   ## create new folder (if needed) within current working directory where you will save data
##   ## first create the name of the species and the file path you need
##   ## also use gsub to replace spaces within character strings (words) with a "-"
##   species_name <- gsub(" ", "-", temp_individual$scientific_name[1])
## 
##   ## print the name for checking
##   print(species_name)
## 
##   ## then create the new folder within current working directory
##   path_to_folder <- paste("./data-testing/tracking-data/",
##                           species_name,
##                           "/shapefiles-individual-tracks",
##                           sep="")
## 
##   ## print the file path name for checking
##   print(path_to_folder)
## 
##   ## Check if folder exists, and if it does not, then make a new folder
##     if (!file.exists(path_to_folder)) {
##     # If it does not exist, create a new folder
##     dir.create(path_to_folder)
##     print(paste("Created folder:", path_to_folder))
##     } else {
##     # do nothing, but let us know the folder exists already
##     print(paste("Folder already exists:", path_to_folder))
##     }
## 
##   ## write the spatial data. Label it by species and bird_id
##   st_write(temp_individual,
##            paste(path_to_folder, "/tracks-individual-animals",
##                  species_name, "_",
##                  temp_individual$ID[1],
##                  ".shp",
##                  sep = ""),
##            delete_layer = T)
## 
##   ## print a loop progress message
##   print(paste("Loop ", i, " of ", max(dataGroup_sf$ID_num), sep = ""))
## 
##   ## remove the temporary file at the end of each loop
##   rm(temp_individual)
## }
## 

#' 
#' 
#' ### When to remove or salvage data for a tracked individual
#' 
#' [Examples / details to be added]
#' 
#' > In some cases, an entire track may be worth disregarding or trying to salvage. However, it often might be the case that only certain trips from the entire period an animal was tracked may be worth salvaging / removing / considering further.
#' 
#' [Examples to be added]
#' 
#' ## Speedfilter data
#' 
#' To help you understand which tracks might need further processing, an initial cleaning of data using a speedfilter can be useful for tracking data.
#' 
#' -   McConnel Speed Filter (i.e. remove points based on unrealistic travel speeds)
#' 
## ----track-yelk-speed-all, include = TRUE--------------------------------------------------------------

## review
head(data.frame(dataGroup),2)

## total number of trips
length(unique(dataGroup$ID))

## start blank df
tracks_speed <- data.frame()

for(i in 1:length(unique(dataGroup$ID))){
  temp <- dataGroup %>% dplyr::filter(ID == unique(dataGroup$ID)[i])
  
  ## remove any erroneous locations due to speed use the McConnel Speed Filter 
  ##from the trip package
  trip_obj <- temp %>% 
    #group_by(tripID) %>% 
    dplyr::select(x = Latitude, 
                  y = Longitude, 
                  DateTime, 
                  everything()) %>% 
    trip()
  
  ## McConnel Speedilter -----
  ## apply speedfilter and creat data frame
  trip_obj$Filter <- speedfilter(trip_obj, max.speed = 100)  # speed in km/h
  trip_obj <- data.frame(trip_obj)
  head(trip_obj,2)
  
  ## How many locations were removed with speed filter?
  print(nrow(subset(trip_obj, trip_obj$Filter == F)))
  
  ## Keep only filtered coordinates - after checking dimensions of other outputs again
  trip_obj <- subset(trip_obj,trip_obj$Filter==TRUE)
  
  ## bind back onto dataframe
  tracks_speed <- rbind(tracks_speed, trip_obj)
  
  ## remove temporary items before next loop iteration
  rm(temp,trip_obj)
  
  ##
  print(i)

}

## review
head(tracks_speed,2)

## rename to align with example code
dataGroup <- tracks_speed %>% 
  mutate(Latitude = x,
         Longitude = y)


#' 
#' ## Review speed filtered data vs. original
#' 
#' [Consider plotting options from other scripts]
#' 
#' ## Splitting trips
#' 
#' Preparation
#' 
#' ## Define colony / origin of trips ----
#' 
#' This supports application of the tripSplit function
#' 
#' You must consider how you define your colony of origin for each of the tracked animals
#' 
## ----track-yelk-ind-col-locs, include = TRUE-----------------------------------------------------------

"OPTION 1: Same unique colony for all birds"
## example relates to a seabird during the breeding season, when the species
## is known to be a central place forager. Therefore, define the colony position
## based on the first longitude and latitude coordinates which SHOULD originate
## from the breeding colony if all birds tracke appropriately from the same colony
colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

##
head(colony)

## Or, manually specify a unique Lon / Lat
# colony <- dataGroup %>% 
#   summarise(
#     Longitude = 16.875879, 
#     Latitude  = 42.774843
#   )


"OPTION 2: Specify unique colony or unique nest per bird"
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


#' 
#' ### Are your colony locations where you expect them to be?
#' 
#' For tripSplit to work best, you want the colony locations to reflect where the colony actually is. If you did not clean or prepare your data appropriately, it may be the case that the colony locations are specified incorrectly.
#' 
#' If your colony locations are incorrectly placed, consider appropriate action.
#' 
## ----track-yelk-ind-col-locs-checking, include = TRUE--------------------------------------------------

## interactive plot - review where the individual colony location records
## were deemed to be.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = data.frame(dataGroup),
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot the colony locations from birds
  addCircleMarkers(data = data.frame(colony_nest),
                   radius = 5,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F) 

map


#' 
#' > CONSIDER: Based on your review of the data and knowledge of the study sytem, are the colony locations where you expect them to be? If not, consider appropriate action. For example, create a new dataframe with the relevant colony locations for each tracked individual.
#' 
#' [What other options exist to correct colony location data where required?]
#' 
#' ## Apply tripSplit()
#' 
#' **What does tripSplit do:** [update text from track2KBA manuscript]
#' 
#' **When not to apply tripSplit():** "If your data does not relate to a central place forager (CPF), OR
#' a time when an animal may be exhibiting central place foraging behaviours,
#' then skip this section and those relating to CPF data."
#' 
#' **How tripSplit() helps:**"This step is often very useful to help automate the removal of location points
#' on land, or near the vicinty of a colony. We don't want these extra points to bias
#' our interpretation of the data."
#' 
#' **General considerations when applying tripSplit():** "The user must define ecologically sensible parameters to help automate the tripSplitting process."
#' 
## ----track-yelk-ind-tripSplit, include = TRUE----------------------------------------------------------

## Input is a 'data.frame' of tracking data and the central-place location(s). 
## Output is a 'SpatialPointsDataFrame'.
trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony_nest, # define source location.
  innerBuff  = 3,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 1,      # hours - defines time an animal must have traveled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  nests = T,           # specify nests = T if using unique colony locations per animal,
  gapLimit = NULL, # The period of time between points (in days) to be considered too large to be a contiguous tracking event
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
table(trips$Returns)


#' 
#' ### Review of tripSplit() output
#' 
#' In the example above, we specified `rmNonTrip = F` so as not remove any points not deems as associated with a trip. I.e. the points typically lying within the `innerBuff` distance and for those where the animal travelled for less than `duration` specified,
#' 
#' Let's review the general points we are not considering as part of trips.
#' 
## ----track-yelk-ind-tripSplit-review, include = TRUE---------------------------------------------------

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


#' 
#' ### Understanding what is happening in tripSplit() further
#' 
#' Essentially, we are using a function that helps us bulk clean tracking data. The goal is to assign individual trips to multiple animals that have been tracked, and doing this in an automated way.
#' 
#' > Go back and change `innerBuff` and `duration` parameters in particular, and recreate the plot above showing the points not associated with a trip. See how changing the arguments impacts the likely data that will be removed for the analysis. You only want to remove (i.e. "clean up") the points that are most likely not associated with a trip.
#' 
#' 
#' ### Review the individual trips for each tracked animal after applying tripSplit()
#' 
#' A simple way to do this is with the `mapTrips` function.
#' 
#' The plots show an overview of individual trips per bird. Only data for the first 25 birds is shown.
#' 
## ----track-yelk-ind-mapTrips, include = TRUE-----------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## track2KBA::mapTrips() ----
## view data after splitting into trips ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## plot quick overview of trips recorded for individual birds (i.e. the plots show
## an overview of individual trips per bird). Only data for the first 25 birds is
## shown
mapTrips(trips = trips, colony = colony_nest)

## If you want to map the trips from the next 25 animals tracked, use the IDs argument
mapTrips(trips = trips, IDs = 26:50, colony = colony_nest)


#' 
#' ### Plot the individual trips for each tracked animal after applying tripSplit()
#' 
#' If, after reviewing the simplified plots of individual trips for each tracked animal using the `mapTrip()` function you are not satisfied, then you should explore the relative data further.
#' 
#' [One way of exploring the trips outputted for individually tracked animals would be to rapidly review summary plots for each trip, showing start, journey, and end points, where the point locations are also joined together with a line. Should consider inlcuding this plotting option. Perhaps also with option of specifying a vector of relative individuals.]
#' 
#' ## Keep points associated with complete trips only
#' 
#' > Keeping points associated with complete tracks only is the approach considred in the `track2KBA` online tutorial. But you may want to explore which tracks you are keeping or not.
#' 
#' > Here, the user should consider if too many individual trips have been removed.
#' i.e. if you tracked 30 birds and you estimated to have approximately 3 trips 
#' recoreded per bird, then you would have a total of 90 trips. But it's likely that
#' on some trips, that not the entire trip was recorded (for multiple reasons).
#' Therefore, you might expect to rather have about 83 trips recorded across all
#' birds because for 7 trips data might not have indicated birds returned to the colony.
#' If you had a very high proportion of trips that did not return to the colony, then
#' it's likely that you have defined the parameters incorrectly for tripSplit and you
#' should reconsider better ecologically based estimates for these parameters. There
#' is of course the chance that there are other issues with your data which would warrant
#' more detailed exploration.
#' 
## ----track-yelk-ind-keep-complete-trips, include = TRUE------------------------------------------------

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
trips.return.yes <- subset(trips, trips$Returns == "Yes" )
totalTripsYes <- data.frame(trips.return.yes) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)
## view summary result
table(totalTripsYes$Returns)

## Filter for trips that do not reutrn
trips.return.no <- subset(trips, trips$Returns == "No" )
totalTripsNo <- data.frame(trips.return.no) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)
## view summary result
table(totalTripsNo$Returns)

## CONSIDER and compare: total trips that returned vs. did not:
table(totalTripsYes$Returns)
table(totalTripsNo$Returns)

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


#' 
#' 
#' ### Review points associated with complete trips only
#' 
#' If you want to explore further the trips that were not considered to have returned, then use the object above, `trips.return.no`, to investigate the individual trips further. E.g. through individual plotting.
#' 
## ----track-yelk-ind-keep-complete-trips-choose, include = TRUE-----------------------------------------

## rename the object to align better with further components of the script
trips <- trips.return.yes


#' 
#' ## Keep trips with >5 points
#' 
## ----track-yelk-five-points, include = TRUE------------------------------------------------------------

## review
head(trips,2)

## create data frame and remove trips with <5 locations; as required for track2KBA analysis
trips_to_keep <- data.frame(trips) %>% 
  group_by(tripID) %>% 
  summarise(triplocs = n()) %>% 
  dplyr::filter(triplocs > 5)
  
##
trips_df <- data.frame(trips) %>% 
  dplyr::filter(tripID %in% trips_to_keep$tripID)

## Compare how many trips you removed
length(unique(trips$tripID))
length(unique(trips_df$tripID))


#' 
#' <br>
#' 
#' [explore / consider: impact of pre-filtering steps on individual trips being removed with <5 points]
#' 
#' ## tripSummary()
#' 
#' **Summarise the tracking data using the `tripSummary()` function:**
#' 
#' Useful for central place foragers
#' 
## ----track-yelk-tripSummary, include=TRUE--------------------------------------------------------------

## Before summarizing the trip movements, using tripSummary(). 
## First, we can filter out data from trips that did not return to the vicinity 
## of the colony (i.e. within returnBuff), so they don't skew the estimates.
## COMPLETED ABOVE
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tripSummary() ----
## Rough summary of tracking data for complete trips ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sumTrips <- tripSummary(trips = trips_df, colony = colony_nest, nests = T)

## Check you only have complete trips here (if that is what you are aiming for)
table(sumTrips$complete)

## filter for only complete trips if needed
#sumTrips  <- sumTrips  %>% dplyr::filter(complete= "complete trip")

## view output
head(sumTrips ,10)
## view unique individual ID
unique(sumTrips$ID)
## number of individuals with tracking data
length(unique(sumTrips$ID))
## number of unique trips from all individuals
length(unique(sumTrips$tripID))


#' 
#' ## Further summary of tripSummary
#' 
## ----yelk-data-summary-tripSummary, include = TRUE-----------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visual summary of data ----
"NOTE: These metrics are just summaries of summary data. Could technically determine
these values more accurately."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## all data
sumTrips  %>% mutate(group = "all_tracks") %>% 
  group_by(group) %>% 
  summarise(n_trips = n(),
            avg_triptime_h = round(mean(duration),2), 
            med_triptime_h = round(median(duration),2), 
            min_triptime_h = round(min(duration),2),
            max_triptime_h = round(max(duration),2),
            avg_totdist_km = round(mean(total_dist),2), 
            med_totdist_km = round(median(total_dist),2), 
            min_totdist_km = round(min(total_dist),2),
            max_totdist_km = round(max(total_dist),2),
            avg_maxdist_km = round(mean(max_dist),2), 
            med_maxdist_km = round(median(max_dist),2), 
            min_maxdist_km = round(min(max_dist),2),
            max_maxdist_km = round(max(max_dist),2)) %>% data.frame()


## by individual
bird_sum <- 
  sumTrips  %>% 
  group_by(ID) %>% 
  summarise(n_trips = n(),
            avg_triptime_h = round(mean(duration),2), 
            med_triptime_h = round(median(duration),2), 
            min_triptime_h = round(min(duration),2),
            max_triptime_h = round(max(duration),2),
            avg_totdist_km = round(mean(total_dist),2), 
            med_totdist_km = round(median(total_dist),2), 
            min_totdist_km = round(min(total_dist),2),
            max_totdist_km = round(max(total_dist),2),
            avg_maxdist_km = round(mean(max_dist),2), 
            med_maxdist_km = round(median(max_dist),2), 
            min_maxdist_km = round(min(max_dist),2),
            max_maxdist_km = round(max(max_dist),2)) %>% data.frame()

bird_sum

## save the data as an excel sheet
#write.xlsx(bird_sum, file = "./Data/track2KBA_output/TrackingData_Summary.xlsx", 
#           sheetName = "Sheet1", 
#           col.names = TRUE, row.names = T, append = FALSE)


## histograms - foraging trip duration
p1 <- ggplot(sumTrips , aes(duration)) +
  geom_histogram(colour = "darkgrey", fill = "cyan")+
  theme(
    axis.text=element_text(size=14, color="black"),
    axis.title=element_text(size=16),
    panel.background=element_rect(fill="white", colour="black")) +
  ylab("n tracks") + xlab("Duration (hours)")

p1


## total distance travelled from the colony
p2 <- ggplot(sumTrips , aes(total_dist)) +
  geom_histogram(colour = "darkgrey", fill = "cyan")+
  theme(
    axis.text=element_text(size=14, color="black"),
    axis.title=element_text(size=16),
    panel.background=element_rect(fill="white", colour="black")) +
  ylab("n tracks") + xlab("Total dist. (km)")

p2


## maximum distance travelled from the colony
p3 <- ggplot(sumTrips , aes(max_dist)) +
  geom_histogram(colour = "darkgrey", fill = "cyan")+
  theme(
    axis.text=element_text(size=14, color="black"),
    axis.title=element_text(size=16),
    panel.background=element_rect(fill="white", colour="black")) +
  ylab("n tracks") + xlab("Max dist. (km)")

p3


## save the plots
#ggsave(filename = paste("./Plots/HistSummary_Duration.png",sep=""), 
#       p1, 
#       dpi = 300, units = "mm", width = 180,height = 130)


#ggsave(filename = paste("./Plots/HistSummary_TotDist.png",sep=""), 
#       p2, 
#       dpi = 300, units = "mm", width = 180,height = 130)


#ggsave(filename = paste("./Plots/HistSummary_MaxDist.png",sep=""),
#       p3, 
#       dpi = 300, units = "mm", width = 180,height = 130)


#' 
#' 
#' ## Sampling interval assessment
#' 
#' To implement track2KBA fully, you need data approximating an even sampling interval
#' i.e. location points must be regularly spaced in time.
#' Determine how "gappy" the tracking data is (time intervals between location data)
#' This is an important step for almost all tracking data analyses.
#' If your data is not filtered / cleaned correctly, results may be spurious.
#' 
## ----track-yelk-ind-sampling-interval, include = TRUE--------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Check sampling interval ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## data for summarising
head(data.frame(trips_df),2)

## Determine difference between consecutive timestamps 
## (NB: consecutive order of timestamps is critical here!)
## Doing this by tripID, not individual ID - change the group_by argument if needed
timeDiff <- trips_df %>% 
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


#' 
#' ### Sampling interval review
#' 
#' > Consider whether the sampling interval of your tracking data is appropriate
#'   for formally running the track2KBA functions. Remember, the time differences
#'   between each of your location points should be equal (or close enough to equal) 
#'   across all location points and individuals tracked. If the time difference
#'   between location points is not equal, the outputs you generate from track2KBA
#'   will not be valid because the underlying kernel density analysis implemented
#'   within the track2KBA functions will be invalid 
#'   (because you need points evenly spaced in time for this analysis to be valid).
#' 
#' Therefore, review the summary of your recorded sampling interval data:
#' 
## ----track-yelk-ind-sampling-interval-review, include = TRUE-------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Review sampling interval ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Average sampling interval of all data
## median of median time gaps in minutes
median(SummaryTimeDiff$median_timegap_secs)/60

## Sort data by maximum time gap first - then view.
## Consider if you have any outlier trips with massively different time gaps.
SummaryTimeDiff %>% arrange(-max_timegap_secs) %>% head(10)

## simple histogram 
hist(SummaryTimeDiff$max_timegap_days)


#' 
#' If you have trips where the maximum time gap in seconds (`max_timegap_secs`) is 
#' extremely different to the *median of median time gaps* in minutes, then it's likely you need to consider why this is the case for those trips.
#' - Did you specify tripSplit parameters incorrectly?
#' - Does the data require further cleaning in some other way?
#' - What else to consider
#' 
#' > NOTE: you should be aware of what the original pre-programmed sampling interval was. How do you results compare to this interval?
#' 
#' ["CONSIDER: What is the best way to provide advice on appropriate time gap selection
#' for interpolation based on results above vs. comparison with intended sampling interval."]
#' 
#' ## Remove points near the colony
#' 
#' **Key considerations related to identifying IBAs / KBAs:** when identifying an IBA or KBA for seabirds using the track2KBA protocol, you effectively need information about the source population (typically the colony) and distribution data (tracking data). This means that not only can you identify a pelagic site from tracking data, but you can also consider an IBA/KBA for the colony itself and a possible at-sea buffer around the colony.
#' 
#' <br>
#' 
#' - Because you should typically consider identifying the at-sea buffer around the colony in addition to any potential pelagic / offshore sites supported by the tracking data, you can remove location points from the data within a suitable buffer distance.
#' 
#' <br>
#' 
#' - Removing trip start and end points near colony for the interpolation, especially if using more advanced methods like CRAWL, ALSO supports that the speed filters can focus on points at sea when you estimate birds should be moving, as opposed to trying to deal with sections of trips when the bird is actually stationary on land.
#' 
#' > CONSIDER: Appropriate choice of buffer distance in relation to identifying at-sea buffer IBAs for seabirds.
#' 
#' [consider better name for at-sea buffer IBAs]
#' 
#' [Should we apply relative at-sea buffer at the tripSplit step or later on. Later seems to make sense to me. But this also means we might remove what are considered invidual trips if we increase the buffer size.]
#' 
## ----track-yelk-ind-buffer-points, include = TRUE------------------------------------------------------

warning("Must update GitHub tutorial which specifies distance incorrectly")
## remove trip start and end points near colony - distance is in m (unlike innerBuff where distance was in km)
tracks <- trips_df[trips_df$ColDist > 5*1000, ]

##
length(unique(tracks$tripID))

##
head(tracks,2)


#' 
#' ## Metadata before interpolation
#' 
#' Because the interpolation functions often remove some of the additional metadata which are useful for later steps in the track2kba protocol, we save a relevant version of the metadata here.
#' 
## ----track-yelk-metadata-save, include=TRUE------------------------------------------------------------

tracks_meta <- tracks %>% 
  dplyr::select(scientific_name,
                common_name,
                site_name,
                colony_name,
                bird_id = ID,
                trip_id = tripID,
                age,
                sex,
                breed_stage,
                breed_status) %>% 
  group_by(trip_id) %>% 
  slice(1)

## Compare
length(unique(tracks$tripID))
length(unique(tracks_meta$trip_id))


#' 
#' 
#' 
#' ## Interpolation
#' 
#' If you have gaps in your tracking data, you need to fill these gaps for the purpose of the track2KBA protocol. It's likely you will need to do this for many other tracking data analyses.
#' 
#' Broadly speaking, there are two key ways to fill the gaps in your tracking data, a process known as interpolation.
#' 
#' These two ways include:
#' - Simpler linear interpolation
#' - More advanced interpolation options that try account for where the animal could have moved (e.g. CRAWL)
#' 
#' > Typically, for flying seabirds, where gaps in tracking data are less likely because birds do not typically dive underwater for durations as long as diving seabirds, linear interpolation should serve as a suitable starting point.
#' 
#' > More advanced interpolation methods may be required for non-flying seabirds, or other diving marine predators.
#' 
#' ### Linear interpolation: all animals
#' 
## ----track-yelk-interpolation-linear-all, include = TRUE-----------------------------------------------

## review input data
head(tracks,2)
length(unique(tracks$tripID))

## start blank df
tracks_interp_df <- data.frame()

for(i in 1:length(unique(tracks$tripID))){
  
  temp <- tracks %>% dplyr::filter(tripID == unique(tracks$tripID)[i])
  
  ## Linear interpolation -----
  ## Apply linear interpolation step to speed filtered only data
  
  ## create ltraj object
  trip_lt <- as.ltraj(xy = bind_cols(x = temp$Longitude, 
                                     y = temp$Latitude),
                      date = temp$DateTime,
                      id = temp$tripID)
  
  ## Linearly interpolate/re-sample tracks every 30 minutes (specified in seconds)
  trip_interp <- redisltraj(trip_lt, 1800, type="time")
  head(trip_interp)
  
  ## convert back into format for track2KBA - dataframe for now
  trip_interp <- ld(trip_interp) %>% 
    dplyr::mutate(Longitude = x,
                  Latitude = y)
  
  ## bind back onto dataframe
  tracks_interp_df <- rbind(tracks_interp_df, trip_interp)
  
  ## remove temporary items before next loop iteration
  rm(temp,trip_lt)
  
  ##
  print(i)

}

## review it worked by checking total number of unique trips and comparing to original
length(unique(tracks$tripID))
length(unique(tracks_interp_df$id))
## compare total number of points
nrow(tracks)
nrow(tracks_interp_df)


#' 
#' #### Review interpolation for all animals
#' 
## ----track-yelk-interpolation-all-review, include = TRUE-----------------------------------------------

## Review interpolation for all animals
head(tracks_interp_df,2)


#' 
#' ## Bind metadata back after interpolation
#' 
#' Because we lost the useful metadata after the interpolation step, bind this data back onto the outputted data following interpolation
#' 
## ----tracks-yelk-meta-bind, include=TRUE---------------------------------------------------------------

## First create columns with same names
tracks_interp_df <- tracks_interp_df %>% 
  rename(trip_id = id)

## Now bind the metadataback
dim(tracks_interp_df)
tracks_interp_df <- left_join(tracks_interp_df, 
                               tracks_meta,
                               by = "trip_id")

##
dim(tracks_interp_df)



#' 
#' 
#' 
#' ## projectTracks()
#' 
#' > Here is where you could also begin for non-central place foraging data
#' 
#' track2KBA uses Kernel Density Estimation (KDE) to produce space use estimates for each individual track. In order for these to be accurate, we need to transform the tracking data to an equal-area projection.
#' 
#' > NOTE: when you have several trips per individual, you typically run track2KBA at the level of the individual, as opposed to the trip.
#' 
#' [CONSIDER APPROPRIATE GUIDANCE IN RELATION TO indEffectTest]
#' 
## ----track-yelk-projecTracks, include=TRUE-------------------------------------------------------------

## review current data
head(data.frame(tracks_interp_df),2)

## Format the data BACK INTO key data fields to the standard used in track2KBA
dataGroup_interp <- formatFields(
  ## your input data.frame or tibble
  dataGroup = tracks_interp_df, 
  ## ID of the animal you tracked
  fieldID   = "bird_id", 
  fieldDateTime = "date",
  ## longitude of device
  fieldLon  = "Longitude", 
  ## latitude of device
  fieldLat  = "Latitude"
)

## Check output. Output is a data.frame
head(dataGroup_interp,2)

## run the function
tracks_interp <- projectTracks(dataGroup = dataGroup_interp, projType = 'azim', custom=TRUE )


#' 
#' ##findScale()
#' 
#' `findScale()` provides options for setting the all-important smoothing parameter in the KDE.
#' 
#' `findScale()` calculates candidate smoothing parameter values using different methods.
#' 
#' Choosing the 'optimal' smoothing parameter is critical. See GitHub page. Consider what further advice we can give to users regarding choice of smoothing parameter?
#' 
## ----track-yelk-findScale, include=TRUE----------------------------------------------------------------

hVals <- findScale(
  tracks   = tracks_interp,
  scaleARS = TRUE,
  sumTrips = sumTrips)

## Review output
hVals

## must choose between one of three smoothing parameters for further analyses
## smoothing parameter is distance in km. Read more in supporting documents.

## Review each outputted smoothing parameter option
hVals$mag # affected by range of animal movement. Only works for central place foragers.
hVals$href # sort of represents quality of data
hVals$scaleARS # affected by quality of data and subsequent ability to determine scale at which animal interacts with environment. Learn more about First Passage Time analysis


#' 
#' ## estSpaceUse()
#' 
## ----track-yelk-estSpaceUse, include=TRUE--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## estSpaceUse() ----
## Produce KDEs for each individual
## Consider the step above where we removed points within a buffer distance
## from the colony. Consider appropriate action depending on data.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## review input again
head(data.frame(tracks_interp),2)

## produce the KDEs for each individual
## NOTE: The grid cell size (i.e., grid resolution) should be less than the 
## selected h value, otherwise the entire space use area of the animal may be 
## encompassed in few cells

KDE <- estSpaceUse(
  tracks = tracks_interp, 
  scale = hVals$mag, 
  levelUD = 50, 
  polyOut = TRUE
)

## use the simple mapping function to get an overview of UDs for individuals
mapKDE(KDE = KDE$UDPolygons, colony = colony)


#' 
#' ## Review smoothing
#' 
#' After applying estSpaceUse and viewing plot with mapKDE, at this step we should 
#' verify that the smoothing parameter value we selected is producing reasonable 
#' space use estimates, given what we know about our study animals. Are the core 
#' areas much larger than expected? Much smaller? If so, consider using a different 
#' value for the `scale` parameter.
#' 
## ----track-yelk-review-smoothing, include=TRUE---------------------------------------------------------

## specify an individual
## 14
p = 4

## convert to sf object 
trips_sf_IndBird <- st_as_sf(tracks_interp) %>% 
  dplyr::filter(ID == unique(tracks_interp$ID)[p])

## get the UD for a single individual 
ud_sf_IndBird <- KDE$UDPolygons %>% 
  dplyr::filter(id == trips_sf_IndBird$ID[1]) %>% 
  st_transform(.,crs = st_crs(tracks_interp))

## Plot OVERALL data again for first single individual
plot(st_geometry(trips_sf_IndBird), 
     cex = 0.5, 
     pch = 1)

## and add the UD to the plot
plot(st_geometry(ud_sf_IndBird),add=T, border = "blue")

warning("Assess whether your selected smoothing parameter has resulted in sensible
        Utilisation Distributions.")

warning("Can we use the above to consider some form of test to guide smoothing
        parameter choice?.")


#' 
#' ## repAssess()
#' 
#' Estimate how representative this sample of animals is of the population.
#' 
#' >NOTE: iterations should be set to 100 at a minimum when running the script officially.
#' 
## ----track-yelk-repAssess, include = TRUE--------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## repAssess() ----
## estimate how representative this sample of animals is of the population.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"NOTE: See iterations"
repr <- repAssess(
  tracks    = tracks_interp, 
  KDE       = KDE$KDE.Surface,
  levelUD   = 50,
  iteration = 1, ## iterations should be set to 100 at a minimum when running the script officially
  bootTable = FALSE)


#' 
#' ## findSite()
#' 
## ----findSite, include = TRUE--------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## findSite() ----
## using findSite we can identify areas where animals are overlapping in space 
## and delineate sites that meet some criteria of importance.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## indicate the population size of your source populaiton. e.g. the population size
## of the colony from which you tracked birds. For KBA identification, this estimate
## should be in Mature Individuals.
## I.e. for seabird colonies: breeding pairs * 2 = mature individuals
SourcePopulationSize = 1200

##
sf_use_s2(FALSE)

## findSite function
Site_PolyTrue <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = SourcePopulationSize,     
  polyOut = T
)

## review outputs
dim(Site_PolyTrue)
Site_PolyTrue$potentialSite

## plot option
Sitemap_PolyTrue <- mapSite(Site_PolyTrue, colony = colony)

## review output object
Sitemap_PolyTrue

## If in findSite we instead specify polyOut=FALSE, our output will be a spatial
## grid of animal densities, with each cell representing the estimated number, or
## percentage of animals using that area. So this output is independent of the
## representativness-based importance threshold. i.e. the output indicates
## only the areas used by more or less individuals that you tracked, it does not
## give you a polygon that you would necessarily assess against IBA / KBA criteria.
## The output also does not use the representatives measure to estimate the OVERALL
## number of individuals that are likely using certain areas when you specify the
## popSize of your source population.

# ## findSite with polyOut=FALSE
# Site_PolyFalse <- findSite(
#   KDE = KDE$KDE.Surface,
#   represent = repr$out,
#   levelUD = 50,
#   popSize = SourcePopulationSize, 
#   polyOut = FALSE
# )
# 
# ## review outputs
# dim(Site_PolyFalse)
# max(Site_PolyFalse@data$N_IND)
# max(Site_PolyFalse@data$N_animals)
# head(unique(Site_PolyFalse@data$ID_IND))
# 
# ## simple plot option
# Sitemap_PolyFalse <- mapSite(Site_PolyFalse, colony = colony)
# 
# ## review output object
# Sitemap_PolyFalse


#' 
#' ## findSite() further explanation
#' 
#' By default, findSite sets the threshold of site importance for the source population based on the degree of tracking sample representativeness following Lascelles et al. (2016): i.e., samples that are >90%, 80-89%, 70-79%, and <70% representative set the threshold for delineating a site as important for the local source population at 10%, 12.5%, 25%, and 50%, respectively)
#' 
#' For samples with representativeness of >90%, the area that is considered
#' potentially suitable for assessment against relevant criteria. eg. KBAs,
#' is only that area used by 10% or MORE of the individuals from the source
#' population. Any area used by 10% or LESS of the source population is not
#' thought to sufficiently represent where the source population may be found,
#' and hence is excluded from further assessment.
#' 
#' The threshold rules were agreed upon by expert consensus.
#' 
#' Note, when representativeness = 90% a bigger overall area in red is defined
#' as potentially suitable for assessment against relevant criteria.
#' when representativeness = 75% a SMALLER overall area in red is defined
#' as potentially suitable for assessment against relevant criteria.
#' This is because with a lower representativeness score, track2KBA provides
#' a more conservative estimate of which areas can definitively be considered
#' core areas regularly used by the sampled population.
#' NOTE also: the total number of individuals estimated using the area is reduced
#' when representativeness is lower. ie. basically, a lower total number of
#' individuals are estimated to be using areas when we are more unsure about how
#' well the data likely represents the source population.
#' 
#' ## IBA assessment data preparation
#' 
#' NOTE, when polyOut = TRUE for function findSite(), the output includes a simple features object with polygons (represented by each row of data) 
#' that represent the number of overlapping UDs for individuals (N_IND), and the associated estimate of abundance (N_animals) within each of those polygons scaled according to the popSize and representativeness score.
#' 
#' In track2KBA we don't offer much advice about how to use the final outputs
#' from findSite() for assessing data against various criteria, such as the IBA
#' or KBA criteria. The text on GitHub (as of 18 Oct 2022), notes: 
#' 
#' 'Then, we can combine all the polygons within the 'potentialSite' area, 
#' and use, for example, the maximum number of individuals present in that 
#' area to assess whether it may merit identification as a Key Biodiversity 
#' Area according to the KBA standard.', 
#' 
#' BUT: This does text does not describe how to deal with all the individual
#' polygons that were representative, and may be quite separate from each
#' other in space!
#' 
## ----yelk-data-iba-polygons, include=TRUE--------------------------------------------------------------

## OPTION 2: Assess each representative polygon of data, where potentialSite==TRUE, 
## against relevant criteria. 

#Site_PolyTrue <- Site_PolyTrue[-29,]


## To assess each polygon we first need to summarise the data further.
## First, we must determine how many and where each unique polygon is.
Site.polygons <- Site_PolyTrue %>% 
  ## filter to only include potentialSite's
  dplyr::filter(potentialSite==TRUE) %>%
  ## union to create multipart polygon
  st_union(.) %>% 
  ## cast to create single part polygon object
  st_cast(., "POLYGON") %>% 
  st_as_sf() %>% 
  ## add unique id for each polygon
  mutate(poly.id = 1:nrow(.))

## Plot to review
p <- ggplot() +
  geom_sf(data = Site.polygons, aes(fill = as.factor(poly.id))) +
  #scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  scale_color_viridis(discrete = TRUE) +
  guides(fill=guide_legend(title='Polygon')) +
  ## add the polygon labels
  geom_sf_text(data = Site.polygons, aes(label = poly.id), colour = "black", size = 5) +
  ## remove x y labels
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  # Title
  ggtitle(paste("Polygons for assessment against IBA criteria",sep=""))+
  theme(plot.title = element_text(hjust = 0.5))

p

## Second, considering that for each representative polygon, the maximum number
## of animals could technically spread across anywhere in the site, we need
## to get the maximum number of animals for each site.

## Merge the new unique polygons with the Site info and get overlapping data
sf::sf_use_s2(FALSE) # run this if issues with st_intersection: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
## Using an intersection, find all the data from Site object (the findSite output) 
## that intersects with Site.polygons object (the unique polygons)
Site.polygons.data <- st_intersection(Site.polygons, Site_PolyTrue)

## Summarise the data for each unique polygon.
Site.polygons.data.summary <- Site.polygons.data %>% 
  group_by(poly.id) %>% 
  summarise(N_animals_max = round(max(N_animals),0),
            N_IND_max  = round(max(N_IND ),0)) %>% 
  st_drop_geometry() %>% 
  data.frame()

## bind this data back onto the spatial object
Site.polygons <- left_join(Site.polygons, 
                           Site.polygons.data.summary,
                           by = "poly.id")




#' 
#' ##
#' 
## ----yelk-data-iba-finalpolygons-----------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Assign metadata for IBA Criteria assessment () ----
"CURRENTLY ONLY BASED ON ROW BY ROW ASSESSMENT AGAINST IBA CRITERIA"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Now, include the necessary columns of metadata for assessing sites against IBA criteria
warning("For assessing multiple datagroups via thet track2KBA workflow, you could
      create an overall metadata table and call the relevant metadata each time instead
      of manually inputting data here")

metData <- data.frame(SiteName = "Name of site for source population",
                      RegionName = "Name of broader region source population is found",
                      Site_LatMid = colony$Latitude, # central coordinate of lat for site of source population
                      Site_LonMid = colony$Longitude, # central coordinate of lon for site of source population
                      SpeciesComNam = "Masked Booby",
                      Scientific.name = "Sula dactylatra",
                      StartSurvey = "Start year of survey / estimate",
                      EndSurvey = "End year of survey / estimate",
                      Season = "Broad seasonal distribution",
                      SeasonSpecific = "Specific period of year for species of relevance")

## bind these onto spatial data for assessment against IBA criteria
Site <- cbind(Site, metData)

## save for further analyses
save(Site, file = "C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_output_for_criteria_assessment.Rdata")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save key outputs for plotting in alternate GIS (QGIS, ArcGIS Pro, etc) ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## save overlapping polygons as shapefile for plotting. i.e. the output of findSite()
## When plotting in Arc, suggested option is display graduated colours with
## method set to equal interval. Then, remove borders of individual polygons
## This will give the "raster-like" appearance.
## Then make layer slightly transparent if need be.
Site %>% 
  filter(.data$N_animals > 0) %>% 
  st_write(., "C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_findSiteOutputWhen_polyOut_TRUE.shp", delete_layer=T)



#' 
