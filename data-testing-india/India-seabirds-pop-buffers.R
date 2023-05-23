## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Purpose of script ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Building test data for India - Seaward extension application

## Data sources
## IBA spatial data comes from the main database - it was prepared manually in ArcGIS
## IBA tabular data comes from the main database - it was prepared by Jono Handley with steps specific to sites with seabird trigger species
## Buffer distances - were provided from AVISTEP project data

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(xlsx)
## leaflet package for interactive maps in R
library(leaflet)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## What is my working directory ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## review
getwd()

## copy here
"C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/PROJECTS/Marine Toolkit/GitHub_MarineToolkit/MarMeCo-Toolkit-R-Beta"

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## read in relevant data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iba.confirmed <- st_read("data-testing-india\\India-shapefiles\\IBA_Polygons_Seabirds_India_Coastal_Confirmed.shp")
iba.proposed <- st_read("data-testing-india\\India-shapefiles\\IBA_Polygons_Seabirds_India_Coastal_Proposed.shp")
iba.tabular <-  read.csv("data-testing-india\\IBA_PopulationData_Seabirds_India_CoastalMarineSites_Buffer.csv")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## quick check of input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(iba.tabular,2)
plot(st_geometry(iba.confirmed))
plot(st_geometry(iba.proposed))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## amend input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iba.tabular <- iba.tabular %>% 
  mutate(Latitude = SitLat,
         Longitude = SitLong,
         Row.key.original.data = 1:nrow(.))
     
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Inspect where colony data overlaps existing IBAs ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## interactive plot
map.alldata <- leaflet() %>% ## start leaflet plot
  addTiles() %>% 
  ## plot the layers Note: leaflet automatically finds lon / lat colonies
  ## CONFIMED IBAs
  addPolygons(data = iba.confirmed,
              ## size of points
              #radius = 3,
              ## colour of points
              label = iba.confirmed$SitRecID,     
              fillColor = "yellow",
              color = "green",
              #opacity =1,
              ## transparency of points
              fillOpacity = 0.5, 
              ## set stroke = F to remove borders around points
              stroke = T,
              labelOptions = labelOptions(noHide = F)) %>% 
  ## plot the layers Note: leaflet automatically finds lon / lat colonies
  ## PROPOSED IBAs
  addPolygons(data = iba.proposed,
              ## size of points
              #radius = 3,
              ## colour of points
              label = iba.proposed$SitRecID,     
              fillColor = "orange",
              color = "red",
              #opacity =1,
              ## transparency of points
              fillOpacity = 0.5, 
              ## set stroke = F to remove borders around points
              stroke = T,
              labelOptions = labelOptions(noHide = F)) %>% 
  ## plot the layers Note: leaflet automatically finds lon / lat colonies
  ## Colonies
  addCircles(data = iba.tabular,
             ## size of points
             #radius = 3,
             ## colour of points
             label = iba.tabular$SitRecID,     
             fillColor = "black",
             color = "purple",
             #opacity =1,
             ## transparency of points
             fillOpacity = 0.5, 
             ## set stroke = F to remove borders around points
             stroke = T,
             labelOptions = labelOptions(noHide = F)) %>% 
  # Add the legend to the map
  addLegend(position = "bottomright",
    colors = c("green", "red", "purple"),
    labels = c("Confirmed IBA", "Proposed IBA", "Colony"),
    opacity = 0.5,
    title = "Legend"
  )

## generate the plot
map.alldata

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Consider ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"SOME locations do not seem to line up with existing IBAs. Given the species information
comes from the IBAs directly, it could be the case that the site coordinates were specified
incorrectly in BirdLife's database."

"SOLUTION: Will need to triple check coordinates and potentially manually amend."

"NOTE: Techincally what we are considering colony coorindates here, are (or should at least)
actually representing the centroid of an IBA site."

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Determine the centroids of the current IBA network bind to tabular data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## confirmed
iba.confirmed.centroids <- iba.confirmed %>% 
  mutate(Latitude.centroid = st_coordinates(st_centroid(iba.confirmed))[,"Y"],
         Longitude.centroid = st_coordinates(st_centroid(iba.confirmed))[, "X"]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(SitRecID,
                Latitude.centroid,
                Longitude.centroid,
                IbaStatus)
## review
head(iba.confirmed.centroids)

## proposed
iba.proposed.centroids <- iba.proposed %>% 
  mutate(Latitude.centroid = st_coordinates(st_centroid(iba.proposed))[,"Y"],
         Longitude.centroid = st_coordinates(st_centroid(iba.proposed))[, "X"]) %>% 
  st_drop_geometry() %>% 
  dplyr::select(SitRecID,
                Latitude.centroid,
                Longitude.centroid,
                IbaStatus)
## review
head(iba.proposed.centroids)

## merge 
iba.centroids <- rbind(iba.confirmed.centroids,
                       iba.proposed.centroids)

## and bind to tabular data
iba.tabular <- left_join(iba.tabular, iba.centroids, by = "SitRecID")

## review
head(data.frame(iba.tabular),2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot map again with revised colony locations ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Colonies
map.alldata %>% 
  addCircles(data = iba.tabular,
             ## size of points
             #radius = 3,
             ## colour of points
             lng = iba.tabular$Longitude.centroid,
             lat = iba.tabular$Latitude.centroid,
             label = iba.tabular$SitRecID,     
             fillColor = "black",
             color = "cyan",
             #opacity =1,
             ## transparency of points
             fillOpacity = 0.5, 
             ## set stroke = F to remove borders around points
             stroke = T,
             labelOptions = labelOptions(noHide = F)) %>% 
    # Add the legend to the map
    addLegend(position = "bottomright",
              colors = c("cyan"),
              labels = c("Colony.centroid"),
              opacity = 0.5,
              #title = "Legend"
              layerId = 1,
    )


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Simplify tabular data for seaward extension excercise ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sea.ext.tabular <- iba.tabular %>% 
  dplyr::select(SitRecID,
                IBA_International = SitInternational,
                scientific = SciName4Analysis,
                common = ComName4Analysis,
                IBA_StatusForAssessment = StatusForAssessment,
                Latitude.IBA.centroid = Latitude.centroid,
                Longitude.IBA.centroid = Longitude.centroid,
                Site_SurveyYear_Min = SpcPopYear,
                Site_SurveyYear_Max = SpcPopYearEnd,
                Pop_Min = SpcPopMin,
                Pop_Max = SpcPopMax,
                Pop_Units = UniDesc,
                Abundance_Description = AbuDesc,
                MeanOfMaxDistance_Breeding,
                Row.key.original.data)

## review
head(sea.ext.tabular,2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Only keep records with buffer available ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##species needing further research because we currently have no buffer distance
## easily available for them
species.further.research <- sea.ext.tabular %>% 
  dplyr::filter(MeanOfMaxDistance_Breeding %in% c("not-available-in-Juan-data",  "species-not-in-Juan-data")) %>% 
  dplyr::select(scientific,
                common) %>% 
  group_by(scientific) %>% 
  slice(1)
##
species.further.research

## species with buffer distance in Juan data (following BirdLife AVISTEP project)
species.with.buffer <- sea.ext.tabular %>% 
  dplyr::filter(!MeanOfMaxDistance_Breeding %in% c("not-available-in-Juan-data",  "species-not-in-Juan-data")) %>% 
  dplyr::select(scientific,
                common) %>% 
  group_by(scientific) %>% 
  slice(1)
##
species.with.buffer

## identify data with buffer or not
table(sea.ext.tabular$MeanOfMaxDistance_Breeding)

sea.ext.tabular$Buffer_available <- ifelse(!sea.ext.tabular$MeanOfMaxDistance_Breeding %in% c("not-available-in-Juan-data",  "species-not-in-Juan-data"),
                                           "buffer_yes",
                                           "buffer_no")

table(sea.ext.tabular$MeanOfMaxDistance_Breeding)
table(sea.ext.tabular$Buffer_available)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Convert input data estimates into mature individuals for IBA assessment ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##
table(sea.ext.tabular$Pop_Units)

## ensure all columns are numeric
sea.ext.tabular$Pop_Min  <- as.numeric(sea.ext.tabular$Pop_Min )
sea.ext.tabular$Pop_Max <- as.numeric(sea.ext.tabular$Pop_Max)

"It seems, where information is available, that all units are in individuals.
So I will only convert from individuals to mature individuals. Where we don't
have units, we also don't generally have an abundance estimate as those records
typically relate to presence only records."


"Convert according to BirdLife International standard conversions"
## Convert
sea.ext.tabular$Pop_Min_MatInds  <- sea.ext.tabular$Pop_Min * 2/3
sea.ext.tabular$Pop_Max_MatInds  <- sea.ext.tabular$Pop_Max * 2/3

## review
head(sea.ext.tabular,2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get a best estimate for each individual colony ----
"Not all records have a min and max, some only have a min"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Where colonies only have 1 record, check that it always relates to min - manual check
sea.ext.tabular <- sea.ext.tabular %>% 
  mutate(min_yes = ifelse(Pop_Min_MatInds > 0, 1, 0),
         max_yes = ifelse(Pop_Max_MatInds > 0, 1, 0)) %>% 
  mutate(min_max = min_yes + max_yes)

sea.ext.tabular

## where there is only a min estimate, update the max estimate to also be this this value
sea.ext.tabular$Pop_Max_MatInds <- ifelse(sea.ext.tabular$max_yes == 0,
                                          sea.ext.tabular$Pop_Min_MatInds,
                                          sea.ext.tabular$Pop_Max_MatInds)


## Now determine "best" estimate for record
sea.ext.tabular$Pop_Best_MatInds <- round((sea.ext.tabular$Pop_Max_MatInds + sea.ext.tabular$Pop_Min_MatInds) / 2,0)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Extract key columns of data for seaward extension analysis
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## review
head(sea.ext.tabular,2)

## Get the columns of relevant info
sea.ext.analysis.data <- sea.ext.tabular %>% 
  mutate(Pop.units.available = ifelse(Pop_Units == "individuals","yes","no")) %>% 
  dplyr::select(SitRecID,
                IBA_International, 
                scientific,
                common,
                IBA_StatusForAssessment,
                Latitude = Latitude.IBA.centroid,
                Longitude = Longitude.IBA.centroid,
                Pop_Best_MatInds,
                Pop.units.available,
                Buffer_available,
                MeanOfMaxDistance_Breeding) %>% 
  dplyr::filter(Buffer_available == "buffer_yes") %>% 
  mutate(Note = "Species data produced from existing IBA data in BLI WBDB. Updated colony specific records will enhance seaward extension analysis and assessment of sites as potential IBAs")

## Final species
unique(sea.ext.analysis.data$common)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Extract country shapefile data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## key countries in region of interest
c.IND <- readRDS("data-testing-india\\gadm36_IND_0_sf.rds")
c.LKA <- readRDS("data-testing-india\\gadm36_LKA_0_sf.rds")

## bind them together for basemap
base.map <- rbind(c.IND, c.LKA)

## plot to check
plot(st_geometry(base.map))

## plot colony locations
table(sea.ext.analysis.data$IBA_International)
sea.ext.analysis.data.sf <-  sea.ext.analysis.data %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) ## wgs84 <- st_crs("EPSG:4326")
plot(st_geometry(sea.ext.analysis.data.sf), add = T, col = "red")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save final data for analysis
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save(sea.ext.analysis.data, file="data-testing-india\\India-data-4-seaward-extension.Rdata")
save(base.map, file="data-testing-india\\India-base-map.Rdata")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Test data against IBA criteria
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## load the function
source("data-testing-india\\Function_IBA_IBAcriteriaAssessment_India.R")

iba.df <- Assess_IBA_criteria(input.data = sea.ext.analysis.data,
                              scientific.column = "scientific",
                              Best.estimate.mature.individuals = "Pop_Best_MatInds")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save results to compare
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.csv(sea.ext.analysis.data,"data-testing-india\\India-data-4-seaward-extension.csv", row.names = F)
write.csv(iba.df,"data-testing-india\\India-data-4-seaward-extension-IBA-assessed.csv", row.names = F)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Test criteria again by manipulating some data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(data.frame(new.df),1)
data.fudge <- sea.ext.analysis.data[1,]
data.fudge$Pop_Best_MatInds = 75000

test.df <- rbind(sea.ext.analysis.data,
                 data.fudge)

test.iba <- Assess_IBA_criteria(input.data = test.df,
                              scientific.column = "scientific",
                              Best.estimate.mature.individuals = "Pop_Best_MatInds")

tail(data.frame(test.iba),1)
