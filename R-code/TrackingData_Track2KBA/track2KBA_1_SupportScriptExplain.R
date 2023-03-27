## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## This script uses the example data for Masked Boobies associated with the
## track2KBA GitHub account: https://github.com/BirdLifeInternational/track2kba
## Supported by the manuscript Beal et al. 2021

## The script aims to further explain what the functions are doing and acts as a
## guide for inexperienced users. Users should consult this script in conjunction
## with the 2021 manuscript, supplementary material and GitHub account.

## While track2KBA is designed to allow minimal input from the user, users should
## understand the consequence of choices to input parameters while using the functions
## provided within track2KBA

## OUTPUT from this script: Can be used to assess data against relevant criteria
## e.g. IBA or KBA criteria
## IBA: http://datazone.birdlife.org/site/ibacriteria
## KBA: https://www.keybiodiversityareas.org/working-with-kbas/proposing-updating/criteria

## Jono Handley, jonathan.m.handley@gmail.com

## R version 4.1.2 (2021-11-01) -- "Bird Hippie"

## June 2022

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries --------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(track2KBA)
library(tidyverse)
library(readxl)
library(sf)
library(xlsx)
library(sp)
library(gridExtra)
library(viridis)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Should explain the relevance of the datagroup when applying track2KBA"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Before starting with any tracking data. Look at the data! Does it make sense?
- Load raw data
- Save as shapefiles
- Plot in GIS
It's often easier to explore your data in an interactive way.
BUT, consider best approach depending on size of dataset."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Run example track2KBA data from GitHub account / tutorial          ----------
## https://github.com/BirdLifeInternational/track2kba
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data(boobies)
head(boobies,2)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## formatFields() ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Format the key data fields to the standard used in track2KBA
dataGroup <- formatFields(
  dataGroup = boobies, 
  fieldID   = "track_id", 
  fieldDate = "date_gmt", 
  fieldTime = "time_gmt",
  fieldLon  = "longitude", 
  fieldLat  = "latitude"
)

## Check output
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


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Define colony ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## example relates to a seabird during the breeding season, when the species
## is known to be a central place forager, therefore, define the colony position
colony <- dataGroup %>% 
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude)
  )

##
str(dataGroup)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tripSplit() ----
"If your data does not relate to a central place forager (CPF), OR
a time when an animal may be exhibiting central place foraging behaviours,
then skip this section and those relating to CPF data."
## Split tracking data into trips for Central Place Foragers via tripSplit() ----
"This step is often very useful to help automate the removal of location points
on land, or near the vicinty of a colony. We don't want these extra points to bias
our interpretation of the data."
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dataGroup
"load data directly if needed."
#save(dataGroup, file = "C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_dataGroup_boobies.Rdata")
#load("Track2KBA_SupportFiles/track2KBA_dataGroup_boobies.Rdata")

## Split the trips
"The user must define ecoogically sensible parameters to help automate the tripSplitting
process."
## Input is a 'data.frame' of tracking data and the central-place location. 
## Output is a 'SpatialPointsDataFrame'.
trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 3,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 1,      # hours - defines time an animal must have travelled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  rmNonTrip  = TRUE    # If true - points not associated with a trip will be removed
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


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"nonsensical splitting"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Split the trips
"DEFINE nonsensical ecological parameters. Distance"
trips_nonsense <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 30,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 1,      # hours - defines time an animal must have travelled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  rmNonTrip  = TRUE    # If true - points not associated with a trip will be removed
)

## plot nonsensical splitting tracks - just for demonstration
"If innerBuff is really large, and rmNonTrip = T, as in example above, you remove
all the points in the innerBuff."
plot(st_geometry(st_as_sf(trips_nonsense)), 
     cex = 0.5, 
     pch = 1)


## Split the trips
"DEFINE nonsensical ecological parameters. Distance AND Time"
trips_nonsense_time <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 30,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 8,      # hours - defines time an animal must have travelled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  rmNonTrip  = TRUE    # If true - points not associated with a trip will be removed
)

## plot NONSENSE splitting tracks - just for demonstration
"If innerBuff is really large AND duration is too long, and rmNonTrip = T, 
as in example above, you remove all the points in the innerBuff, and also those
points associated with trips shorter than what you set the duration at."
plot(st_geometry(st_as_sf(trips_nonsense_time)), 
     cex = 0.5, 
     pch = 1)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Split trips but keep all locations"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## considering if argument rmNonTrip = False
"If you specify rmNonTrip = False, then you keep all the points not assigned to 
individual trips. I.e. you still keep all the points over land, at the colony, etc."
trips_false <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 3,      # km - defines distance an animal must travel to count as trip started
  returnBuff = 10,     # km - defines distance an animal must be from the colony to have returned and thus completed a trip
  duration   = 1,      # hours - defines time an animal must have travelled away from the colony to count as a trip. helps remove glitches in data or very short trips that were likely not foraging trips.
  rmNonTrip  = FALSE    # If true - points not associated with a trip will be removed
)

## compare the different scenarios in plots
plot(st_geometry(st_as_sf(trips)), cex = 0.5, pch = 1) # ecologically sensible split
plot(st_geometry(st_as_sf(trips_nonsense)), cex = 0.5, pch = 1) # ecologically nonsensical: distance
plot(st_geometry(st_as_sf(trips_nonsense_time)), cex = 0.5, pch = 1) # ecologically nonsensical: distance AND time
plot(st_geometry(st_as_sf(trips_false)), cex = 0.5, pch = 1) # keeping all points

## compare different scenarios in data - Number of points
dim(trips)
dim(trips_nonsense)
dim(trips_nonsense_time)
dim(trips_false)

## Other comparisons
head(data.frame(trips_false),3)
head(data.frame(trips),3)
table(trips@data$Returns)
table(trips_false@data$Returns)
unique(trips$tripID)
unique(trips_false$tripID) ## Note the '-1' tripID

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## mapTrips() ----
## view data after splitting into trips ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## plot quick overview of trips recorded for individual birds
mapTrips(trips = trips, colony = colony)

## convert to sf object and plot overall data again for all birds
trips_sf <- st_as_sf(trips)
plot(st_geometry(trips_sf), 
     cex = 0.5, 
     pch = 1)

## convert to sf object and plot overall data again for first single tripID
trips_sf_IndTrip <- st_as_sf(trips) %>% 
  dplyr::filter(tripID == unique(trips$tripID)[1])

plot(st_geometry(trips_sf_IndTrip), 
     cex = 0.5, 
     pch = 1)

## number of datapoints after tripSplit.
## Remember, if rmNonTrip = TRUE, you will remove points not associated with a trip.
nrow(trips)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## data for summarising
head(data.frame(trips),2)

## Determine difference between consecutive timestamps 
## (NB: consecutive order of timestamps is critical here!)
## Doing this by tripID, not individual ID - change the group_by argument if needed
timeDiff <- trips %>% 
  data.frame() %>% 
  group_by(tripID) %>% 
  arrange(DateTime) %>% 
  mutate(delta_secs = as.numeric(difftime(DateTime, lag(DateTime, default = first(DateTime)), units = "secs"))) %>% 
  slice(2:n()) 

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
  summarise(avg_timegap_secs = mean(delta_secs), 
            min_timegap_secs = min(delta_secs),
            max_timegap_secs = max(delta_secs)) %>% data.frame()

## View results
SummaryTimeDiff
head(SummaryTimeDiff)

## Sort data by maximum time gap first - then view.
## Consider if you have any outline trips with massively different time gaps.
SummaryTimeDiff %>% arrange(-max_timegap_secs) %>% head()
  

## Print warning for users to consider
warning("Consider whether the sampling interval of your tracking data is appropriate
        for formally running the track2KBA functions. Remember, the time differences
        between each of your location points should be equal (or close enough to equal) 
        across all location points and individuals tracked. If the time difference
        between location points is not equal, the outputs you generate from track2KBA
        will not be valid because the underlying kernel density analysis implemented
        within the track2KBA functions will be invalid.")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot the outlier trips ----
## Assess if trips need manual manipulation or should be removed
## NB: Consider also if these trips will be removed in later steps.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## create vector of outlier trips
class(trips$tripID)
outlier <- c("69304_01")

## convert to sf object and subset necessary trip IDs for inspection
outlier.trips <- st_as_sf(trips) %>% dplyr::filter(tripID %in% outlier)

## plot quick overview of trips 
## requires converting back to sp object for working with mapTrips.
mapTrips(trips = as_Spatial(outlier.trips), colony = colony)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Interpolating tracking data to make sampling interval equal ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

warning("There are several ways to clean up tracks and generate data so that 
        sampling intervals are equal across all location points.
        e.g. linear interpolation, CRAWL package. Further advice to be generated")

warning("Also, before implementing some form of interpolation, you may wish to use
        a speedfilter to clean up outlier location points.")

warning("NOTE: Speedfilters and interpolation generally apply to tracking data from
        all scenarios, not only for central place foraging animals.")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter and summary of number of trips after splitting tracks ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Before summarizing the trip movements, using tripSummary(). 
## First, we can filter out data from trips that did not return to the vicinity 
## of the colony (i.e. within returnBuff), so they don't skew the estimates.
## Let's also check how many trips we record as Yes vs. No before filtering

head(trips,2)
table(trips$Returns)

totalTripsAll <- data.frame(trips) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)

table(totalTripsAll$Returns)

## Filter to only include trips that return
trips <- subset(trips, trips$Returns == "Yes" )

totalTripsYes <- data.frame(trips) %>% group_by(tripID, Returns) %>% 
  summarise(count = n()) %>% 
  data.frame(.)

table(totalTripsYes$Returns)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tripSummary() ----
## Rough summary of tracking data for complete trips ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sumTrips <- tripSummary(trips = trips, colony = colony)

## Check you only have complete trips here (if that is what you are aiming for)
table(sumTrips$complete)
## filter for only complete trips if needed
#sumTrips  <- sumTrips  %>% dplyr::filter(complete= "complete trip")

## view output
head(sumTrips ,10)
## view unique individual ID
unique(sumTrips$ID)
## number of individuals with tracking data
length(unique(sumTrips $ID))
## number of unique trips from all individuals
length(unique(sumTrips$tripID))

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## projectTracks() ----
## track2KBA uses Kernel Density Estimation (KDE) to produce space use estimates 
## for each individual track. In order for these to be accurate, we need to 
## transform the tracking data to an equal-area projection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(data.frame(trips))

tracks <- projectTracks(dataGroup = trips, projType = 'azim', custom=TRUE )
class(tracks)
str(tracks)

## compare 
head(data.frame(trips),2)
head(data.frame(tracks),2)

## compare projections
proj4string(trips)
proj4string(tracks)

## Consider a custom projection
warning("FROM GITHUB SUPPORT PAGE:
        track2KBA uses Kernel Density Estimation (KDE) to produce space use estimates 
        for each individual track. In order for these to be accurate, we need to 
        transform the tracking data to an equal-area projection. We can use the 
        convenience function projectTracks to perform this projection. We can select 
        between an azimuthal or cylindrical projection, and decide whether to center
        the projection on the data itself. Custom-centering is generally a good idea 
        for quick analyses as this will minimize distortion, however it is important
        to remember that the resulting projection will be data specific. So if you 
        remove even one track and re-analyze, the projection will differ between 
        datasets. For formal analysis, the best solution is to find a standard 
        projection that is appropriate for your study region.")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## findScale() ----
## findScale provides options for setting the all-important smoothing parameter in the KDE. 
## findScale calculates candidate smoothing parameter values using several different methods.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hVals <- findScale(
  tracks   = tracks,
  scaleARS = TRUE,
  sumTrips = sumTrips)

hVals

## must choose between one of three smoothing parameters for further analyses
## smoothing parameter is distance in km. Read more in supporting documents
hVals$mag # affected by range of animal movement. Only works for central place foragers.
hVals$href # sort of represents quality of data
hVals$scaleARS # affected by quality of data and subsequent ability to determine scale at which animal interacts with environment. Learn more about First Passage Time analysis

warning("Choosing the 'optimal' smoothing parameter is critical. See GitHub page.
        Consider what further advice we can give to users regarding choice of 
        smoothing parameter.")

warning("Given track2KBA gives three options for smoothing parameter, how will
        users know which is best? Especially inexperienced users. ALSO, how
        does this choice now impact analysing large datasets. How can we better
        automate the decision about choosing optimal smoothing parameter?")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## estSpaceUse() ----
## Produce KDEs for each individual
## Note: here we might want to remove the trip start and end points that fall 
## within the innerBuff distance we set in tripSplit, so that they don't skew 
## the at-sea distribution towards to colony.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(tracks,4)
min(tracks$ColDist)


warning("Must update GitHub tutorial which specifies distance incorrectly")
## remove trip start and end points near colony - distance is in m (unlike innerBuff where distance was in km)
proj4string(tracks)
tracks <- tracks[tracks$ColDist > 3*1000, ] 

## produce the KDEs for each individual
## NOTE: The grid cell size (i.e., grid resolution) should be less than the 
## selected h value, otherwise the entire space use area of the animal may be 
## encompassed in few cells

KDE <- estSpaceUse(
  tracks = tracks, 
  scale = hVals$mag, 
  levelUD = 50, 
  polyOut = TRUE
)

mapKDE(KDE = KDE$UDPolygons, colony = colony)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Over or undersmoothing? ----
## After applying estSpaceUse and viewing plot with mapKDE, at this step we should 
## verify that the smoothing parameter value we selected is producing reasonable 
## space use estimates, given what we know about our study animals. Are the core 
## areas much larger than expected? Much smaller? If so, consider using a different 
## value for the `scale` parameter.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## convert to sf object and plot OVERALL data again for first single individual
trips_sf_IndBird <- st_as_sf(trips) %>% 
  dplyr::filter(ID == unique(trips$ID)[1])

plot(st_geometry(trips_sf_IndBird), 
     cex = 0.5, 
     pch = 1)

## get the UD for a single individual and add to plot
ud_sf_IndBird <- KDE$UDPolygons %>% 
  dplyr::filter(id == trips_sf_IndBird$ID[1]) %>% 
  st_transform(.,crs = st_crs(trips_sf_IndBird))

plot(st_geometry(ud_sf_IndBird),add=T, border = "blue")

## Determine number of points captured in UD
sf::sf_use_s2(FALSE) # run this if issues with st_intersection: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
PointsInUD <- st_intersection(trips_sf_IndBird, ud_sf_IndBird)

nrow(trips_sf_IndBird)
length(unique(PointsInUD$DateTime))
length(unique(PointsInUD$DateTime)) / nrow(trips_sf_IndBird) * 100

warning("Assess whether your selected smoothing parameter has resulted in sensible
        Utilisation Distributions.")

warning("Can we use the above to consider some form of test to guide smoothing
        parameter choice?.")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## repAssess() ----
## estimate how representative this sample of animals is of the population.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"NOTE: See iterations"
repr <- repAssess(
  tracks    = tracks, 
  KDE       = KDE$KDE.Surface,
  levelUD   = 50,
  iteration = 1, ## iterations should be set to 100 at a minimum when running the script officially
  bootTable = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## findSite() ----
## using findSite we can identify areas where animals are overlapping in space 
## and delineate sites that meet some criteria of importance.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## indicate the population size of your source populaiton. e.g. the population size
## of the colony from which you tracked birds. For KBA identification, this estimate
## should be in Mature Individuals. 
## I.e. for seabird colonies: breeding pairs * 2 = mature individuals
SourcePopulationSize = 500

## 
Site_PolyTrue <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = SourcePopulationSize,     # 500 individual seabirds breed one the island
  polyOut = TRUE
)

##
dim(Site_PolyTrue)

## plot
Sitemap_PolyTrue <- mapSite(Site_PolyTrue, colony = colony)
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

Site_PolyFalse <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = SourcePopulationSize,     # 500 individual seabirds breed one the island
  polyOut = FALSE
)

#
dim(Site_PolyFalse)
max(Site_PolyFalse@data$N_IND)
max(Site_PolyFalse@data$N_animals)
head(unique(Site_PolyFalse@data$ID_IND))

## plot
Sitemap_PolyFalse <- mapSite(Site_PolyFalse, colony = colony)
Sitemap_PolyFalse

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Understanding the findSite() output ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## rename output with population estimate to align with GitHub example
Site <- Site_PolyTrue

## explore object
Site
dim(Site)

## simple plots to explain what is happening
ggplot() +  geom_sf(data = Site[1,])
ggplot() +  geom_sf(data = Site[2,])
ggplot() +  geom_sf(data = Site[3,])
ggplot() +  geom_sf(data = Site[4,])

## or different way of simple plot to explain what is happening
ggplot() +  
  ## plot first row of output. i.e. the area where no core areas are overlapping
  geom_sf(data = Site[1,], fill = "lightblue") +
  ## second row. i.e. the areas represented by the core areas of single individuals
  geom_sf(data = Site[2,], fill = "blue") +
  ## core areas of two individuals overlapping
  geom_sf(data = Site[3,], fill = "orange") +
  ## core areas of three individuals overlapping
  geom_sf(data = Site[4,], fill = "red") + ## could repeat further until you reach your total sample of tracked individuals
  geom_sf(data = Site[5,], fill = "green")
  
## So, as per the GitHub example, where 43 birds were tracked:
length(unique(boobies$track_id))

## the areas, for example, where 4 cores area overlap, would represent
## the area used by approximately 9% of the population
(4 / length(unique(boobies$track_id))) * 100

## the areas, for example, where 5 cores area overlap, would represent
## the area used by approximately 12% of the population
(5 / length(unique(boobies$track_id))) * 100

##
print("So, how does the above relate to the representativness score?")

## From Beal et al. 2021 Supp material
warning("By default, findSite sets the threshold of site importance for the source 
      population based on the degree of tracking sample representativeness 
      following Lascelles et al. (2016): i.e., samples that are 
      ???90%, 80-89%, 70-79%, and <70% representative set the threshold for 
      delineating a site as important for the local source population at 
      10%, 12.5%, 25%, and 50%, respectively ")

## What this mean is:
warning("For samples with representativeness of >90%, the area that is considered
      potentially suitable for assessment against relevant criteria. eg. KBAs,
      is only that area used by 10% or MORE of the individuals from the source
      population. Any area used by 10% or LESS of the source population is not
      thought to sufficiently represent where the source population may be found,
      and hence is excluded from further assessment.")

warning("NOTE: The threshold rules were agreed upon by expert consensus.")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## findSite() ----
## Understanding the impact of different population estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## above we ran findSite with a population estimate of 500.
## let's save this as a new object
Site_PolyTrue_500 <- Site_PolyTrue

## Now let's run findSite again with a different population estimate.
SourcePopulationSize = 10000

## 
Site_PolyTrue_10000 <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = SourcePopulationSize,     # 500 individual seabirds breed one the island
  polyOut = TRUE
)

#
dim(Site_PolyTrue_10000)

## plot
Sitemap_PolyTrue_10000 <- mapSite(Site_PolyTrue_10000, colony = colony)
Sitemap_PolyTrue_10000

## Compare the plots with different population estimates
SiteMap_PolyTrue_500 <- mapSite(Site_PolyTrue_500, colony = colony)
SiteMap_PolyTrue_500
Sitemap_PolyTrue_10000
grid.arrange(SiteMap_PolyTrue_500, Sitemap_PolyTrue_10000, ncol=2)

##
warning("So, you can see that by changing the source population estimate,
        the core area identified does not change. Only the estimated number of
        animals using the core site changes.")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Understanding the repAssess() output
## Following the descriptions above, let's understand the repAssess() output further
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Example with 90% representativeness
Site_Test_Rep90 <- findSite(
  KDE = KDE$KDE.Surface,
  #represent = repr$out,
  represent = 90, # here, instead of inputs from the GitHub example, I am just using 90% representativeness as an example
  levelUD = 50,
  popSize = SourcePopulationSize,     # 500 individual seabirds breed one the island
  polyOut = TRUE
)

## Example with 75% representativeness
Site_Test_Rep75 <- findSite(
  KDE = KDE$KDE.Surface,
  #represent = repr$out,
  represent = 75, # here, instead of inputs from the GitHub example, I am just using 75% representativeness as an example
  levelUD = 50,
  popSize = SourcePopulationSize,     # 500 individual seabirds breed one the island
  polyOut = TRUE
)


## plot the two outputs
p1 <- mapSite(Site_Test_Rep90, colony = colony)
p2 <- mapSite(Site_Test_Rep75, colony = colony)
grid.arrange(p1, p2, ncol=2)

## Understand
warning("Note, when representativeness = 90% a bigger overall area in red is defined
      as potentially suitable for assessment against relevant criteria.
      when representativeness = 75% a SMALLER overall area in red is defined
      as potentially suitable for assessment against relevant criteria.
      This is because with a lower representativeness score, track2KBA provides
      a more conservative estimate of which areas can definitively be considered
      core areas regularly used by the sampled population.
      NOTE also: the total number of individuals estimated using the area is reduced
      when representativeness is lower. ie. basically, a lower total number of
      individuals are estimated to be using areas when we are more unsure about how
      well the data likely represents the source population.")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## IBA Criteria assessment () ----
## Assessing final outputs against relevant criteria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

warning("Remember, when polyOut = TRUE for function findSite(), the output
      includes a simple features object with polygons (represented by each row of data) 
      that represent the number of overlapping UDs for individuals (N_IND), and the associated
      estimate of abundance (N_animals) within each of those polygons scaled according
      to the popSize and representativeness score.")

## SEE:
colnames(Site)
head(Site)

warning("In track2KBA we don't offer much advice about how to use the final outputs
        from findSite() for assessing data against various criteria, such as the IBA
        or KBA criteria. The text on GitHub (as of 18 Oct 2022), notes: 
        
        'Then, we can combine all the polygons within the 'potentialSite' area, 
        and use, for example, the maximum number of individuals present in that 
        area to assess whether it may merits identification as a Key Biodiversity 
        Area according to the KBA standard.', 
        
        BUT: This does text does not describe how to deal with all the individual
        polygons that were representative, and may be quite separate from each
        other in space!")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## IBA Criteria assessment () ----
"IBA Criteria assessment: OPTIONS"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

warning("There are essentially two ways to assess the outputs of track2KBA against
        IBA or KBA criteria.")


"OPTION 1"
## OPTION 1: Assess each row of data, where potentialSite==TRUE, against relevant 
## criteria. 

## For example, in
head(Site,10)
warning("In above, you would assess each row of data against criteria. So, you would
        use N_animals as your abundance estimate and then assess each row of data
        against relevant criteria.
        
        What this will give you ultimately, is a set of final polygons that have
        a minimum and maximum estimate for N_animals found in the polygon.")

## See
Sitemap_PolyTrue
warning("Inside the red polygons in the plot (Remember, the red polygons are the
        representative sites), there is a darker shade of blue in the middle and
        lighter shade of blue toward the boundary of the red polygons. 
        Perhaps obvious, but the darker area represents the maximum, and the ligher 
        area represents the minimum.")

warning("BUT: Inside the red polygon, it is technically feasible that the maximum
        number of animals could all use the areas inside the red polygon at various 
        points in time.
        
        HENCE, Option 2!!!")


"OPTION 2"
## OPTION 2: Assess each representative polygon of data, where potentialSite==TRUE, 
## against relevant criteria. 

## To assess each polygon we first need to summarise the data further.
## First, we must determine how many and where each unique polygon is.
Site.polygons <- Site %>% 
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
Site.polygons.data <- st_intersection(Site.polygons, Site)

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## IBA Criteria assessment () ----
"IBA Criteria assessment: OPTIONS SUMMARY"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Based on the two options above, you now have:
Site
Site.polygons

warning("WE, the marine science team / marine toolkit team, as of 18 October 2022,
        need to consdier which option is best still.")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cleaning up data () ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"In this section, as opposed to cleaning up data after IBA assessment, I could
the options about filling holes in polygons, dropping small pieces, considering
minimum convex polygons.

Especially if going with option 2 above."


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
