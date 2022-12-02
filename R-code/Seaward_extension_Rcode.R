## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Script overview----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PURPOSE OF SCRIPT: Delineate a seaward extension boundary from a colony of 
## breeding seabirds.

## SEAWARD EXTENSION DEFINITION: These are marine areas immediately surrounding 
## seabird breeding colonies. The areas are typically used for ecologically relevant 
## behaviours such as rafting, preening, bathing, foraging or transiting between
## foraging trips, depending on a species.

## INPUT data (typical): 
## Seabird breeding colony location - point or polygon data
## Abundance estimate for seabird breeding colony (when available)
## Estimate of appropriate buffer size

## OUTPUT data: 
## Polygon / Raster of seaward extension representing an area used for a specific
## behaviour

## CAUTION:
## Definig seaward extensions around seabird breeding colonies should consider 
## the ecology of the species. In other words: it is generally unsuitable to try
## and delineate important sites using this method that are artificially large.
## For example, it would likely be unsuitable to use this method to derive a foraging
## area for albatross; tracking data would be better suited for defining a foraging
## area for albatross. See further examples in the Marine Toolkit.

## Script authors:

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Script background ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Original workflow adapted from Critchley et al. 2018: https://doi.org/10.1016/j.biocon.2018.06.007
## With adaptations for Handley et al. 2021: https://doi.org/10.3389/fmars.2020.602972 


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(sf)
library(raster)
library(gdistance)
library(rgdal)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set working directory if required ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica")

mapppd<-read.csv("./Data/PopData/mIBA_Antarctica_PenguinColPopData_ForagingRadius_June2020.csv")

## Calling the update to gridDistance function within Raster package
## Jono Handley updated function to allow for better movement across cells (directions = 16)
source("./R_code_PEW_Antarctica/gridDistance_directions16.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Some data checking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(mapppd)
head(mapppd,2)
dim(mapppd)
table(mapppd$common_name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Check if any colonies have most recent count = 0
### consider if worth removing these colonies for analysis -> doesn't seem like Strycker et al. 2020
### did in all cases. Therefore, I'll keep all colonies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Reduce dataframe to keep necessary columns only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mapppd <- mapppd %>% dplyr::select(-Caution_recent, -Source_Main, -Source_Actual, -notes)
head(mapppd,2)
dim(mapppd)
table(mapppd$common_name)

## remove CHP data from Bouvet island
mapppd %>% filter(cammlr_region == 48.6)
mapppd <- mapppd %>% mutate(sps_ccamlr = paste(common_name, cammlr_region, sep = "_")) %>% 
  filter(sps_ccamlr != "CHP_48.6") %>% dplyr::select(-sps_ccamlr)
dim(mapppd)
table(mapppd$common_name)
head(mapppd,2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### get some test data to make a shapefile where I can edit bearings manually
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SEE ...JH4_Test script for further details

## Create new csv file for loading in ArcMap in which you can add bearing for each colony manually! - FML - Must be better way!
mapppd_test <- droplevels(mapppd)
table(mapppd_test$common_name)

## write as csv for now
write.csv(mapppd_test, paste("./Data/PopData/",
                             "mIBA_Antarctica_PenguinColPopData_ForagingRadius_June2020_Bearing.csv", sep=""), row.names = F)

## Having edited the file, load the shapefile
dat_bearing <- readOGR("./Data/PopData/mIBA_Antarctica_PenguinColPopData_ForagingRadius_June2020_Bearing.shp")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Plotting test data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(dat_bearing)
head(dat_bearing)
## correct the column names - thankfully columns are still in same order after editing in ArcMap
names(dat_bearing) <- c(colnames(mapppd),"bearing")
## Convert columns to numeric as needed
dat_bearing@data$penguin_count_max <- as.numeric(as.character(dat_bearing@data$penguin_count_max))
dat_bearing@data$penguin_count_min <- as.numeric(as.character(dat_bearing@data$penguin_count_min))
dat_bearing@data$penguin_count_recent <- as.numeric(as.character(dat_bearing@data$penguin_count_recent))
dat_bearing@data$count_median <- as.numeric(as.character(dat_bearing@data$count_median))


dat_bearing@data$year_max <- as.numeric(as.character(dat_bearing@data$year_max))
dat_bearing@data$year_min <- as.numeric(as.character(dat_bearing@data$year_min))
dat_bearing@data$year_recent <- as.numeric(as.character(dat_bearing@data$year_recent))
dat_bearing@data$year_nb <- as.numeric(as.character(dat_bearing@data$year_nb))




##
dat_bearing_sf <- st_as_sf(dat_bearing)
plot(st_geometry(dat_bearing_sf))


## need to amend ccamlr_colony which was clearly affected in fucking ArcMap!
summary(dat_bearing@data$cammlr_region)
table(dat_bearing@data$cammlr_region)

region_dat <- mapppd_test %>% dplyr::select(common_name, site_name, cammlr_region)
head(region_dat,3)
region_dat <- region_dat %>% unite(.,col = sps_site, common_name, site_name, sep="_")

dat_bearing_sf <- dat_bearing_sf %>% unite(.,col = sps_site, common_name, site_name, sep="_", remove = F)
head(dat_bearing_sf,2)

dim(dat_bearing_sf)
dim(region_dat)

## Join tables
dat2 <- left_join(dat_bearing_sf, region_dat, by = "sps_site")
head(dat2,2)

## replace values
dat2$cammlr_region.x <- dat2$cammlr_region.y
head(dat2,4)

## reformat data to match original input
## because data is already in spatial file - do not need to create anymore
## as was done in earlier versions of script. Therefore, also unselecting long / lat columns
## so this data will align with part in for loop where year is selected.
dat3 <- dat2 %>% dplyr::select(-sps_site,-longitude_epsg_4326,-latitude_epsg_4326, -cammlr_region.y) %>% rename(cammlr_region = cammlr_region.x)
head(dat3,4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Load in additional data layers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Antarctica Medium Res Polygon (read in as simple feature)
Ant_med_shp <- st_read("./Data/Shapefiles/Coastline_medium_res_polygon/Coastline_medium_res_polygon.shp")
Ant_high_shp <- st_read("./Data/Shapefiles/Coastline_high_res_polygon_v7.1/Coastline_high_res_polygon_v7.1.shp")

# Plot High to check
#plot(Ant_high_shp['surface'])
plot(st_geometry(Ant_high_shp))

#ggplot(Ant_high_shp) +
#  geom_sf(aes(fill = surface))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Convert data antarctica projection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col_locs_proj <- st_transform(dat3, crs = st_crs(Ant_high_shp))


## PLOT
plot(st_geometry(Ant_med_shp))
plot(st_geometry(col_locs_proj), add=T, pch = 19)

## plot different surface layers
#ggplot(Ant_med_shp) +
#  geom_sf(aes(fill = surface))

## Plot plain background map
#ggplot(Ant_med_shp) +
#  geom_sf() 

## Plot plain background and unique penguin colonies
#ggplot(Ant_med_shp) +
#  geom_sf() +
#  geom_sf(data=col_locs_proj, aes(fill = common_name, color = common_name))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Get max travel distance for all species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Max Dist for all regions where species are
MaxDist_All <- read.csv("./Data/Foraging Radii - Penguins of Antarctica/Report_PEW_Final/MaxDist_FR_summary_forR.csv")
MaxDist_All

"Note: Following Teschke et al. 2016 - Max values come from those reported in lit,
using mean-max value reported for the study + 3SDs"

# -----------------------------------------------------------------------------
### Generate raster from map for distance calculations
### Create blank raster of SPECIFIED resolution that covers full extent of region
"OR load raster directly, as possibly hashed out at end of this section"
# -----------------------------------------------------------------------------

# Specified resolution of grid cell in meters
spec_res = 5000

# border of antarctica and key islands
#Ant_med_shp ## get bounding box info (bbox)
#Ant_high_shp ## get bounding box info (bbox)

## Create larger area by buffer in meters, to account for seaward area beyond border land masses
#max(MaxDist_All$Chick_rearing)*1000
#ant_buff = 220000

#ras <- raster(xmn = -2661818 - (ant_buff), xmx = 2748309 + (ant_buff), 
#              ymn = -2491520 - (ant_buff), ymx = 2322031 + (ant_buff), 
#              resolution = spec_res)

#crs(ras) <- crs(Ant_high_shp) # match projection of raster to map
# rasterize will set ocean to NA so inverse it and set water to "1"
# land is equal to zero because it is "NOT" NA
#plot(Ant_med_shp)
#mask <- rasterize(Ant_high_shp, ras)
#writeRaster(mask, "./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km_mask.tif", overwrite = T)
#mask <- raster("./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km_mask.tif")
#plot(mask)
#ras <- is.na(mask)
#plot(ras)

# Set land to 2 to make it more expensive to cross
#ras[ras==0] <- 2
#plot(ras)

## save raster for comparing in arcmap
#writeRaster(ras, "./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif", overwrite = T)
"Load raster directly"
ras <- raster("./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif")
plot(ras)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Get max travel distance for all species
"NO LONGER RUNNING THIS SECTION OWING TO UPDATE USING MY OWN modifications and using gridDistance (see below)"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Further details of what is needed here: See: https://mgimond.github.io/Spatial/raster-operations-in-r.html'
# Create a Transition object from the raster (specifies how an animal could move from point to point in a raster)
# this calculation will take time when resolution is small

"From critchley et al 2018"
#tr <- transition(ras, function(x) 1/mean(x), 8)

"JH update:"
#tr_h16  <- transition(ras, transitionFunction=function(x){1},16,symm=FALSE)
#tr <- geoCorrection(tr_h16, scl = FALSE) # correct for diagonal distances
#save(tr, file = "./Data/Rdata/GeoCorrectedRaster.Rdata")
#load("./Data/Rdata/GeoCorrectedRaster.Rdata")

# -----------------------------------------------------------------------------
### Note: transition and geoCorrection may have issues with convergence (files to big)
### Therefore, could try to get smaller examples datasets or
### restart R and clear working memory
# -----------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Select data for single species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_for_analysis = "EMP"

cols_proj <- col_locs_proj %>% filter(common_name == species_for_analysis)
head(cols_proj,2)
dim(cols_proj)

## Check pop counts
cols_proj %>% filter(common_name ==species_for_analysis) %>% 
  arrange(common_name, -penguin_count_max) %>% 
  dplyr::select(common_name, penguin_count_max) %>% 
  head(3)


## Subset data for checking
#cols_proj <- cols_proj %>% filter(common_name ==species_for_analysis) %>% 
#  filter(penguin_count_max >= 284535)

#cols_proj

## Plot plain background and unique penguin colonies
#ggplot(Ant_med_shp) +
#  geom_sf() +
#  geom_sf(data=cols_proj, aes(fill = common_name, color = common_name))

# -----------------------------------------------------------------------------
### Transform and plot colonies
# -----------------------------------------------------------------------------

## Convert to SpatialPointsDataFrame and only select the first n entries for simplicity
#Colonies <- as_Spatial(slice(ADP_cols_proj, 1:3))
## Or, select them all
Colonies <- as_Spatial(cols_proj)
length(Colonies)
head(Colonies)

# Check colonies are in the right place
plot(ras)
plot(Colonies, pch = 19, add = T) 

## Create list of population counts for each colony
"Correct species population now determined within loop"
#Populations <- data.frame(Colonies@data$penguin_count_max) 
#head(Populations)
#dim(Populations)

#set foraging radius distance (in metres) to be used in model
'Could update and specify with a dataframe for each species'
#MaxDist <- 50000
MaxDist_Sps <- MaxDist_All %>% dplyr::select(Species_code, CCAMLR_Region, Chick_rearing) %>% 
  filter(Species_code == species_for_analysis)
MaxDist_Sps

## Check unique CCAMLR Regions for species
Cols_Check <- droplevels(subset(data.frame(Colonies), common_name == species_for_analysis))
table(Cols_Check$cammlr_region)
MaxDist_Sps <- droplevels(MaxDist_Sps)
table(MaxDist_Sps$CCAMLR_Region)


## -----------------------------------------------------------------------------
# Set up loop & progress bar 
## -----------------------------------------------------------------------------

## Set up loop through colonies
"Putting colony info into loop instead to account for different max distances at various colonies"
Colonies_Sps <- data.frame(coordinates(Colonies))
dim(Colonies_Sps)
head(Colonies_Sps)
nrow(Colonies_Sps)

# Create a stack to store each raster in the loop, 
# these will then be summed at the end
ColonyStack <- stack()
nlayers(ColonyStack)

# Create progress bar to track percentage of loops completed
# This is useful when there are a large number of colonies 
# but it does slow down the loop slightly
pb <- winProgressBar(title="Raster loop progress bar", label="0% done", 
                     min=0, max=100, initial=0)

## -----------------------------------------------------------------------------
# Generate distribution for each colony 
## -----------------------------------------------------------------------------
setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica")

## Change raster name for travel around land calculation in loop
#plot(ras)
#antrans <- ras

head(Colonies,2)
count_type = "penguin_count_recent"

"Consider count type within loop (will need to do for all 4)"
#for (i in 1:length(Colonies[,1])){
for (i in 1:length(Colonies)){
#  for (i in 1:3){    
  
  print(paste("Loop number", i, sep=" "))
  
  #-----From Critchley Code: Not sure this actually accounting for movement around land----"
  #-----Therefore, removing for now----"
  
  #R <- accCost(tr, SpatialPoints(Colonies_Sps[i,]))
  ## now raster still shows the expensive travel over land
  ## so we mask it out for sea travel only
  #R <- mask(R, mask, inverse = TRUE)
  
  #-----From PhD Code: Travel distance avoiding land------"
  
  ## Change raster name for travel around land calculation in loop - this also "resets"
  ## the required base raster for distance calculation.
  #plot(ras)
  antrans <- ras
  
  ## Extract the cell location where the colony is
  colonypoint <- Colonies_Sps[i,]
  pp <- extract(antrans,colonypoint, cellnumbers=TRUE) 
  #pp2 <- rowColFromCell(antrans, pp) # used to work in earlier version
  
  ## convert that cell to a value of 3 
  #antrans[pp2[1,1],pp2[1,2]] <- 3 # used to work in earlier version
  antrans[pp[1,1]] <- 3
  
  #-----Tests if you want------"
  #plot(antrans)
  #maxValue(antrans)
  #minValue(antrans)
  #summary(antrans)
  #plot(SpatialPoints(Colonies_Sps[i,]), pch = 19, add = T)
  #writeRaster(antrans, 
  #            paste("./Data/Rasters/Distribution_Test/","Colony_Mask_EMPStancomb.tif", sep=""), overwrite = T)
  
  ## You now have a raster with colony cell = 3, land cells = 2 and sea cells =1
  ## Calculate distance from colony with a land mask
  Dist.LandMask <- gridDistance(x=antrans, # x specifies which raster to use
                                origin=3, # origin specifies which cell to determine distance from
                                omit=2) # omit says which cell values to avoid while determining distance
  
    #maxValue(Dist.LandMask)
  if (maxValue(Dist.LandMask) == 0) print("WARNING: Check for NAs in final output of ColonyStack. Your origin(colony) may be to far in land. Therefore, your origin is completely buffered by omit cells(land cells), and no distance calculation for cells at sea can be performed") else print("Distance calculation successful")
  
  #plot(Dist.LandMask)
  
  #-----Change name back to Critchley code------"
  R <- Dist.LandMask
  
  ## JH: Get MaxDist for colony of interest, dealing with some annoynig factor to conversions
  MaxDist <- subset(MaxDist_Sps, CCAMLR_Region == as.character(paste((Colonies@data[i,]$cammlr_region))))
  MaxDist <- (MaxDist$Chick_rearing[1])*1000
  
  ## any cell further than MaxDist, make it NA
  R[R > MaxDist] = NA
  
  #plot(R)
  
  ## normalise to 0 and 1 probability of occurance
  R <- -1*(R/MaxDist)+1  
  
  #plot(R)
  
  ## Calculate ditance from each cell to the colony
  #dist.R <- distanceFromPoints(R, (Colonies[i,]))
  
  dist.R <- distanceFromPoints(R, (Colonies_Sps[i,]))
  #plot(dist.R)
  
  R<- R*(1/log(dist.R)) # weight areas closer to the colony of higher importance
  
  #plot(R)
  
  # normalise to 0 and 1 probability of occurance (1 is closest to colony)
  R <- ((R-cellStats(R,"min"))/(cellStats(R,"max")-cellStats(R,"min"))) 
  
  #plot(R)
  #writeRaster(R, 
  #            paste("./Data/Rasters/Distribution_Test/","CHP_col53.tif", sep=""), overwrite = T)
  #writeRaster(antrans, 
  #            paste("./Data/Rasters/Distribution_Test/","CHP_col53_baselayer.tif", sep=""), overwrite = T)
  
  
  "HERE IS WHERE I COULD LIKELY ADD IN DIRECTIONAL COMPONENT - Something
  that would cut out the area where it would have been unlikely that birds
  travelled around islands. I.e. putting a filter that says all possible
  location points should be buffered by XX degrees. This will avoid the problem
  of the foraging radius approach for island sites smaller than your specified
  resolution"
  
  ## ~~~~~~~~~~~~~~~~~  Define directional bounding box ~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## What do I need?
  ## The point location and a pizza form.
  ## Surely, with a starting point (the colony), and a bearing from that point,
  ## I could then draw a triangle of sorts and extract only the raster values 
  ## within that triangle?
  ## see: destPoint maybe in R?
  
  #plot(R)
  #plot(SpatialPoints(Colonies_Sps[i,]), pch = 19, add = T)
  
  ## Get colony coordinates only
  col_coords <- st_as_sf(data.frame(Lon = Colonies@coords[i,][1], Lat = Colonies@coords[i,][2]),
                                    coords = c("Lon", "Lat"), crs=st_crs(Ant_high_shp))
    
  #plot(col_coords, add= T, col = "red")
  
  ## convert to WGS84
  col_coords_wgs84 <- st_transform(col_coords, crs = 4326)
  
    ## get starting point
  p1 <- as_Spatial(col_coords_wgs84)
  
  ## specify angle on either side of colony bearing to sea
  angle = 60
  "Can consider appropriate angle still"
  
  "Will need to individually (or programmatically somehow) define the likely bearing for each colony"
  
  ## Use this if bearing 0 degrees relates to East
  #ArcMap_bearing = 150 
  ## Then account for map bearing
  #p_bearing = 360 - (ArcMap_bearing - 90)
  
  ## Use this if bearing 0 degrees relates to North
  p_bearing = Colonies@data[i,]$bearing
  
  
  ## calcualte bounding points
  p2 <- destPoint(p1, (p_bearing+angle), 400000)
  p3 <- destPoint(p1, (p_bearing-angle), 400000)
  ## convert back to sf and antarctica projection
  p2sf <- st_as_sf(data.frame(p2), coords = c("lon", "lat"), crs = 4326) %>% st_transform(., crs = st_crs(Ant_high_shp))
  #plot(p2sf, add = T, col = "red")
  
  p3sf <- st_as_sf(data.frame(p3), coords = c("lon", "lat"), crs = 4326) %>% st_transform(., crs = st_crs(Ant_high_shp))
  #plot(p3sf, add = T, col = "red")
  
  ## plot points to check what is happening in ArcMap
  #st_write(p2sf, paste("./Data/Rasters/Distribution_Test/","CHP_col53_0angle.shp", sep=""), delete_layer = T) 
  #st_write(p3sf, paste("./Data/Rasters/Distribution_Test/","CHP_col53_210angle_v2.shp", sep=""), delete_layer = T) 
  
  
  ## create a polygon from the points
  #p1@coords[1]
  #p1@coords[2]
  #p2
  #p3
  
  poly_input <- rbind(c(p1@coords[1],p1@coords[2]), c(p2[1],p2[2]), c(p3[1],p3[2]), c(p1@coords[1],p1@coords[2]))
  pol <-st_polygon(list(poly_input))
  #plot(pol)
  
  #class(pol)
  #st_crs(pol)
  #class(col_coords_wgs84)
  pol2 <- st_sfc(pol, crs = 4326)
  #plot(pol2)
  pol3 <- st_transform(pol2, crs = st_crs(Ant_high_shp))
  #plot(pol3)
  #plot(R)
  #plot(pol3, add = T)
  
  #st_write(pol3, paste("./Data/Rasters/Distribution_Test/","CHP_col53_Poly.shp", sep=""), delete_layer = T) 
  
  
  ## ~~~~~~~~~~~~~~~~~  extract cells in directional bounding box ~~~~~~~~~~~~~~~~~~~~~~~~
 
  ## create a buffer of same resolution as analysis
  pol4_buf <- st_buffer(pol3, dist = 5000) %>% as_Spatial()
  #plot(pol4_buf,add=T)
  #st_write(st_buffer(pol3, dist = 5000), paste("./Data/Rasters/Distribution_Test/","CHP_col53_PolyBuf.shp", sep=""), delete_layer = T) 
  ## mask cells needed
  R_sample <- mask(R, pol4_buf)
  #plot(R_sample)
  
  #writeRaster(R_sample, 
  #            paste("./Data/Rasters/Distribution_Test/","CHP_col53_RasterBoundBuff.tif", sep=""), overwrite = T)
  
  
  ## ~~~~~~~~~~~~~~~~~  continue with parts of Critchley et al. 2018 method ~~~~
    
  
  ## whole area sums to one
  #'JH: Did Critchley do this right? This would mean that birds distribute themselves across the entire area
  #at sea, as oppose to the cell nearest the colony being used by all birds. Proportionally, this may not matter,
  #but the impact on summing birds for IBA purposes may be larger! Must consider...'
  #'Hashing out for now'
  #R <- R/sum(getValues(R), na.rm = T)  
  
  ## multiply by the number of pairs at each colony  
  #R <- R*(Populations[i,1])  
  #R <- R*(Colonies@data[1,]$penguin_count_min)
  #R <- R*(Colonies@data[1,]$count_median)
  #R <- R*(Colonies@data[i,]$penguin_count_max)
  pop_num <- Colonies@data[i,] %>% dplyr::select(count_type)
  pop_num <- pop_num[1,]
  R <- R_sample
  R <- R*(as.numeric(as.character(pop_num))) # required because the pop estimates are being stored as factors after updating in ArcMap
  #R
  
  plot(R)
  title(paste(i, Colonies@data[i,]$site_id, sep="_"))
  
  # access raster attribute table
  R <- ratify(R)
  rat <- levels(R)[[1]]
  
  ## assign relevant attributes
  rat$Colony <- Colonies@data[i,]$site_id
  rat$Species <- Colonies@data[i,]$common_name
  
  ## Attributes relating to year of count - get column number
  yr <- if (count_type=="penguin_count_recent") {
    6
  } else if (count_type=="penguin_count_min") {
    10
  } else if (count_type=="count_median") {
    14
  } else if (count_type=="penguin_count_max") {
    18
  } 
  yr
  
  ## Assign attribute
  rat$CountYear <- as.numeric(Colonies@data[i,][yr])
  
  ## Confirm attributes into raster
  levels(R) <- rat
  #r@data@attributes[[1]]
  #r$layer@data@attributes[1:2][1]
  
  ## Save raster for exploring in ArcMap
  ## Note: Doesn't seem straightforward to save the attribute table for viewing in ArcMap, see: https://stackoverflow.com/questions/23840178/how-to-write-a-raster-with-rat-factors-in-r-raster-package
  #writeRaster(R, 
  #            paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_TEST.tif", sep=""), overwrite = T)
  
  
  # Plot raster to check it worked
  # This will slow down the loop so this step can be removed to speed things up
  #par(ask = F)
  #plot(R)
  #maxValue(R)
  #minValue(R)
  
  # Save raster for each colony into stack
  ColonyStack <- stack(ColonyStack, R)
  
  rm(R,antrans) # Remove large file that is no longer needed
  
  # run progress bar - can be removed to speed up loop
  Sys.sleep(0.1)  # slow down the code for illustration purposes
  #info <- sprintf("%d%% done", round((i/length(Colonies[,1]))*100))
  #setWinProgressBar(pb, i/(length(Colonies[,1]))*100, label=info)
  
  info <- sprintf("%d%% done", round((i/length(Colonies_Sps[,1]))*100))
  setWinProgressBar(pb, i/(length(Colonies_Sps[,1]))*100, label=info)
  
}
## Plot individual layers

nlayers(ColonyStack) ## check nlayers matches number of colonies

## check random subset of layers
Colonies@data[2,]
ColonyStack[[1]]
ColonyStack[[2]]
ColonyStack[[3]]
ColonyStack[[nlayers(ColonyStack)]]

plot(ColonyStack[[1]])
plot(ColonyStack[[2]])
plot(ColonyStack[[3]])

## Check data associated with each layer
ColonyStack[[1]]
summary(ColonyStack[[1]]@data@values)

## check max value of each layer (should be same as penguin count for given count type (min, max, recent, median))
maxValue(ColonyStack)
## Check for no NA values - could indicate something in loop above is not working
max(maxValue(ColonyStack))

## Check count type in raw data to compare with above
col_locs_proj %>% filter(common_name == species_for_analysis) %>% arrange(common_name, -(penguin_count_recent)) %>% head()

## Save RasterStack
save(ColonyStack, 
            file=paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_dist16_stack.Rdata", sep=""))

#rm(ColonyStack)
#load(paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_dist16_stack.Rdata", sep=""))

## -----------------------------------------------------------------------------
# Sum over each raster and generate summed distribution for the species and entire region
## -----------------------------------------------------------------------------
"NOTE, the dist16 in saved named refers to 3rd version because of updates with gridDistance"

output <- sum(ColonyStack, na.rm = T) # sum cell values across colonies
#writeRaster(output, filename = "Raster_name", format = "GTiff", overwrite = TRUE) # Write to raster file
plot(output)

writeRaster(output, 
            paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_A_dist16.tif", sep=""), overwrite = T)

## Specify 0 cells as NA
output[output==0] <- NA
plot(output)
maxValue(output)
writeRaster(output, 
            paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_B_dist16.tif", sep=""), overwrite = T)

## Specify all cells over 1% global population as IBA cells
#gpop_ADP=3790000
#gpop_CHP=6600000 #gpop_CHP=2965800 (Updated ref of 6,600,000 based on pers. comm with Heather Lynch in 2019)
#gpop_EMP=256500 #gpop_EMP=238000 (Updated ref of 256500 based on Trathan et al. 2019)
#gpop_GEP=387000

gpop <- data.frame(Species = c("ADP", "CHP", "EMP", "GEP"),
                  Global_Pop = c(3790000, 3410000, 256500, 387000)) # Refs: ADP, CHP = Strycker et al. 2020, EMP = Trathan et al. 2019, GEP = 

# Filter cells above and below global population threshold of 1%
output[output < subset(gpop, gpop$Species == species_for_analysis)[,2] /100] <- 0
output[output >= subset(gpop, gpop$Species == species_for_analysis)[,2] /100] <- 1
plot(output)

writeRaster(output, 
            paste("./Data/Rasters/IBAs_CellsJune2020/",species_for_analysis,"_",count_type,"_C_dist16.tif", sep=""), overwrite = T)


## -----------------------------------------------------------------------------
### Save colonies as shapefiles for comparison
## -----------------------------------------------------------------------------
head(col_locs_proj,2)
table(col_locs_proj$common_name)

sps <- unique(col_locs_proj$common_name)
sps

## v2 plots include EMP colonies from Trathan et al. 2019
## V3 final sites used for mIBA analysis

#setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica\\Data\\Shapefiles\\MAPPPD\\Final report")
for(i in sps){
  temp <- subset(col_locs_proj, col_locs_proj$common_name == i)
  temp_locs_spdf <- as_Spatial(temp)
  writeOGR(obj = temp_locs_spdf, dsn = "./Data/Shapefiles/PopulationData/FinalReport_v2", 
           layer = paste(i, "_FinalSites_mIBAanalysis_v3", sep=""), driver="ESRI Shapefile", overwrite_layer = T)
}



## -----------------------------------------------------------------------------
# Plot distribution map
## -----------------------------------------------------------------------------

par(ask = F)
plot(output)
plot(st_geometry(Ant_med_shp), add=T)
points(Colonies, pch = 19, cex = 0.75, col = 2)


