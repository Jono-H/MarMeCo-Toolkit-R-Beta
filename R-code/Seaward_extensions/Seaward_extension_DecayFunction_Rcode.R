## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Script overview----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PURPOSE OF SCRIPT: Delineate a seaward extension boundary from a colony of 
## breeding seabirds.
"DECAY FUNCTION SCRIPT"

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
## Defining seaward extensions around seabird breeding colonies should consider 
## the ecology of the species. In other words: it is generally unsuitable to try
## and delineate important sites using this method that are artificially large.
## For example, it would likely be unsuitable to use this method to derive a foraging
## area for albatross; tracking data would be better suited for defining a foraging
## area for albatross. See further examples in the Marine Toolkit.

## Script authors:
"Jonathan Handley, Bethany Clark"

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
library(geosphere)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set working directory if required ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Colony data: location, abundance estimate, bearing pre-defined
load("data-testing/data.penguins.antarctica.Rdata")
d <- data.penguins.antarctica
head(d,2)

## Basemap:
load("./data-testing/Coastline_high_res_polygon_v7.1.Rdata")
head(Ant_high_shp)
basemap <- Ant_high_shp
##plot(st_geometry(basemap)) ## Detailed map - takes time to plot!

## Buffer distance for species
load("data-testing/Buffer.size.penguins.antarctica.Rdata")
buffer.dist <- MaxDist_All
head(buffer.dist)

## clean up working environment
rm(Ant_high_shp, MaxDist_All, data.penguins.antarctica)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load functions ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calling the update to gridDistance function within Raster package
## Jono Handley updated function to allow for better movement across cells (directions = 16)
source("./R-functions/gridDistance_directions16.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Convert colony data to Antarctica / basemap projection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
col_locs_proj <- st_transform(d, crs = st_crs(basemap))


## PLOT colonies
plot(st_geometry(col_locs_proj))

## PLOT basemap and colonies
#plot(st_geometry(basemap)) ## consider size of basemap before plotting
#plot(st_geometry(col_locs_proj), add=T, pch = 19)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Generate raster from map for distance calculations ----
## Create blank raster of SPECIFIED resolution that covers full extent of region
"OR load raster directly, at end of this section"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Specified resolution of grid cell in meters
"For Marine Toolkit, need to provide guidance on choice of resolution. 
Currently, 5km aligned with same scale as the Critchley et al. 2018 paper."
spec_res = 5000

## border of antarctica and key islands
basemap ## manually get bounding box info (bbox)

## Create larger area by buffer in meters, to account for seaward area beyond border land masses
max(buffer.dist$Chick_rearing)*1000
ant_buff = 220000

## Create blank raster
ras <- raster(xmn = -2661818 - (ant_buff), xmx = 2748309 + (ant_buff), 
              ymn = -2491520 - (ant_buff), ymx = 2322031 + (ant_buff), 
              resolution = spec_res)

## set projection of raster
crs(ras) <- crs(basemap) # match projection of raster to map

## convert the basemap, currently in polygon format, to a raster
"NOTE: Operation will take time depending on size of polygon data and detail
of borders. i.e. Higher resolution maps take longer to process."
mask <- rasterize(basemap, ras)

## Simplify raster
## NOTE: rasterize will set ocean to NA because it covers an area of cells where
## there are no polygons, so inverse it and set water to "1"
plot(mask)
ras <- is.na(mask)
plot(ras)

## land is equal to zero because it is "NOT" NA. i.e. there is already a polygon there.
## Set land to 2 to make it more expensive to cross
ras[ras==0] <- 2
plot(ras)

## save raster for comparing in arcmap
#writeRaster(ras, "./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif", overwrite = T)

## If you have already created the necessary raster - load it here.
ras <- raster("./data-testing/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif")
plot(ras)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Get max travel distance for all species
"By Jono Handley: For Marine Toolkit / Handley et al. 2021:
 NOT RUNNING THIS SECTION OWING TO UPDATE USING OWN modifications and using gridDistance (see below)"
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

### Note: transition and geoCorrection may have issues with convergence (files to big)
### Therefore, could try to get smaller examples datasets or
### restart R and clear working memory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Assess possible input parameters for loop below ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## assess possible species of choice
table(col_locs_proj$common_name)

## assess possible count types of choice
head(col_locs_proj,2)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Specify input parameters for loop below ----
"NOTE: This needs simplification or improving. Currently, you have to specificy
the different combinations manually for each run of the loop. So that's 4 penguin
species and 4 count types = 16 different runs of the loop"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## species
species_for_analysis = "EMP"

## count type
count_type = "penguin_count_recent"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Select data for single species ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols_proj <- col_locs_proj %>% filter(common_name == species_for_analysis)
head(cols_proj,2)
dim(cols_proj)

## review pop counts
cols_proj %>% filter(common_name ==species_for_analysis) %>% 
  arrange(common_name, -penguin_count_max) %>% 
  dplyr::select(common_name, penguin_count_max) %>% 
  head(3)

## plot colony locations
plot(st_geometry(cols_proj))
plot(ras)
plot(st_geometry(cols_proj), pch = 19, add = T) ## NOTE: plotting sf objects over rasters sometimes does not work properly.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter buffer distances for key species ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MaxDist_Sps <- buffer.dist %>% dplyr::select(Species_code, CCAMLR_Region, Chick_rearing) %>% 
  filter(Species_code == species_for_analysis)
MaxDist_Sps


## Check unique CCAMLR Regions for species
"For Marine Toolkit - not necessary"
Cols_Check <- droplevels(subset(data.frame(col_locs_proj), common_name == species_for_analysis))
table(Cols_Check$cammlr_region)
MaxDist_Sps <- droplevels(MaxDist_Sps)
table(MaxDist_Sps$CCAMLR_Region)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Convert from sf object to sp object to align with previous analyses ----
"Marine Toolkit - Need to revise / consider steps from here."
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## convert to sp object
Colonies <- as_Spatial(cols_proj)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up loop requirements & progress bar ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get data.frame of colony coordinates
"Could better specify within loop how these colony coordinates are extracted.
This step currently creates a dataframe which just happens to be in the same
order as data is processed in the loop - likely not ideal in all situations."
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOOP: Generate distribution layer for each colony ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Final review of input data for loop
head(Colonies,2)
dim(Colonies)

## Start the loop
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
  
  ## Calculate distance from each cell to the colony
  
  "MARINE TOOLKIT: TRIPLE CHECK - IS THIS DISTANCE CALCULATION AVOIDING LAND??"
  
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
                                    coords = c("Lon", "Lat"), crs=st_crs(basemap))
    
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
  p2sf <- st_as_sf(data.frame(p2), coords = c("lon", "lat"), crs = 4326) %>% st_transform(., crs = st_crs(basemap))
  #plot(p2sf, add = T, col = "red")
  
  p3sf <- st_as_sf(data.frame(p3), coords = c("lon", "lat"), crs = 4326) %>% st_transform(., crs = st_crs(basemap))
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
  pol3 <- st_transform(pol2, crs = st_crs(basemap))
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sum over each raster and generate summed distribution for the species and entire region
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"FOR MARINE TOOLKIT: This IBA section onward - needs reconsidering in context of
what is useful output for general IBA / KBA assessments."
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


