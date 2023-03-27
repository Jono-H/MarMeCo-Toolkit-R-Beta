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
#library(raster) #need to translate to terra
#library(gdistance) #need to update?
#library(rgdal) #need to translate to sf
library(terra)
library(circular) #for bearings stats
library(geosphere)

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

## Map (Beth to add for when people don't have their own basemaps)
#library(rnaturalearth)
#library(rnaturalearthdata)
#worldrn <- ne_countries(scale = "medium", returnclass = "sf") %>%
#  st_make_valid()

## clean up working environment
rm(Ant_high_shp, MaxDist_All, data.penguins.antarctica)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load functions ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calling the update to gridDistance function within Raster package
## Jono Handley updated function to allow for better movement across cells (directions = 16)
#source("./R-functions/gridDistance_directions16.R")

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
## Load raster from map for distance calculations ----
## Blank raster of SPECIFIED resolution that covers full extent of region
## If you have already created the necessary raster - load it here.
## Otherwise go to "Seaward_extension_create_background_raster.R"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ras <- raster::raster("./data-testing/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif")
ras <- terra::rast("./data-testing/Antarctica_5x5km/Antarctica_HighRes_5x5km_terra.tif")
#ras <- terra::rast("./data-testing/Antarctica_1x1km/Antarctica_HighRes_1x1km_terra.tif")
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

"From Critchley et al 2018"
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
"NOTE: This needs simplification or improving. Currently, you have to specify
the different combinations manually for each run of the loop. So that's 4 penguin
species and 4 count types = 16 different runs of the loop"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#create folders for maps to check results
dir.create("./seaward_extension_results")
dir.create("./seaward_extension_results/bearing_method_maps")

species_list <- unique(col_locs_proj$common_name) #GEP, EMP, ADP, CHP

## select species ####
species_number <- 3
species_for_analysis <- species_list[species_number]

## count type
count_type <- "penguin_count_recent"

map_folder <- paste0("./seaward_extension_results/bearing_method_maps/",species_for_analysis,
                  "_",count_type)
dir.create(map_folder)

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
#plot(st_geometry(cols_proj))
#plot(ras)
plot(st_geometry(cols_proj), pch = 19, add = T) ## NOTE: plotting sf objects over rasters sometimes does not work properly.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter buffer distances for key species ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MaxDist_Sps <- buffer.dist %>% dplyr::select(Species_code, 
                                             CCAMLR_Region, 
                                             Chick_rearing,
                                             Incubation) %>% 
  filter(Species_code == species_for_analysis)
MaxDist_Sps

#32 likely too small for Gentoo?
#Find maximum possible buffer across all breeding stages
Max_MaxDist_Sps <- max(c(MaxDist_Sps$Chick_rearing,
                         MaxDist_Sps$Incubation),na.rm = T)

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
Colonies_Sps <- data.frame(sp::coordinates(Colonies))
dim(Colonies_Sps)
head(Colonies_Sps)
nrow(Colonies_Sps)

# Create a stack to store each raster in the loop, 
# these will then be summed at the end
ColonyStack <- raster::stack()
raster::nlayers(ColonyStack)

# Create progress bar to track percentage of loops completed
# This is useful when there are a large number of colonies 
# but it does slow down the loop slightly
#pb <- winProgressBar(title="Raster loop progress bar", label="0% done", 
#                     min=0, max=100, initial=0)
#This caused my R studio to crash. I have just replaced by add total number
#and % to loop print 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOOP: Generate distribution layer for each colony ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Final review of input data for loop
head(Colonies,2)
dim(Colonies)

#Create new data frame to store colony bearing info
cols_df <- as.data.frame(Colonies)
cols_df$auto_bearing_n_cells <- NA
cols_df$auto_bearing_sum_dist <- NA
cols_df$auto_bearing_n_cells_kappa <- NA
cols_df$auto_bearing_sum_dist_kappa <- NA

#21 has error
i <- 1
## Start the loop
for (i in 1:length(Colonies)){
  
  print(paste0("Colony number ", i," of ",length(Colonies),", ",
              round(100*(i/length(Colonies))),"% complete"))
  
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
  #pp <- raster::extract(antrans,colonypoint, cellnumbers=TRUE) 
  pp <- terra::extract(antrans,colonypoint, cells=TRUE) 
  #pp2 <- rowColFromCell(antrans, pp) # used to work in earlier version
  
  ## convert that cell to a value of 3 
  #antrans[pp2[1,1],pp2[1,2]] <- 3 # used to work in earlier version
  #antrans[pp[1,1]] <- 3 #raster
  antrans[pp$cell] <- 3
  
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
  #Dist.LandMask <- raster::gridDistance(x=antrans, # x specifies which raster to use
  #                                     origin=3, # origin specifies which cell to determine distance from
  #                                     omit=2) # omit says which cell values to avoid while determining distance
  Dist.LandMask <- terra::gridDistance(x=antrans, # x specifies which raster to use
                                       origin=3, # origin specifies which cell to determine distance from
                                       omit=2) # omit says which cell values to avoid while determining distance
  rm(antrans)
  #maxValue(Dist.LandMask)
  #if (raster::maxValue(Dist.LandMask) == 0) print("WARNING: Check for NAs in final output of ColonyStack. Your origin(colony) may be to far in land. Therefore, your origin is completely buffered by omit cells(land cells), and no distance calculation for cells at sea can be performed") else print("Distance calculation successful")
  if (terra::minmax(Dist.LandMask)[2] == 0) print("WARNING: Check for NAs in final output of ColonyStack. 
                                                  Your origin(colony) may be to far in land. 
                                                  Therefore, your origin is completely buffered by omit cells(land cells), 
                                                  and no distance calculation for cells at sea can be performed") else print(
                                                    "Distance calculation successful")
  
  plot(Dist.LandMask)
  
  #-----Change name back to Critchley code------"
  #R <- Dist.LandMask
  
  ## JH: Get MaxDist for colony of interest, dealing with some annoying factor to conversions
  MaxDist <- subset(MaxDist_Sps, CCAMLR_Region == as.character(paste((Colonies@data[i,]$cammlr_region))))
  MaxDist <- (MaxDist$Chick_rearing[1])*1000
  
  #Or species maximum? Max_MaxDist_Sps  
  
  ## Get colony coordinates only and make buffer to crop raster
  col_coords <- st_as_sf(data.frame(Lon = Colonies@coords[i,][1], Lat = Colonies@coords[i,][2]),
                         coords = c("Lon", "Lat"), crs=st_crs(basemap))
  max_dist_buffer <- st_buffer(col_coords, dist = MaxDist)
  plot(max_dist_buffer, add = T)
  
  ## any cell further than MaxDist, make it NA
  Dist.LandMask[Dist.LandMask > MaxDist] = NA

  #crop to reduce plot size (otherwise memory error when saving on laptop with 8gb ram)
  Dist.LandMask <- terra::crop(Dist.LandMask,
                                    max_dist_buffer,
                                    snap = "out")
  
  ## normalise to 0 and 1 probability of occurence
  Dist.LandMask <- -1*(Dist.LandMask/MaxDist)+1 
  
  #can change to exponential or other decay function instead. e.g. 
  
  plot(Dist.LandMask)
  plot(max_dist_buffer, add = T)
  
  ## Calculate distance from each cell to the colony
  
  "MARINE TOOLKIT: TRIPLE CHECK - IS THIS DISTANCE CALCULATION AVOIDING LAND??" #Yes
  
  #dist.R <- distanceFromPoints(R, (Colonies[i,]))
  #dist.R <- raster::distanceFromPoints(R, (Colonies_Sps[i,]))
  #dist.R <- terra::distance(R, matrix(Colonies_Sps[i,])) #not working - Beth
  
  #plot(dist.R)
  
  #Dist.LandMask<- Dist.LandMask*(1/log(dist.R)) # weight areas closer to the colony of higher importance
  
  #plot(Dist.LandMask)
  
  # normalise to 0 and 1 probability of occurence (1 is closest to colony)
  #Dist.LandMask <- ((Dist.LandMask-cellStats(Dist.LandMask,"min"))/
  #                 (cellStats(Dist.LandMask,"max")-cellStats(Dist.LandMask,"min"))) 
  
  #plot(Dist.LandMask)
  #writeRaster(Dist.LandMask, 
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

  #I am currently not aiming for pizza, but instead oval shape - Beth
  
  #plot(Dist.LandMask)
  #plot(SpatialPoints(Colonies_Sps[i,]), pch = 19, add = T)
  
  #Beth start bearing calcs ####
  
  #plot(R, xlim = c(2216399-(1.1*MaxDist),2216399+(1.1*MaxDist)),
  #     ylim = c(518627.4-(1.1*MaxDist),518627.4+(1.1*MaxDist)))
  
  #plot(col_coords, add = T, pch = 16)
  
  #plot(max_dist_buffer, add = T)
  
  #destPoint in geosphere package requires wgs84
  col_coords_wgs84 <- st_transform(col_coords, crs = 4326)
  
  #create point at each of 360 degrees around the MaxDist circle buffer
  bearing_end <- geosphere::destPoint(p = unlist(col_coords_wgs84$geometry), 
                                      d = MaxDist,
                                      b = 1:360)
  #convert back to basemap crs
  bearing_end_sf <- st_as_sf(data.frame(bearing_end),
                             coords = c("lon", "lat"), crs=st_crs(4326))
  bearing_end_proj <- st_transform(bearing_end_sf, 
                                   crs = st_crs(basemap))
  #set up dataframe for loop to assess the available sea area
  #between the colony and each point around the buffer
  bearings <- 1:360
  radial_lines <- as.data.frame(bearings)
  radial_lines$dist_sum <- NA
  
  head(radial_lines)
  j=1
  for(j in 1:length(bearings)){
    #Create line between colony and point on buffer
    bline <-  st_cast(st_union(bearing_end_proj$geometry[j],
                               col_coords$geometry[1]),
                      "LINESTRING")
    #Convert object to vect so it can interact with raster
    bline_v <- vect(bline)
    #plot(bline_v, add = T)
    
    #Extract the cells that the line overlaps with
    sea_cells <- terra::extract(Dist.LandMask,bline_v)
    
    #Store the total number of cells under the line, the number containing
    #the sea and the dist from each to the colony via marine grid cells
    radial_lines$dist_sum[j] <- round(sum(sea_cells$layer, na.rm = T))
    #print(length(bearings)-j)
  }

  #find the circular mean for sum grid dist traveled, and the
  #kappa (concentration paramenter) 
  dat_angles_s <- rep(radial_lines$bearings,radial_lines$dist_sum)
  dat_circ_s <- circular::circular(dat_angles_s,
                                 type = "angles",
                                 units = "degrees")
  vm_s <- mle.vonmises(dat_circ_s)
  mean_angle_s <- as.numeric(vm_s$mu)
  cols_df$auto_bearing_sum_dist[i] <- ifelse(mean_angle_s > 0,
                                             mean_angle_s, 360 + mean_angle_s)
  cols_df$auto_bearing_sum_dist_kappa[i] <- as.numeric(vm_s$kappa)
  #hist(dat_angles_s)
  #abline(v = cols_df$auto_bearing_sum_dist[i], col = "blue")

  #predict von Mises distribution for colony
  vm_s_result <- geostats::vonMises(a = 1:360,
                          mu = as.numeric(vm_s$mu),
                          kappa = as.numeric(vm_s$kappa),
                          degrees = T)
  #Scale to max distance & save to data frame
  radial_lines$vm_s_dist <- vm_s_result/max(vm_s_result)*MaxDist
  
  #plot to check match up with histogram of angles
  #par(mfrow=c(2,1))
  #plot(radial_lines$vm_s_dist, ylim = c(0,max(radial_lines$vm_s_dist)),
  #     main = "von Mises model (km)")
  #hist(dat_angles_s)
  #abline(v = cols_df$auto_bearing_sum_dist[i], col = "blue")
  #par(mfrow=c(1,1))

  #create projected points around the perimeter using dist sum
  bearing_oval_s <- geosphere::destPoint(p = unlist(col_coords_wgs84$geometry), 
                                            d = radial_lines$vm_s_dist,
                                            b = 1:360)
  bearing_oval_s_poly <- bearing_oval_s %>%
    data.frame() %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = st_crs(basemap))

  #Create line for colony direction line to plot on the map
  bearing_end_final_proj_sum <- geosphere::destPoint(p = unlist(col_coords_wgs84$geometry), 
                                                 d = MaxDist,
                                                 b = mean_angle_s) %>%
    data.frame() %>%
    st_as_sf(coords = c("lon", "lat"), crs=st_crs(4326)) %>%
    st_transform(crs = st_crs(basemap))
  col_dir_line_v_sum <- st_cast(st_union(bearing_end_final_proj_sum$geometry[1],
                                     col_coords$geometry[1]),
                            "LINESTRING") %>% vect()
  
  #create colony direction line for human estimated direction
  bearing_end_final_proj_original <- geosphere::destPoint(p = unlist(col_coords_wgs84$geometry), 
                                                     d = MaxDist,
                                                     b = cols_df$bearing[i]) %>%
    data.frame() %>%
    st_as_sf(coords = c("lon", "lat"), crs=st_crs(4326)) %>%
    st_transform(crs = st_crs(basemap))
  col_dir_line_v_original <- st_cast(st_union(bearing_end_final_proj_original$geometry[1],
                                         col_coords$geometry[1]),
                                "LINESTRING") %>% vect()
  
  #Save out a plot of the raster for each colony with 
  #maximum distance buffer and estimated colony direction
  #Blue = percentage of cells containing sea
  #Red = sum of distance from colony via marine grid cells
  png(filename = paste0(map_folder,"/colony", i, ".png"))
  plot(Dist.LandMask)
  plot(col_coords, add = T, pch = 16)
  plot(max_dist_buffer, add = T)
  plot(col_dir_line_v_original, add = T, col = "black")
  plot(col_dir_line_v_sum, add = T, col = "blue")
  plot(bearing_oval_s_poly, add = T, border = "blue")
  dev.off()
  
  #need to add circular variance/kappa to plots
  
  write.csv(cols_df,paste0("seaward_extension_results/bearing_method_maps/auto_bearings_",species_for_analysis,
            "_",count_type,".csv"), row.names = F)
}  
## convert to WGS84


#Compare the calculated bearings to those estimated manually
plot(cols_df$bearing,cols_df$auto_bearing_sum_dist)

bearing_circ <- circular::circular(cols_df$bearing,
                                   type = "angles",
                                   units = "degrees")
auto_bearing_sum_dist_circ <- circular::circular(cols_df$auto_bearing_sum_dist,
                                   type = "angles",
                                   units = "degrees")
#Run circular correlation
cor.circular(bearing_circ,auto_bearing_sum_dist_circ)
#Beth end ####

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




## -----------------------------------------------------------------------------
# Plot distribution map
## -----------------------------------------------------------------------------

par(ask = F)
plot(output)
plot(st_geometry(Ant_med_shp), add=T)
points(Colonies, pch = 19, cex = 0.75, col = 2)


