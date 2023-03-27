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
## Estimate of appropriate buffer size
## Polygon of land mass for study area    #edit to work with rnaturalearth

## OUTPUT data: 
## Raster of study area at correct resolution

## Script authors:
"Jonathan Handley, Bethany Clark"

##Clear R
rm(list=ls())

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library(tidyverse)
library(sf)

library(terra)
#library(geosphere)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Colony data: location, abundance estimate, bearing pre-defined
load("./data-testing/data.penguins.antarctica.Rdata")
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
## Generate raster from map for distance calculations ----
## Create blank raster of SPECIFIED resolution that covers full extent of region
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Specified resolution of grid cell in meters
"For Marine Toolkit, need to provide guidance on choice of resolution. 
Currently, 5km aligned with same scale as the Critchley et al. 2018 paper."
spec_res = 5000
#looks alright for EMP, but may be too large for GEP - Beth
#spec_res = 1000 #added for GEP


#could take from % of publuished max dist, and then round them based on a % of the max


## border of antarctica and key islands
basemap ## manually get bounding box info (bbox)

## Create larger area by buffer in meters, to account 
## for seaward area beyond border land masses
#(max(buffer.dist$Chick_rearing)*1000)
#ant_buff <- 220000
ant_buff <- ((max(buffer.dist$Chick_rearing)*1000)) + spec_res

## Create blank raster
#ras <- raster::raster(xmn = -2661818 - (ant_buff), xmx = 2748309 + (ant_buff), 
#                      ymn = -2491520 - (ant_buff), ymx = 2322031 + (ant_buff), 
#                      resolution = spec_res)
## set projection of raster
#raster::crs(ras) <- raster::crs(basemap) # match projection of raster to map

bounds <- st_bbox(col_locs_proj)
ras <- terra::rast(xmin = bounds[[1]] - ant_buff, 
                   ymin = bounds[[2]] - ant_buff, 
                   xmax = bounds[[3]] + ant_buff, 
                   ymax = bounds[[4]] + ant_buff, 
                   resolution = spec_res,
                   crs = st_crs(basemap)$wkt)

## convert the basemap, currently in polygon format, to a raster
"NOTE: Operation will take time depending on size of polygon data and 
detail of borders. i.e. Higher resolution maps take longer to process."
#mask <- raster::rasterize(basemap, ras)
basemap_vector <- vect(basemap)
mask <- terra::rasterize(basemap_vector, ras)

## Simplify raster
## NOTE: rasterize will set ocean to NA because it 
## covers an area of cells where
## there are no polygons, so inverse it and set water to "1"
#plot(mask)
ras <- is.na(mask)
plot(ras)

## land is equal to zero because it is "NOT" NA. 
## i.e. there is already a polygon there.
## Set land to 2 to make it more expensive to cross
ras[ras==0] <- 2
plot(ras)

## save raster for comparing in arcmap
#writeRaster(ras, "./Data/Rasters/Antarctica_5x5km/Antarctica_HighRes_5x5km.tif", overwrite = T)
terra::writeRaster(ras, "./data-testing/Antarctica_5x5km/Antarctica_HighRes_5x5km_terra.tif",
                   overwrite=T)
dir.create("./data-testing/Antarctica_1x1km/")
terra::writeRaster(ras, "./data-testing/Antarctica_1x1km/Antarctica_HighRes_1x1km_terra.tif",
                   overwrite=T)
