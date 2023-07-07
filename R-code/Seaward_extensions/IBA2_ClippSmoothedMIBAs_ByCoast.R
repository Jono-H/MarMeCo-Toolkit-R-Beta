################################################################################
################################################################################

## PEW Antarctica project: Preparation of final marine IBA layers for penguins
## in Antarctica

## This script follows other in the June update - see PowerPoint project diary

################################################################################
################################################################################
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(tmap)
library(units)
library(rgeos)
library(ggspatial)
library(ggmap)
library(ggrepel)
library(grid)
## get key function for north arrow alignment
source("./R_code_PEW_Antarctica/annotation_north_arrow.R")

setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica")

################################################################################
################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in key data files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Function for loading
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Load files - the smoothed 64 polygons (June 2020 data update)
IBA_main_sf <- loadRData(paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothedOverlaps_v1.Rdata", sep=""))

## Load shapefiles
Ant_high_shp <- st_read("./Data/Shapefiles/Coastline_high_res_polygon_v7.1/Coastline_high_res_polygon_v7.1.shp")
Ant_med_shp <- st_read("./Data/Shapefiles/Coastline_medium_res_polygon/Coastline_medium_res_polygon.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Specify basemap of relevance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
basemap <- Ant_high_shp


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Clip out the Antarctica Coastline from the main IBAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Check projections following renaming of object for alignment with older script version
IBA_main_sf 
st_crs(IBA_main_sf)
st_crs(basemap)
proj4string(as_Spatial(IBA_main_sf))
proj4string(as_Spatial(basemap))

## Convert to sp objects as oppose to sf (better for cropping polygons)
IBA_sp <- as_Spatial(IBA_main_sf)
ANT_sp <- as_Spatial(basemap)

## clip out overlapping part of polygons
plot(ANT_sp)
plot(IBA_sp,add=T)

## Do the cropping (TAKES HOURS/DAYS!)
IBA_main_sf_clipped <- raster::erase(IBA_sp, ## IBA polygons
                                     ANT_sp) ## Coastline shapefile


IBA_main_sf_clipped
## Save as shapefile
save(IBA_main_sf_clipped, file = "./Data/Rasters/IBAs_CellsJune2020/ALL_mIBAs_Antarctica_Clipped.Rdata")
writeOGR(obj = IBA_main_sf_clipped, dsn = "./Data/Rasters/IBAs_CellsJune2020", layer = "ALL_mIBAs_Antarctica_Clipped", driver="ESRI Shapefile", overwrite_layer = T)
IBA_main_sf_clipped <- readOGR(dsn = "./Data/Rasters/IBAs_CellsJune2020", layer = "ALL_mIBAs_Antarctica_Clipped")

## Save as Rdata file
IBA_main_clipped <- st_as_sf(IBA_main_sf_clipped)
IBA_main_clipped

## plot to check example
par(mfrow=c(1,2))
p=27
IBA_main_clipped %>% filter(Poly_ID == p) %>% st_geometry() %>% plot()
IBA_main_sf %>% filter(Poly_ID == p) %>% st_geometry() %>% plot()

## re-calculate area of polygons
IBA_main_clipped$area_m2 <- drop_units(st_area(IBA_main_clipped))
IBA_main_clipped

## Rename for consistency with rest of script below:
IBA_main_sf <- IBA_main_clipped
save(IBA_main_sf, file = "./Data/Rasters/IBAs_Cells/ALL_mIBAs_Antarctica_Clipped.Rdata")
#load(paste("./Data/Rasters/IBAs_Cells/ALL_mIBAs_Antarctica_Clipped.Rdata",sep=""))
#IBA_main_sf

"Will now need to produce report. Will first need to manually assign polygons to Clockwise labelling around Antarctica"
