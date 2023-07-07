################################################################################
################################################################################

## PEW Antarctica project: Creating a decision work flow for assessing if colony
## clusters trigger IBA criteria. I.e. instead of 1 colony triggering criteria,
## check if colonies close by to each other would trigger criteria when you sum
## their populations. The radius around colonies might be set by the possible
## foraging radious of the method, as defined from tracking data and reviewed in
## the literature.

## Jono Handley (jonathan.handley@birdlife.org/jonathan.m.handley@gmail.com)
## Oct 2019

## updated June 2020


################################################################################
################################################################################
library(tidyverse)
library(sf)
library(raster)
library(units)

setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Call in data for species of interest and create raster stack
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dir("./Data/Rasters/IBAs_CellsJune2020")

species = "EMP"
  
## generate a list of input rasters ("grids")
RasterList <- grep(species,
                   dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="_C_dist16.tif$",full.names=T),
                   value = T)

## Check list
RasterList

## create a raster stack from the input raster files 
RasterStack <- raster::stack(RasterList)

## Plot a layer to check
RasterStack
plot(RasterStack[[4]]) # change number for different layers

## sum over the raster stack 
RasterSum <- sum(RasterStack)

## Plot to check
RasterSum
plot(RasterSum)

## Get only those cells which met IBA criteria at least 3 times for population 
## values (min, max, median, most recent)
RasterSum_temp <- RasterSum
RasterSum_temp@data@values
summary(RasterSum_temp@data@values)
unique(RasterSum_temp@data@values)

## save temp file for checking
#writeRaster(RasterSum_temp, 
#            paste("./Data/Rasters/IBAs_CellsJune2020/",species,"_AggregatedTemp_dist16.tif", sep=""), overwrite = T)


## amend cells
RasterSum_temp[RasterSum_temp<3] <- 0
RasterSum_temp[RasterSum_temp>=3] <- 1
RasterIBA <- RasterSum_temp

## Plot to check final layer
RasterIBA
plot(RasterIBA)

## Save final species layers (repeat for each species)
writeRaster(RasterIBA, 
            #paste("./Data/Rasters/IBAs_CellsJune2020/Aggregated_dist16",species,"_Aggregated_dist16.tif", sep=""), overwrite = T)
            paste("./Data/Rasters/IBAs_CellsJune2020/",species,"_Aggregated_dist16.tif", sep=""), overwrite = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Merge layers for all species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## generate a list of input rasters ("grids") - note use of $ to avoid inclusion of other file types
FinalList <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="_Aggregated_dist16.tif$",full.names=T)

## Check list
FinalList
#FinalList <- FinalList[c(1,4,5,6)] # remove final plot (ALL) just in case you are re-running analysis
FinalList

## create a raster stack from the input raster files 
FinalStack <- raster::stack(FinalList)

## Plot a layer to check
FinalStack
plot(FinalStack[[3]]) # change number for different layers (species layer)

"NOTE: If two adjacent cells from different species both have value 1 then instead of
having two adjecent single species IBAs, you may end up with one large one. This is a
mild headache that I need to deal with! See [ugly script] below"

## Sum over all layers
FinalSum <- sum(FinalStack, na.rm=T)
plot(FinalSum)
## Make 0 values NA
FinalSum[FinalSum==0] <- NA
plot(FinalSum)

## Save final Antarctica IBA overall layer for where there are two or one species
writeRaster(FinalSum, 
            paste("./Data/Rasters/IBAs_CellsJune2020/",
                  "ALL",
                  "_Aggregated_dist16.tif", sep=""), overwrite = T)


## Test of raster cell overlaps with a specific layer?
## First get only raster cells with value of 1
FinalSum_single <- FinalSum
FinalSum_single[FinalSum_single==2] <- NA
plot(FinalSum_single)

## Save final Antarctica IBA overall layer for where there is only 1 species
writeRaster(FinalSum_single, 
            paste("./Data/Rasters/IBAs_CellsJune2020/",
                  "ALL_1species",
                  "_Aggregated_dist16.tif", sep=""), overwrite = T)

## Now, get where those cells are only related to each species
FinalStack

## ADP
FinalStack[[1]]
plot(FinalStack[[1]])
ADP <- mask(FinalStack[[1]], FinalSum_single)
plot(ADP)

## CHP
FinalStack[[2]]
plot(FinalStack[[2]])
CHP <- mask(FinalStack[[2]], FinalSum_single)
plot(CHP)

## EMP
FinalStack[[3]]
plot(FinalStack[[3]])
EMP <- mask(FinalStack[[3]], FinalSum_single)
plot(EMP)

## GEP
FinalStack[[4]]
plot(FinalStack[[4]])
GEP <- mask(FinalStack[[4]], FinalSum_single)
plot(GEP)

## Second, get only raster cells with value of 2 (i.e. double species in a site)
FinalSum_double <- FinalSum
FinalSum_double[FinalSum_double==1] <- NA
plot(FinalSum_double)

## Change values for single species layers
ADP[ADP==1] <- 3
ADP[ADP==0] <- NA

CHP[CHP==1] <- 4
CHP[CHP==0] <- NA

EMP[EMP==1] <- 5
EMP[EMP==0] <- NA

GEP[GEP==1] <- 6
GEP[GEP==0] <- NA

plot(ADP)
plot(CHP)
plot(EMP)
plot(GEP)
plot(FinalSum_double)

## Now merge all these layers
FinalSumSpecies <- merge(FinalSum_double, ADP, CHP, EMP, GEP)
plot(FinalSumSpecies)

## save temp file for checking
writeRaster(FinalSumSpecies, 
            paste("./Data/Rasters/IBAs_CellsJune2020/","All_mIBA_SpeciesRasterCells_Ant.tif", sep=""), overwrite = T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Convert rasters to polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## From JH2 Script
#r <- rasterToPolygons(FinalSum)
#plot(r)
#r_dissolved <- rasterToPolygons(FinalSum, dissolve = T)
#plot(r_dissolved)

## From JH3 onward Script
r <- rasterToPolygons(FinalSumSpecies)
plot(r)
r_dissolved <- rasterToPolygons(FinalSumSpecies, dissolve = T)
plot(r_dissolved)

## Convert to simple features object and save as shapefile
s <- st_as_sf(r)
s_dissolved <- st_as_sf(r_dissolved)
plot(s)
plot(s_dissolved)
## Save as each raster cell as a polygon
st_write(s, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_poly.shp", sep=""), delete_layer = T)
## Here you are saving the dissolved polygons. I.e. all the polygons where only 1 species overlaps is saved as an entire polygon
st_write(s_dissolved, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_DissolvedPoly.shp", sep=""), delete_layer = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### dissolve polygons into single border where there is overlap
### i.e. get all the entire area which encompasses all IBAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rmapshaper)

r_merge <- ms_dissolve(r_dissolved)
plot(r_merge)

## Convert to simple features object and save as shapefile
s_merge <- st_as_sf(r_merge)
plot(s_merge)
st_write(s_merge, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_MergedPoly.shp", sep=""), delete_layer = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Split single part polygon to multi-part
"Do this for DISSOLVED polygons"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r_multi <- ms_explode(r_merge)
s_multi <- st_as_sf(r_multi)
plot(s_multi)
st_write(s_multi, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_MultiPoly.shp", sep=""), delete_layer = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Split single part polygon to multi-part
"Do this for NON - DISSOLVED polygons"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r_multi <- ms_explode(r_dissolved)
s_multi <- st_as_sf(r_multi)
plot(s_multi)
st_write(s_multi, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_MultiPoly.shp", sep=""), delete_layer = T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Determine size of each polygon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s_area <- st_area(s_multi)
class(s_area)
s_multi2 <- s_multi %>% mutate(area_m2=s_area)
st_write(s_multi2, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_MultiPoly2.shp", sep=""), delete_layer = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Check size of polygons that might be small and isolated
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(s_multi2$area_m2)
sort(s_multi2$area_m2)
sort(s_multi2$area_m2/1000000)
hist(sort(s_multi2$area_m2/1000000),breaks = 100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Remove units from area that are only a single cell large
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## drop class units
s_multi2$area_m2 <- drop_units(s_multi2$area_m2)

## remove smallest units (area of single cell)
s_major <- filter(s_multi2, area_m2 > min(area_m2))

## Plot to compare
plot(s_major)
plot(st_geometry(s_major))
plot(st_geometry(s_multi2))
plot(st_geometry(s_major), add=T, col="red")

## Check number of layers
dim(s_multi2)
dim(s_major)

## Check next smallest unit
min(s_major$area_m2)
## save as shapefile for checking
st_write(s_major, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_Major.shp", sep=""), delete_layer = T)
## save as R datafile for analysis
save(s_major,  file=paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_Major.Rdata", sep=""))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KEEP units from area that are only a single cell large - for comparison
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## KEEP smallest units (area of single cell)
s_minor <- filter(s_multi2, area_m2 == min(area_m2))
## save as shapefile for checking
st_write(s_minor, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_Minor.shp", sep=""), delete_layer = T)


## See next script for assigning population values within each IBA polygon

