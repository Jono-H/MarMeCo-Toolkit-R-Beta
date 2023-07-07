################################################################################
################################################################################

## PEW Antarctica project: Having determined final boundaries of IBA polygons,
## now need to assign population values within each boundary. Need to assign
## which species are present and tha max and min values accordingly in each polygon

## Jono Handley (jonathan.handley@birdlife.org/jonathan.m.handley@gmail.com)
## Jan 2020

## Updates June 2020

"NB: From V3 onward, trying to include detail about individual colonies that make
up each marine IBA"

################################################################################
################################################################################
library(tidyverse)
library(sf)
library(raster)
library(units)
library(rmpshpr)
library(smoothr)
library(rgdal)

setwd("C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Pew_Antarctica")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in major IBA polygons 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBA_maj <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major.shp")
IBA_maj

load(paste("./Data/Rasters/IBAs_CellsJune2020/","ALL", "_Aggregated_Major.Rdata", sep=""))
IBA_maj <- s_major


##rename variables and amend IDs
IBA_maj <- IBA_maj %>% rename(poly_ID = layer) %>% mutate(poly_ID = 1:length(poly_ID))
max(IBA_maj$poly_ID)
plot(st_geometry(IBA_maj))
head(IBA_maj)
tail(IBA_maj)
unique(IBA_maj$poly_ID)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in main population data associated with major polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load(paste("./Data/Rasters/IBAs_CellsJune2020/","66sites_mIBA_Antarctica_Info_v1.Rdata", sep=""))
head(MAIN_IBA_info,9)
tail(MAIN_IBA_info,4)
unique(MAIN_IBA_info$Poly_ID)
## Align with naming convention
IBA_main_info <- MAIN_IBA_info

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Read in colony population data associated with major polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBA_colony_info <- read.csv(paste("./Data/Antarctica_Penguin_mIBAs/ColoniesInMainIBAs_June2020.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First need to confirm which species contributed to which IBAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IBA_main_sf <- IBA_maj
head(IBA_main_info,9)

## Create blank data frame
df_names <- sort(unique(IBA_main_info$Species))
df <- data.frame(matrix(ncol = length(df_names), nrow = 0))
colnames(df) <- df_names
df <- data.frame(df, Poly_ID = as.numeric())

## Single example for checking
p = 29
temp <- IBA_main_info %>% filter(Poly_ID == p) %>% distinct(Species)
df_species <- data.frame(ADP = ifelse("ADP" %in% temp$Species,1,0),
                         CHP = ifelse("CHP" %in% temp$Species,1,0),
                         EMP = ifelse("EMP" %in% temp$Species,1,0),
                         GEP = ifelse("GEP" %in% temp$Species,1,0),
                         Poly_ID = p)

df_species
plot(st_geometry(IBA_maj))
poly1 <- IBA_maj %>% filter(poly_ID == p)
plot(st_geometry(poly1), add=T, col="red")
plot(st_geometry(poly1))


## assign species for all polygons
for(p in 1:max(IBA_main_sf$poly_ID)){
  temp <- IBA_main_info %>% filter(Poly_ID == p) %>% distinct(Species)
  df_species <- data.frame(ADP = ifelse("ADP" %in% temp$Species,1,0),
                           CHP = ifelse("CHP" %in% temp$Species,1,0),
                           EMP = ifelse("EMP" %in% temp$Species,1,0),
                           GEP = ifelse("GEP" %in% temp$Species,1,0),
                           Poly_ID = p)
  df <- rbind(df,df_species)
}

df
dim(df)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Now need to join this information back to the spatial layer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBA_main_sf
IBA_main_sf2 <-  IBA_main_sf %>% arrange(poly_ID)
IBA_main_sf2

## plot to check examples and associated dataframe which would get joined
p=29
df %>% filter(Poly_ID == p)
plot(st_geometry(IBA_maj))
IBA_main_sf2 %>% filter(poly_ID == p) %>% st_geometry() %>% plot(add=T, col="red")

## rename variables to match
IBA_main_sf3 <- IBA_main_sf2 %>% rename(Poly_ID = poly_ID)

## Perform the join
IBA_main_sf4 <- inner_join(IBA_main_sf3, df, by="Poly_ID")
head(IBA_main_sf4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Save Shapefiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Old saved name when scripts were ordered differently (had done other steps beforehand)
#st_write(IBA_main_sf4, paste("./Data/Rasters/IBAs_Cells/","ALL_mIBAs_Antarctica_Clipped_v2.shp", sep=""), delete_layer = T)
## New saved name
st_write(IBA_main_sf4, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_mIBAs_Antarctica_SpsInIBAs_v1.shp", sep=""), delete_layer = T)
save(IBA_main_sf4, file = paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_mIBAs_Antarctica_SpsInIBAs_v1.Rdata", sep=""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Smoothing out polygons to make them more visually appealing"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Or to smooth out jagged polygons... but what about polygons directly adjacent to each other?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"SEE OLD SCRIPT: IBA_PopNumsToIBAs_JH... to read/see/learn more about different 
smoothing options. e.g. ksmooth, spline, chaikin, and testing which was appropriate"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loop to smooth over polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## rename file to align naming convetions of old scripts
IBA_maj <- IBA_main_sf4
head(IBA_maj)

## Poly ID
t = 1
## SUbset polygon
poly1 <- IBA_maj %>% filter(Poly_ID == t)

## Access number of vertices for polygon
nvert <- length(unlist(poly1$geometry[[1]][1]))/2
nvert

## Get the smooth_level factor
smooth_level <- if (nvert>=100) {
  6
} else if (nvert<100 & nvert>=50) {
  5
} else if (nvert<50 & nvert>=40) {
  4
} else if (nvert<40 & nvert>=30) {
  3
} else if (nvert<30 & nvert>=10) {
  2
} else if (nvert<10 & nvert>0) {
  1
}  
smooth_level

## Kernel smooth
plot(st_geometry(poly1))
poly_smooth <- smooth(poly1, method = "ksmooth", smoothness = smooth_level) # 1 is default
plot(st_geometry(poly_smooth), border = "#E41A1C", lwd = 2, add = TRUE)
poly1 <- poly_smooth


for(t in 2:max(IBA_maj$Poly_ID)){
  ## SUbset polygon
  poly_temp <- IBA_maj %>% filter(Poly_ID == t)
  
  ## Access number of vertices for polygon
  nvert <- length(unlist(poly_temp$geometry[[1]][1]))/2
  
  ## Get the smooth_level factor
  smooth_level <- if (nvert>=100) {
    6
  } else if (nvert<100 & nvert>=50) {
    5
  } else if (nvert<50 & nvert>=40) {
    4
  } else if (nvert<40 & nvert>=30) {
    3
  } else if (nvert<30 & nvert>=10) {
    2
  } else if (nvert<10 & nvert>0) {
    1
  }  
  
  
  ## Kernel smooth
  poly_smooth <- smooth(poly_temp, method = "ksmooth", smoothness = smooth_level) # 1 is default
  
  ##rbind polygons together
  poly1 <- rbind(poly1, poly_smooth)
  
}

## Check
poly1
dim(poly1)
## Plot example
plot(st_geometry(poly1)[[4]])

## Compare with original pixelated version
plot(st_geometry(poly1))
plot(st_geometry(IBA_maj))

## rename just in case
all_poly_smooth <- poly1

## Save shapefile and explore in Arcmap
st_write(all_poly_smooth, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_Aggregated_Major_Smooth_SpsInIBAs_v1.shp", sep=""), delete_layer = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"UPDATED SCRIPT (where I erase parts of overlapping polygons): START FROM HERE"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_poly_smooth <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_Smooth_SpsInIBAs_v1.shp")
head(all_poly_smooth)
dim(all_poly_smooth)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dealing with slight overlaps of polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Use intersection to determine where polygons are overlapping
## note the new column produced: n.overlaps
s_inter <- st_intersection(all_poly_smooth)
s_inter$n.overlaps
s_inter$origins

dp <- data.frame(s_inter)
head(dp)
tail(dp)
dp
## Find the polygons that are overlapping - get the lists that have more than two objects in them
str(dp$origins)
dp$origins[[59]] 
dp$origins[[60]] 
## List of overlapping polygons
unique(lengths(dp$origins)) 

## Original 49 polygons onl ever had two overlapping polygons
#dp_over <- subset(dp$origins, lengths(dp$origins) == 2) # note lengths not length! differenet functions

## Now with 66 polygons, we have cases where some polygons overlap in 3s
dp_over <- subset(dp$origins, lengths(dp$origins) > 1) # note lengths not length! differenet functions

str(dp_over)
length(dp_over)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Work with overlapping polygon pairs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(dp_over)
unlist(dp_over)
dp_unique <- unique(unlist(dp_over))
length(dp_unique)
dp_unique

## First select all those polygons which do not overlap
s_non <- all_poly_smooth %>% filter(!(Poly_ID %in% dp_unique))

## Now select polygons where there is overlap
s_over <- all_poly_smooth %>% filter(Poly_ID %in% dp_unique)

plot(st_geometry(s_non))
plot(st_geometry(s_over))

"Owing to updated 66 IBAs in June 2020 - and checking in ArcMap, polygons 4, 5, 29
seem to be doing something weird. Also, polygon 25 completely engulfs these polygons"
## Check the MAIN colony info associated with the IBAs
IBA_main_info %>% filter(Poly_ID == 4)
IBA_main_info %>% filter(Poly_ID == 5)
IBA_main_info %>% filter(Poly_ID == 29)

## Check the INDIVIDUAL colony info associated with the IBAs
IBA_colony_info %>% filter(Poly_ID == 4)
IBA_colony_info %>% filter(Poly_ID == 5)
IBA_colony_info %>% filter(Poly_ID == 29)

"Having done some checking in excel and ArcMap, it seems for whatever reason that
polygons 4 and 5 are inside of 29. 29 seems to account for the lower and upper counts
of each count type.

polygon 25 contains polygon 29 which contains polygons 4 and 5 - hence the triple
overlap. Solution seems to be to remove polygons 4 and 5"

unique(all_poly_smooth$Poly_ID)
## arrange, remove points 4 and 5, rename
all_poly_smooth <- all_poly_smooth %>% arrange(Poly_ID) %>% filter(!Poly_ID %in% c(4,5))
## re-number Poly_ID from 1 again
all_poly_smooth$Poly_ID <- 1:nrow(all_poly_smooth)
unique(all_poly_smooth$Poly_ID)
dim(all_poly_smooth)




"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"NB NB NB - need to remove and re-assign polygon IDs to other key files to make sure everything matches!"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


head(IBA_maj)
dim(IBA_maj)
## arrange, remove points 4 and 5, rename
IBA_maj <- IBA_maj %>% arrange(Poly_ID) %>% filter(!Poly_ID %in% c(4,5))
## re-number Poly_ID from 1 again
IBA_maj$Poly_ID <- 1:nrow(IBA_maj)
unique(IBA_maj$Poly_ID)
dim(IBA_maj)

## Plot to test and check these layers still overlap
t = 47
IBA_maj %>% filter(Poly_ID == t) %>% st_geometry() %>% plot()
all_poly_smooth %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")

## save updated layers
#st_write(all_poly_smooth, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_Aggregated_Major_Smooth_SpsInIBAs_v2.shp", sep=""), delete_layer = T)
all_poly_smooth <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_Smooth_SpsInIBAs_v2.shp")

#st_write(IBA_maj, paste("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_v2.shp", sep=""), delete_layer = T)
#st_write(IBA_maj, paste("./Data/Rasters/IBAs_CellsJune2020/ALL_mIBAs_Antarctica_SpsInIBAs_v2.shp", sep=""), delete_layer = T)
IBA_maj <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_v2.shp")
#save(IBA_maj, file = paste("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_v2.Rdata", sep=""))
#save(IBA_maj, file = paste("./Data/Rasters/IBAs_CellsJune2020/ALL_mIBAs_Antarctica_SpsInIBAs_v2.Rdata", sep=""))
load(file = paste("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_v2.Rdata", sep=""))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Amend main population data associated with major polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## FIRST for MAIN IBA INFO

## arrange, remove points 4 and 5, rename
unique(MAIN_IBA_info$Poly_ID)
MAIN_IBA_info <- MAIN_IBA_info %>% arrange(Poly_ID) %>% filter(!Poly_ID %in% c(4,5))
unique(MAIN_IBA_info$Poly_ID)
dim(MAIN_IBA_info)

## re-number Poly_ID from 1 again - NOTING FOR EACH UNIQUE POLY_ID
MAIN_IBA_info$Poly_ID2 <- cumsum(!duplicated(MAIN_IBA_info$Poly_ID)) 
tail(MAIN_IBA_info,20)

## Plot to test and check these layers still overlap
t = 3
IBA_maj %>% filter(Poly_ID == t) %>% st_geometry() %>% plot()
all_poly_smooth %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")
MAIN_IBA_info %>% filter(Poly_ID == t)
plot(st_geometry(IBA_maj))
all_poly_smooth %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")


## NOW for COLONY IBA INFO

## arrange, remove points 4 and 5, rename
unique(IBA_colony_info$Poly_ID)
IBA_colony_info <- IBA_colony_info %>% arrange(Poly_ID) %>% filter(!Poly_ID %in% c(4,5))
unique(IBA_colony_info$Poly_ID)
dim(IBA_colony_info)

## re-number Poly_ID from 1 again - NOTING FOR EACH UNIQUE POLY_ID
IBA_colony_info$Poly_ID2 <- cumsum(!duplicated(IBA_colony_info$Poly_ID)) 
tail(IBA_colony_info,20)


## Plot to test and check these layers still overlap
t = 3
IBA_maj %>% filter(Poly_ID == t) %>% st_geometry() %>% plot()
all_poly_smooth %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")
MAIN_IBA_info %>% filter(Poly_ID == t)
IBA_colony_info %>% filter(Poly_ID == t)
plot(st_geometry(IBA_maj))
all_poly_smooth %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")


## Now remove checking column
MAIN_IBA_info$Poly_ID <- MAIN_IBA_info$Poly_ID2
head(MAIN_IBA_info,9)
tail(MAIN_IBA_info,4)
MAIN_IBA_info <- MAIN_IBA_info %>% dplyr::select(-Poly_ID2)


IBA_colony_info$Poly_ID <- IBA_colony_info$Poly_ID2
head(IBA_colony_info,9)
tail(IBA_colony_info,4)
IBA_colony_info <- IBA_colony_info %>% dplyr::select(-Poly_ID2)


## And finally, save over files
save(MAIN_IBA_info, 
          file=paste("./Data/Rasters/IBAs_CellsJune2020/","64sites_mIBA_Antarctica_Info_v2.Rdata", sep=""))

write.csv(IBA_colony_info, paste("./Data/Antarctica_Penguin_mIBAs/ColoniesInMainIBAs_June2020_v2.csv"), row.names = F)



"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"Now Continue to deal with overlapping polygons"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"
"------------------------------------------------------------------------------------------------------"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dealing with slight overlaps of polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Use intersection to determine where polygons are overlapping
## note the new column produced: n.overlaps
s_inter <- st_intersection(all_poly_smooth)
s_inter$n.overlaps
s_inter$origins

dp <- data.frame(s_inter)
head(dp)
tail(dp)
dp
## Find the polygons that are overlapping - get the lists that have more than two objects in them
str(dp$origins)
dp$origins[[12]] 

## List of overlapping polygons - SHOULD NOW ONLY HAVE INSTANCE OF TWO OVERLAPS
unique(lengths(dp$origins)) # SHOULD NOW ONLY HAVE INSTANCE OF TWO OVERLAPS

## Original 49 polygons onl ever had two overlapping polygons
#dp_over <- subset(dp$origins, lengths(dp$origins) == 2) # note lengths not length! differenet functions

## Now with 64 polygons, we are back to the case where some polygons overlap in pairs
dp_over <- subset(dp$origins, lengths(dp$origins) == 2) # note lengths not length! differenet functions

str(dp_over)
length(dp_over)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Work with overlapping polygon pairs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(dp_over)
unlist(dp_over)
dp_unique <- unique(unlist(dp_over))
length(dp_unique)
dp_unique

## First select all those polygons which do not overlap
s_non <- all_poly_smooth %>% filter(!(Poly_ID %in% dp_unique))

## Now select polygons where there is overlap
s_over <- all_poly_smooth %>% filter(Poly_ID %in% dp_unique)

plot(st_geometry(s_non))
plot(st_geometry(s_over))
s_non$Poly_ID
s_over$Poly_ID

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Start editing the polygon pairs where there is overlap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"use here for an example"

## Specify list element to get.
p = 1
dp_poly <- unlist(dp_over[p])

## get the first and second polygons that overlap in this polygon pair
p1 <- s_over %>% filter(Poly_ID == dp_poly[1])
p2 <- s_over %>% filter(Poly_ID == dp_poly[2])

## Plot the two polygons
plot(st_geometry(p1))
plot(st_geometry(p2),add=T)

## Test to confirm they overlap
s_over_test <- st_intersects(p1,p2, sparse = F)
s_over_test[1]

## Re-calculate area of each polygon
#p1$area_m2 <- drop_units(st_area(p1))
#p2$area_m2 <- drop_units(st_area(p2))

## compare/select which polygon is largest
#s_compare <- ifelse(p1$area_m2 > p2$area_m2, p1, p2)

## Convert to sp objects as oppose to sf (better for cropping polygons)
p1s <- as_Spatial(p1)
p2s <- as_Spatial(p2)

proj4string(p1s)
proj4string(p2s)

## clip out overlapping part of polygons
plot(p2s)
plot(p1s,add=T)
#test <- p2s - p1s
p1s_clipped <- raster::erase(p1s, p2s)
plot(p1s_clipped)

## test if new polygons are overlapping
#test <- st_as_sf(test)
#st_intersects(test,p1, sparse = F)
#t2 <- st_intersection(test,p1)

## Convert back to sf object
p1 <- st_as_sf(p1s_clipped)
p2 <- st_as_sf(p2s)

## Plot new pairs
plot(st_geometry(p1))
plot(st_geometry(p2), add=T, col = "blue")

## Remove the old polygons which we just mofified from the multipolygon object
s_over <- s_over %>% filter(!(Poly_ID %in% dp_poly))

## And add back the modified polygons
s_over <- rbind(s_over,p1,p2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Now create a loop to modified all pairs of overlapping polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"use here to loop over all. I.e. can start from 1"

for(p in 1:length(dp_over)){
  
  ## Specify list element to get.
  dp_poly <- unlist(dp_over[p])
  
  ## get the first and second polygons that overlap in this polygon pair
  p1 <- s_over %>% filter(Poly_ID == dp_poly[1])
  p2 <- s_over %>% filter(Poly_ID == dp_poly[2])
  
  ## Test to confirm they overlap
  s_over_test <- st_intersects(p1,p2, sparse = F)
  print(paste("Loop", p, "Overlap is", paste(s_over_test[1])))
  
  ## Convert to sp objects as oppose to sf (better for cropping polygons)
  p1s <- as_Spatial(p1)
  p2s <- as_Spatial(p2)
  
  ## clip out overlapping part of polygons
  p1s_clipped <- raster::erase(p1s, p2s)
  
  ## Convert back to sf object
  p1 <- st_as_sf(p1s_clipped)
  p2 <- st_as_sf(p2s)
  
  ## Remove the old polygons which we just mofified from the multipolygon object
  s_over <- s_over %>% filter(!(Poly_ID %in% dp_poly))
  
  ## And add back the modified polygons
  s_over <- rbind(s_over,p1,p2)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Merge all polygons together and save for viewiwing in ArcMap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s_all <- rbind(s_non, s_over)
dim(s_all)
s_all
sort(unique(s_all$Poly_ID))

plot(st_geometry(s_all))
s_all

## FROM OLD VERSION of SCRIPT where I looked at intersection: Save shapefile for comparison
#st_write(s_non, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_Aggregated_Major_SmoothClipped.shp", sep=""), delete_layer = T)

## Calculate area of each polygon again to account for raster -> polygon conversion
s_area <- st_area(s_all)
class(s_area)
s_all <- s_all %>% mutate(area_m2=s_area)
s_all$area_m2 <- drop_units(s_all$area_m2)
head(s_all)
str(s_all)

## Check all the polygons are there
s_all$Poly_ID
sort(s_all$Poly_ID)

## Check projection
st_crs(s_all)
st_crs(all_poly_smooth)

## Check difference between file types
str(s_all)
str(all_poly_smooth)
s_all
all_poly_smooth

## arrange by Poly_ID
s_all <- s_all %>% arrange(Poly_ID)


## Save

## OLD (49miba data) naming
#save(s_all, file = paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothClippedOverlaps_v2.Rdata", sep=""))
## NEW saving name (64 mIBA data)
save(s_all, file = paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothedOverlaps_v1.Rdata", sep=""))
load(paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothedOverlaps_v1.Rdata", sep=""))
plot(s_all)
head(s_all)

## Save as spatial file
st_write(s_all, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothedOverlaps_v1.shp", sep=""), delete_layer = T)
