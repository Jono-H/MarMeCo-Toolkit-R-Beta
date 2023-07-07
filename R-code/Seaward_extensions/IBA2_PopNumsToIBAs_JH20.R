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
### Read in necessary data
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


## Read in rasters
List_min <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="min_B_dist16.tif$",full.names=T)
List_max <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="max_B_dist16.tif$",full.names=T)
List_median <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="median_B_dist16.tif$",full.names=T)
List_recent <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="recent_B_dist16.tif$",full.names=T)

## create a raster stack from the input raster files 
RasterStack <- raster::stack(c(List_min, List_max, List_median, List_recent))
RasterStack
names(RasterStack)
names(RasterStack[[5]])
plot(RasterStack[[5]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Subset single polygon and get relevant raster info relating to polygon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"This section and next are for testing purposes"
p = 25

stack_n = 6

poly1 <- IBA_maj %>% filter(poly_ID == p)
plot(st_geometry(poly1))
plot(RasterStack[[stack_n]], add = T)
plot(st_geometry(poly1), add=T)

## Check projections of each
st_crs(poly1)
proj4string(as_Spatial(poly1))
proj4string(RasterStack)
plot(RasterStack[[stack_n]])
plot(st_geometry(poly1),add=T)

## Mask raster stack
RasterMask <- mask(x = RasterStack, mask = poly1)
plot(RasterMask[[stack_n]])
plot(st_geometry(poly1))
plot(RasterMask[[stack_n]],add  =T)
names(RasterMask)

## Then Crop it to remove blank space around it
RasterCropped <- crop(x = RasterMask, y = extent(poly1))
## See difference in plots
plot(RasterMask[[stack_n]])
plot(RasterCropped[[stack_n]])
plot(st_geometry(poly1), add=T)
nlayers(RasterMask)
nlayers(RasterCropped)
minValue(RasterCropped[[stack_n]])
maxValue(RasterCropped[[stack_n]])
head(sort(values(RasterCropped[[stack_n]]), decreasing = F))

## Checking / figuring ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
t <- RasterCropped[[stack_n]] 
## get values only
t_vals <- t[!is.na(values(t))]
## see how many values are below 1% threshold - change value as needed depending on species
t_vals_below <- length(t_vals[t_vals < 34100])

t[t < 34100 & t > 0] <- 1
plot(t)
plot(st_geometry(poly1), add=T)


"SEEMS like as of June 2020 - with new bearing method, when some IBA polygons are created
for the IBA layers, there is a single or two cells which is/are below the 1% threshold that is on
the inner border of the polygon so when you create a polygon, for some reason, this 
cell gets included. 

NB: This seems to happen for varying count type layers.

For example, IBA 13 relates to max count and others, whereas IBA14, only related to
median count. (given there were 66 input sites)

Hence, the input file IBA_Maj has more IBA polygons than the output below: MAIN_IBA_info.
Need to account for this!"


## Save as ESRI files for testing and trying to identify why rasters are not Masking completely (was doing this before I introduced mask then crop options)
## See: https://rpubs.com/ricardo_ochoa/416711
#st_write(poly1, paste("./Data/Rasters/IBAs_CellsJune2020/PopNumbersInIBAs/","poly1_test.shp", sep=""), delete_layer = T)

#writeRaster(RasterMask[[5]], # number here relates to layer info from IBA_info output below
#              paste("./Data/Rasters/IBAs_CellsJune2020/PopNumbersInIBAs/","poly1_Mask_Max.tif", sep=""), overwrite = T)

#writeRaster(RasterCropped[[5]], 
#            paste("./Data/Rasters/IBAs_CellsJune2020/PopNumbersInIBAs/","poly1_Cropped_Max.tif", sep=""), overwrite = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Get key population info for polygon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## BLank data frame of data required
IBA_info <- data.frame(Poly_ID=as.numeric(),
                       Species=as.character(),
                       Count_type=as.character(),
                       Count_min=as.numeric(),
                       Count_max=as.numeric(),
                       r_vals_below=as.numeric())
IBA_info


## Loop over each raster layer per species (e.g species 1, and 4 associated count types. 16 layers in my case)
for (i in 1:nlayers(RasterCropped)){
  # raster layer
  r <- RasterCropped[[i]]
  # get name info
  n <- unlist(str_split(names(r),pattern="_"))
  
  r
  n
  
  " ---- This section is an UGLY hack to account for single cell problem noted above"
  
  ## see how many raster values which were cropped by polygon are below 1% threshold
  r_vals_below <- if (n[1] =="ADP") {
    ## get values only
    r_vals <- r[!is.na(r)]
    ## see how many values are below 1% threshold
    length(r_vals[r_vals < 3790000/100])
  } else if (n[1] =="CHP") {
    r_vals <- r[!is.na(r)]
    length(r_vals[r_vals < 3410000/100])
  } else if (n[1] =="EMP") {
    r_vals <- r[!is.na(r)]
    length(r_vals[r_vals < 256500/100])
  } else if (n[1] =="GEP") {
    r_vals <- r[!is.na(r)]
    length(r_vals[r_vals < 387000/100])
  } 
  r_vals_below
  
  ## The issue seems to relate to when there is a single or TWO cell(s) causing troubles
  ## this will check the layer if there is a single cell and will then convert
  ## that value to the minimum 1% threshold
  r2 <- if (r_vals_below <= 2 & n[1] =="ADP") {
    r[r < 37900 & r > 0] <- 37900
    r
    
  } else if (r_vals_below <= 2 & n[1] =="CHP") {
    r[r < 34100 & r > 0] <- 34100
    r
    
  } else if (r_vals_below <= 2 & n[1] =="EMP") {
    r[r < 2565 & r > 0] <- 2565
    r
    
  } else if (r_vals_below <= 2 & n[1] =="GEP") {
    r[r < 3870 & r > 0] <- 3870
    r
  } else if (r_vals_below != 1) {
    r
  } 
  
  r2
  
  ## Check things are working here
  values(r2)
  minValue(r2)
  maxValue(r2)
  head(sort(values(r2), decreasing = F))
  
  
  ## call r2 r for keeping with old script
  r <- r2
  
  " ------ Here continues with the old script"
  
  # build meta data needed
  IBA_temp <- data.frame(Poly_ID= p, # polygon ID to link with main IBA polygon layer
                         Species=n[1], # species code from naming variable
                         Count_type=n[4], # count type from naming variable
                         Count_min=round(ifelse(cellStats(r,min)==Inf, 0, cellStats(r,min)),0), # min count of raster cells within polygon
                         Count_max=round(ifelse(cellStats(r,max)==-Inf, 0, cellStats(r,max)),0), # max count of raster cells within polygon
                         r_vals_below = r_vals_below)   
  
  IBA_info <- rbind(IBA_info,IBA_temp)
  
  ## remove r and r2 at end of loop
  rm(r, r2)
  
}

IBA_info 
## Remove all the 0 counts (i.e. the raster stack layers which did not occur in this main IBA)
IBA_info %>% filter(Count_max != 0)
## Will also need to re-check which species actually triggered this polygon. THerefore,
## You can get key counts that relate to the polygon. I.e. the min and max counts that 
## triggered >1%. This will help account for muddly green text below:
"NB: in IBA_info output,
You may notice that some cells have values which would [not?] have met KBA criteria (>1% global pop),
However, these are likely from cells where the condition was only met part of the time. In
our case, this is when cells met values for <3 instances of the 4 different count types."

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Integrate loop and key info for all sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Skip to LOAD data if run these sections below already!"


## BLank data frame of data required
MAIN_IBA_info <- data.frame(Poly_ID=as.numeric(),
                            Species=as.character(),
                            Count_type=as.character(),
                            Count_min=as.numeric(),
                            Count_max=as.numeric(),
                            r_vals_below=as.numeric())
MAIN_IBA_info

## Check RasterStack you are extracting data from - ensure it is the Antarctica wide layer for each species count type
names(RasterStack)
unique(IBA_maj$poly_ID)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Run loop with section HASHED the section which says.
## The issue seems to relate to when there is a single or TWO cell(s) causing troubles
## this will check the layer if there is a single cell and will then convert
## that value to the minimum 1% threshold.
This will identify the missing polygons and not mess with some of the other smaller
polygons which might be  only 2 cells big anyway!
"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#p = 2
for(p in 1:max(IBA_maj$poly_ID)){
  
  ## Progress
  print(paste("Begin Outer Loop", p,sep=" "))
  
  ## SUbset polygon
  poly1 <- IBA_maj %>% filter(poly_ID == p)
  ## Mask raster stack
  RasterMask <- mask(x = RasterStack, mask = poly1)
  ## Then Crop it to remove blank space around it
  RasterCropped <- crop(x = RasterMask, y = extent(poly1))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Get key population info for polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## BLank data frame of data required
  site_info <- data.frame(Poly_ID=as.numeric(),
                          Species=as.character(),
                          Count_type=as.character(),
                          Count_min=as.numeric(),
                          Count_max=as.numeric(),
                          r_vals_below=as.numeric())
  
  ## Progress
  print(paste("Begin Inner Loop", p,sep=" "))
  ## Loop over each raster layer per species (e.g species 1, and 4 associated count types. 16 layers in my case)
  for (i in 1:nlayers(RasterCropped)){
    # raster layer
    r <- RasterCropped[[i]]
    # get name info
    n <- unlist(str_split(names(r),pattern="_"))
    
    r
    n
    
    " ---- This section is an UGLY hack to account for single cell problem noted above"
    
    ## see how many raster values which were cropped by polygon are below 1% threshold
    r_vals_below <- if (n[1] =="ADP") {
      ## get values only
      r_vals <- r[!is.na(r)]
      ## see how many values are below 1% threshold
      length(r_vals[r_vals < 3790000/100])
    } else if (n[1] =="CHP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 3410000/100])
    } else if (n[1] =="EMP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 256500/100])
    } else if (n[1] =="GEP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 387000/100])
    } 
    r_vals_below
    
    ## The issue seems to relate to when there is a single or TWO cell(s) causing troubles
    ## this will check the layer if there is a single cell and will then convert
    ## that value to the minimum 1% threshold
    #r2 <- if (r_vals_below <= 2 & n[1] =="ADP") {
    #  r[r < 37900 & r > 0] <- 37900
    #  r
    #  
    #} else if (r_vals_below <= 2 & n[1] =="CHP") {
    #  r[r < 34100 & r > 0] <- 34100
    #  r
    #  
    #} else if (r_vals_below <= 2 & n[1] =="EMP") {
    #  r[r < 2565 & r > 0] <- 2565
    #  r
    #  
    #} else if (r_vals_below <= 2 & n[1] =="GEP") {
    #  r[r < 3870 & r > 0] <- 3870
    #  r
    #} else if (r_vals_below != 1) {
    #  r
    #} 
    
    #r2
    
    ## Check things are working here
    #values(r2)
    #minValue(r2)
    #maxValue(r2)
    #head(sort(values(r2), decreasing = F))
    
    
    ## call r2 r for keeping with old script
    #r <- r2
    
    " ------ Here continues with the old script"
    
    # build meta data needed
    IBA_temp <- data.frame(Poly_ID= p, # polygon ID to link with main IBA polygon layer
                           Species=n[1], # species code from naming variable
                           Count_type=n[4], # count type from naming variable
                           Count_min=round(ifelse(cellStats(r,min)==Inf, 0, cellStats(r,min)),0), # min count of raster cells within polygon
                           Count_max=round(ifelse(cellStats(r,max)==-Inf, 0, cellStats(r,max)),0), # max count of raster cells within polygon
                           r_vals_below = r_vals_below)   
    
    site_info <- rbind(site_info,IBA_temp)
    
    ## remove r and r2 at end of loop
    rm(r, r2)
    
  }
  #head(site_info)
  #dim(site_info)
  
  ## Remove all the 0 counts (i.e. the raster stack layers which did not occur in this main IBA)
  site_info <- site_info %>% filter(Count_max != 0)
  
  ## Need to re-check which species actually triggered this IBA polygon
  ## Min counts
  site_info$Count_min_IBA <- ifelse(site_info$Species =="ADP" & site_info$Count_min >= 3790000/100, 1,
                                    ifelse(site_info$Species =="CHP" & site_info$Count_min >= 3410000/100, 1,
                                           ifelse(site_info$Species =="EMP" & site_info$Count_min >= 256500/100, 1,
                                                  ifelse(site_info$Species =="GEP" & site_info$Count_min >= 387000/100, 1,0))))
  #site_info
  
  ## Max Counts
  site_info$Count_max_IBA <- ifelse(site_info$Species =="ADP" & site_info$Count_max >= 3790000/100, 1,
                                    ifelse(site_info$Species =="CHP" & site_info$Count_max >= 3410000/100, 1,
                                           ifelse(site_info$Species =="EMP" & site_info$Count_max >= 256500/100, 1,
                                                  ifelse(site_info$Species =="GEP" & site_info$Count_max >= 387000/100, 1,0))))
  #site_info
  
  ## Group species and assess for counts triggered >= 3 instances (3 of 4 count types)
  SpeciesToAssess <- site_info %>% group_by(Species) %>% summarise(Sum_min_IBA = sum(Count_min_IBA),
                                                                   Sum_max_IBA = sum(Count_max_IBA))
  SpeciesToAssess <- SpeciesToAssess %>% filter(Sum_min_IBA >= 3)
  SpeciesToAssess <- droplevels(SpeciesToAssess$Species)
  SpeciesToAssess
  
  ## Now select from poly info colony the relevant details you will need for subsetting
  ## the correct rasterstack files.
  #site_info
  site_info_final <- site_info %>% filter(Species %in% SpeciesToAssess) %>% filter(Count_min_IBA != 0)
  #site_info_final
  
  ##
  MAIN_IBA_info <- rbind(MAIN_IBA_info,site_info_final)
}

head(MAIN_IBA_info)
MAIN_IBA_info
dim(MAIN_IBA_info)


## Because of file name issues, median counts have been lavelled as B. Change this
MAIN_IBA_info$Count_type <- fct_recode(MAIN_IBA_info$Count_type, median = "B")
unique(MAIN_IBA_info$Count_type)
table(MAIN_IBA_info$Count_type)

## Check if any polygons are missing for some reason
d <- data.frame(df = c(unique(IBA_maj$poly_ID)))
missing_polys <- d %>% filter(!df %in% c(unique(MAIN_IBA_info$Poly_ID)))
missing_polys



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Run loop again only for the missing polygons - unhash the section which says.
## The issue seems to relate to when there is a single or TWO cell(s) causing troubles
## this will check the layer if there is a single cell and will then convert
## that value to the minimum 1% threshold
"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## BLank data frame of data required
MAIN_IBA_info_missing <- data.frame(Poly_ID=as.numeric(),
                                    Species=as.character(),
                                    Count_type=as.character(),
                                    Count_min=as.numeric(),
                                    Count_max=as.numeric(),
                                    r_vals_below=as.numeric())
MAIN_IBA_info_missing

## Check RasterStack you are extracting data from - ensure it is the Antarctica wide layer for each species count type
names(RasterStack)
unique(IBA_maj$poly_ID)

"NOTE: Only selecting missing_polys here"
#p = 2
for(p in missing_polys$df){
  
  ## Progress
  print(paste("Begin Outer Loop", p,sep=" "))
  
  ## SUbset polygon
  poly1 <- IBA_maj %>% filter(poly_ID == p)
  ## Mask raster stack
  RasterMask <- mask(x = RasterStack, mask = poly1)
  ## Then Crop it to remove blank space around it
  RasterCropped <- crop(x = RasterMask, y = extent(poly1))
  
  print(ifelse(ncell(RasterCropped) <=2, "CAUTION: <= 2 cells in raster - therefore fix may be broken!","Fix OK"))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Get key population info for polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## BLank data frame of data required
  site_info <- data.frame(Poly_ID=as.numeric(),
                          Species=as.character(),
                          Count_type=as.character(),
                          Count_min=as.numeric(),
                          Count_max=as.numeric(),
                          r_vals_below=as.numeric())
  
  ## Progress
  print(paste("Begin Inner Loop", p,sep=" "))
  ## Loop over each raster layer per species (e.g species 1, and 4 associated count types. 16 layers in my case)
  for (i in 1:nlayers(RasterCropped)){
    # raster layer
    r <- RasterCropped[[i]]
    # get name info
    n <- unlist(str_split(names(r),pattern="_"))
    
    r
    n
    
    " ---- This section is an UGLY hack to account for single cell problem noted above"
    
    ## see how many raster values which were cropped by polygon are below 1% threshold
    r_vals_below <- if (n[1] =="ADP") {
      ## get values only
      r_vals <- r[!is.na(r)]
      ## see how many values are below 1% threshold
      length(r_vals[r_vals < 3790000/100])
    } else if (n[1] =="CHP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 3410000/100])
    } else if (n[1] =="EMP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 256500/100])
    } else if (n[1] =="GEP") {
      r_vals <- r[!is.na(r)]
      length(r_vals[r_vals < 387000/100])
    } 
    r_vals_below
    
    ## The issue seems to relate to when there is a single or TWO cell(s) causing troubles
    ## this will check the layer if there is a single cell and will then convert
    ## that value to the minimum 1% threshold
    r2 <- if (r_vals_below <= 2 & n[1] =="ADP") {
      r[r < 37900 & r > 0] <- 37900
      r
      
    } else if (r_vals_below <= 2 & n[1] =="CHP") {
      r[r < 34100 & r > 0] <- 34100
      r
      
    } else if (r_vals_below <= 2 & n[1] =="EMP") {
      r[r < 2565 & r > 0] <- 2565
      r
      
    } else if (r_vals_below <= 2 & n[1] =="GEP") {
      r[r < 3870 & r > 0] <- 3870
      r
    } else if (r_vals_below != 1) {
      r
    } 
    
    #r2
    
    ## Check things are working here
    #values(r2)
    #minValue(r2)
    #maxValue(r2)
    #head(sort(values(r2), decreasing = F))
    
    
    ## call r2 r for keeping with old script
    #r <- r2
    
    " ------ Here continues with the old script"
    
    # build meta data needed
    IBA_temp <- data.frame(Poly_ID= p, # polygon ID to link with main IBA polygon layer
                           Species=n[1], # species code from naming variable
                           Count_type=n[4], # count type from naming variable
                           Count_min=round(ifelse(cellStats(r,min)==Inf, 0, cellStats(r,min)),0), # min count of raster cells within polygon
                           Count_max=round(ifelse(cellStats(r,max)==-Inf, 0, cellStats(r,max)),0), # max count of raster cells within polygon
                           r_vals_below = r_vals_below)   
    
    site_info <- rbind(site_info,IBA_temp)
    
    ## remove r and r2 at end of loop
    rm(r, r2)
    
  }
  #head(site_info)
  #dim(site_info)
  
  ## Remove all the 0 counts (i.e. the raster stack layers which did not occur in this main IBA)
  site_info <- site_info %>% filter(Count_max != 0)
  
  ## Need to re-check which species actually triggered this IBA polygon
  ## Min counts
  site_info$Count_min_IBA <- ifelse(site_info$Species =="ADP" & site_info$Count_min >= 3790000/100, 1,
                                    ifelse(site_info$Species =="CHP" & site_info$Count_min >= 3410000/100, 1,
                                           ifelse(site_info$Species =="EMP" & site_info$Count_min >= 256500/100, 1,
                                                  ifelse(site_info$Species =="GEP" & site_info$Count_min >= 387000/100, 1,0))))
  #site_info
  
  ## Max Counts
  site_info$Count_max_IBA <- ifelse(site_info$Species =="ADP" & site_info$Count_max >= 3790000/100, 1,
                                    ifelse(site_info$Species =="CHP" & site_info$Count_max >= 3410000/100, 1,
                                           ifelse(site_info$Species =="EMP" & site_info$Count_max >= 256500/100, 1,
                                                  ifelse(site_info$Species =="GEP" & site_info$Count_max >= 387000/100, 1,0))))
  #site_info
  
  ## Group species and assess for counts triggered >= 3 instances (3 of 4 count types)
  SpeciesToAssess <- site_info %>% group_by(Species) %>% summarise(Sum_min_IBA = sum(Count_min_IBA),
                                                                   Sum_max_IBA = sum(Count_max_IBA))
  SpeciesToAssess <- SpeciesToAssess %>% filter(Sum_min_IBA >= 3)
  SpeciesToAssess <- droplevels(SpeciesToAssess$Species)
  SpeciesToAssess
  
  ## Now select from poly info colony the relevant details you will need for subsetting
  ## the correct rasterstack files.
  #site_info
  site_info_final <- site_info %>% filter(Species %in% SpeciesToAssess) %>% filter(Count_min_IBA != 0)
  #site_info_final
  
  ##
  MAIN_IBA_info_missing <- rbind(MAIN_IBA_info_missing,site_info_final)
}


head(MAIN_IBA_info_missing)
MAIN_IBA_info_missing
dim(MAIN_IBA_info_missing)

## Because of file name issues, median counts have been lavelled as B. Change this
MAIN_IBA_info_missing$Count_type <- fct_recode(MAIN_IBA_info_missing$Count_type, median = "B")
unique(MAIN_IBA_info_missing$Count_type)
table(MAIN_IBA_info_missing$Count_type)

## Check if any polygons are missing for some reason
d <- data.frame(df = c(unique(MAIN_IBA_info_missing$Poly_ID)))
missing_polys <- d %>% filter(!df %in% c(unique(MAIN_IBA_info_missing$Poly_ID)))
missing_polys


## ~~~~~~~~ Merge files together ~~~~~~~~~~~~~~~
dim(MAIN_IBA_info)
dim(MAIN_IBA_info_missing)

MAIN_IBA_info_ALL <- rbind(MAIN_IBA_info, MAIN_IBA_info_missing)
head(MAIN_IBA_info_ALL)
tail(MAIN_IBA_info_ALL)

## order files appropriately and remove r_vals_below (the Nb checking column)
MAIN_IBA_info_ALL2 <- MAIN_IBA_info_ALL %>% arrange(Poly_ID, Species) %>% 
  dplyr::select(-r_vals_below)

head(MAIN_IBA_info_ALL2)
tail(MAIN_IBA_info_ALL2)

## ~~~~~~~~ change name back to original script
MAIN_IBA_info <- MAIN_IBA_info_ALL2
head(MAIN_IBA_info)
tail(MAIN_IBA_info)


## ~~~~~~~~ saving ~~~~~~~~~~~~~~~
#save(MAIN_IBA_info, 
#     file=paste("./Data/Rasters/IBAs_CellsJune2020/","66sites_mIBA_Antarctica_Info_v1.Rdata", sep=""))


"LOAD ------ if run above already"


load(paste("./Data/Rasters/IBAs_CellsJune2020/","66sites_mIBA_Antarctica_Info_v1.Rdata", sep=""))
head(MAIN_IBA_info,9)
tail(MAIN_IBA_info,4)
unique(MAIN_IBA_info$Poly_ID)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Now, would be good to create figure and associated table for each MAIN IBA polygon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"From MAIN_IBA_info output - could create associated table and polygon of MAIN IBAs"


"-------------------------------------------------------------------------------------"
"-------------------------------------------------------------------------------------"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## The above gave you population estimates for each MAIN marine IBA, BUT did not give you
## information regarding which colonies contributed to each marine IBA. This would be
## useful information to have.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"-------------------------------------------------------------------------------------"
"-------------------------------------------------------------------------------------"

## Load raster stack layers which relate to foraging radius area used by each colony
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Read in raster stack files
List_stacks <- dir(path="./Data/Rasters/IBAs_CellsJune2020",pattern="_stack.Rdata$",full.names=T)
List_stacks

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use stack_assess dataframe to call out the correct raster stack layers and merge to mega dataframe
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Rename to match previous naming convention
stack_assess <- MAIN_IBA_info


## unique row id for stack_assess
stack_assess$Row_ID <- 1:nrow(stack_assess)
head(stack_assess,2)
tail(stack_assess,2)
dim(stack_assess)
unique(stack_assess$Poly_ID)


## Remove previous R objects that will have same name in loop below
rm(temp_info, temp_info2, RasterMask, RasterCropped, stack_temp, poly1)

## BLank data frame of data required
rm(stack_iba_info)
stack_iba_info <- data.frame(Poly_ID=as.numeric(),
                             Raster_layer=as.numeric(),
                             Species=as.character(),
                             Count_type=as.character(),
                             Count_min=as.numeric(),
                             Count_max=as.numeric(),
                             Count_year=as.numeric(),
                             Colony=as.character(),
                             stack_assess_row_id=as.numeric())

rm(temp_info,temp_info2)

# [May] Need to run loop several times because of memory issues.
max(stack_assess$Row_ID)


#for(s in 1:60){
#for(s in 61:113){
#    for(s in 114:170){
#     for(s in 171:220){
        for(s in 221:max(stack_assess$Row_ID)){
  #for(s in 1:9){ # Test loop
  
  ## Loop progress
  print(paste("Outer Loop",s,"of",max(stack_assess$Row_ID)))
  
  ## Get key row relating to data frame
  stack_data <- subset(stack_assess, stack_assess$Row_ID == s)
  
  ## Get main IBA polygon that relates to this row of info
  poly1 <- IBA_maj %>% filter(poly_ID == stack_data$Poly_ID)
  #plot(st_geometry(poly1))
  
  ## get raster stack FILE NAME that relates to this row of info
  ## Because of memory issues, let's go layer by layer and use some pattern matching:
  stack_filename <- str_subset(List_stacks, paste(stack_data$Species[1])) %>% str_subset(paste(stack_data$Count_type[1]))
  
  ## load R data file of interest - based on your IBA info relating to main IBA polygon
  ## of interest
  stack_temp <- loadRData(stack_filename)
  
  ## Mask raster stack
  RasterMask <- mask(x = stack_temp, mask = poly1)
  
  ## Then Crop it to remove blank space around it
  RasterCropped <- crop(x = RasterMask, y = extent(poly1))
  
  ## See difference in plots
  #plot(RasterMask[[1]]) # NB: you would need to guess which stack layer fits exactly into poly1 - i.e. these plots are likely not useful
  #plot(RasterCropped[[1]])
  #nlayers(RasterMask)
  #nlayers(RasterCropped)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ### Get key population info for polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## BLank data frame of data required
  temp_info <- data.frame(Poly_ID=as.numeric(),
                          Raster_layer=as.numeric(),
                          Species=as.character(),
                          Count_type=as.character(),
                          Count_min=as.numeric(),
                          Count_max=as.numeric(),
                          Count_year=as.numeric(),
                          Colony=as.character(),
                          stack_assess_row_id=as.numeric())
  temp_info
  
  
  ## Loop over each raster layer per count type for species (e.g species 1, and XXX associated count types, for XXX associated colonies)
  print(paste("Inner Loop",s,"of",max(stack_assess$Row_ID)))
  for (i in 1:nlayers(RasterCropped)){
    # raster layer
    r <- RasterCropped[[i]]
    # build meta data needed
    IBA_temp <- data.frame(Poly_ID= stack_data$Poly_ID, # polygon ID to link with main IBA polygon layer
                           Raster_layer = i, # which layer from the raster stack
                           Species=paste(stack_data$Species[1]), # species code from naming variable
                           Count_type=paste(stack_data$Count_type[1]), # count type from naming variable
                           Count_min=round(ifelse(cellStats(r,min)==Inf, 0, cellStats(r,min)),0), # min count of raster cells within polygon
                           Count_max=round(ifelse(cellStats(r,max)==-Inf, 0, cellStats(r,max)),0), # max count of raster cells within polygon
                           Count_year= data.frame(stack_temp[[i]]@data@attributes)[,4][1], # becase stack_temp should match order of RasterCropped (which are both rasterstacks). get metadata from stack_temp that relates to cropped raster of interest. Count_year in this case
                           Colony=droplevels(data.frame(stack_temp[[i]]@data@attributes)[,2][1]), # Colony relating to layer
                           stack_assess_row_id = s)
    
    temp_info <- rbind(temp_info,IBA_temp)
  }
  
  #temp_info 
  
  ## Get main info relating to this polygon - use max because...
  temp_info2 <- temp_info %>% filter(Count_max != 0)
  #temp_info2
  
  ## bind back to overall dataframe
  stack_iba_info <- rbind(stack_iba_info, temp_info2)
  
  ## Remove file versions
  rm(temp_info, temp_info2, RasterMask, RasterCropped, stack_temp, poly1)
  
  ## Free up ram via garbage dump (I don't really know what this does :), see: https://stackoverflow.com/questions/8813753/what-is-the-difference-between-gc-and-rm
  ## NB: It may slow things down considerably - consider trade of depending on data volume
  gc()
  
}

head(stack_iba_info)
tail(stack_iba_info)

"Save and load as required"

#setwd("F:/") ## save to extrenal hard drive if memory issues occur.

#save(stack_iba_info, file = "./Data/Rasters/ColoniesInIBAs_June2020_stacks_1to60.Rdata")
#save(stack_iba_info, file = "./ColoniesInIBAs_June2020_stacks_1to60.Rdata")
#write.csv(stack_iba_info, "./Data/Rasters/ColoniesInIBAs_June2020_stacks_1to60.csv", row.names = F)
#write.csv(stack_iba_info, "./ColoniesInIBAs_June2020_stacks_1to60.csv", row.names = F)
load("./Data/Rasters/ColoniesInIBAs_June2020_stacks_1to60.Rdata")
part1 <- stack_iba_info

##
#save(stack_iba_info, file = "./Data/Rasters/ColoniesInIBAs_June2020_stacks_61to113.Rdata")
#save(stack_iba_info, file = "./ColoniesInIBAs_June2020_stacks_61to113.Rdata")
#write.csv(stack_iba_info, "./Data/Rasters/ColoniesInIBAs_June2020_stacks_61to113.csv", row.names = F)
#write.csv(stack_iba_info, "./ColoniesInIBAs_June2020_stacks_61to113.csv", row.names = F)
load("./Data/Rasters/ColoniesInIBAs_June2020_stacks_61to113.Rdata")
part2 <- stack_iba_info

##
#save(stack_iba_info, file = "./Data/Rasters/ColoniesInIBAs_June2020_stacks_114to170.Rdata")
#save(stack_iba_info, file = "./ColoniesInIBAs_June2020_stacks_114to170.Rdata")
#write.csv(stack_iba_info, "./Data/Rasters/ColoniesInIBAs_June2020_stacks_114to170.csv", row.names = F)
#write.csv(stack_iba_info, "./ColoniesInIBAs_June2020_stacks_114to170.csv", row.names = F)
load("./Data/Rasters/ColoniesInIBAs_June2020_stacks_114to170.Rdata")
part3 <- stack_iba_info

##
#save(stack_iba_info, file = "./Data/Rasters/ColoniesInIBAs_June2020_stacks_171to220.Rdata")
#save(stack_iba_info, file = "./ColoniesInIBAs_June2020_stacks_171to220.Rdata")
#write.csv(stack_iba_info, "./Data/Rasters/ColoniesInIBAs_June2020_stacks_171to220.csv", row.names = F)
#write.csv(stack_iba_info, "./ColoniesInIBAs_June2020_stacks_171to220.csv", row.names = F)
load("./Data/Rasters/ColoniesInIBAs_June2020_stacks_171to220.Rdata")
part4 <- stack_iba_info

##
#save(stack_iba_info, file = "./Data/Rasters/ColoniesInIBAs_June2020_stacks_221to267.Rdata")
#save(stack_iba_info, file = "./ColoniesInIBAs_June2020_stacks_221to267.Rdata")
#write.csv(stack_iba_info, "./Data/Rasters/ColoniesInIBAs_June2020_stacks_221to267.csv", row.names = F)
#write.csv(stack_iba_info, "./ColoniesInIBAs_June2020_stacks_221to267.csv", row.names = F)
load("./Data/Rasters/ColoniesInIBAs_June2020_stacks_221to267.Rdata")
part5 <- stack_iba_info


###

MAIN_IBA_SITE_Info <- rbind(part1,part2,part3,part4,part5)
head(MAIN_IBA_SITE_Info)
tail(MAIN_IBA_SITE_Info)
unique(MAIN_IBA_SITE_Info$Poly_ID)
"Why are there not 66 unique IBAs / Poly IDs? - ISSUE should be solved with awful work around above!"
## Save final collated information relating to individual colony layers that make
## up each of the main IBAs. Might want to consider plotting through some of these
## to fully validate that everything is working
write.csv(MAIN_IBA_SITE_Info, paste("./Data/Antarctica_Penguin_mIBAs/ColoniesInMainIBAs_June2020.csv"), row.names = F)


getwd()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Smoothing out polygons to make them more visually appealing"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Or to smooth out jagged polygons... but what about polygons directly adjacent to each other?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


"NB AS OF JUNE 2020 UPDATE - the below steps are now performed in scrip: IBA2_SmoothPolygons_JH..."


## SUbset polygon
poly_temp2 <- IBA_maj %>% filter(poly_ID == t)

plot(rbind(poly_temp,poly_temp2))


## Access number of vertices for polygon
nvert <- length(unlist(poly_temp$geometry[[1]][1]))/2
nvert


## Simple feature smoothing
plot(st_geometry(poly_temp))
sf_smooth <- st_buffer(poly_temp, dist = 10000)
plot(st_geometry(sf_smooth), add=T, border= "red")
plot(st_geometry(sf_smooth))

## Kernel smooth
plot(st_geometry(poly_temp))
poly_smooth <- smooth(poly_temp, method = "ksmooth", smoothness = 2) # 1 is default
plot(st_geometry(poly_smooth), border = "#E41A1C", lwd = 2, add = TRUE)

## SPline smooth
plot(st_geometry(poly_temp))
poly_smooth <- smooth(poly_temp, method = "spline", vertex_factor = 5) # 5 is default
plot(st_geometry(poly_smooth), border = "#E41A1C", lwd = 2, add = TRUE)

## Chaikin smooth
plot(st_geometry(poly_temp))
poly_smooth <- smooth(poly_temp, method = "chaikin", refinements = 3) # 3 is default
plot(st_geometry(poly_smooth), border = "#E41A1C", lwd = 2, add = TRUE)

length(poly_temp$geometry)
class(poly_temp$geometry)
str(poly_temp$geometry)

poly_temp$geometry[[1]][1]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loop to smooth over polygons
"First test over a variety of sizes of the different IBAs. Determining number of vertices"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Poly ID
t = 3
## SUbset polygon
poly_temp <- IBA_maj %>% filter(poly_ID == t)
poly_temp <- IBA_maj %>% filter(area_m2 == 4.0000e+08)

## Access number of vertices for polygon
nvert <- length(unlist(poly_temp$geometry[[1]][1]))/2
nvert

## Kernel smooth
plot(st_geometry(poly_temp))
poly_smooth <- smooth(poly_temp, method = "ksmooth", smoothness = 2) # 1 is default
plot(st_geometry(poly_smooth), border = "#E41A1C", lwd = 2, add = TRUE)

plot(rbind(poly_temp,poly_temp2))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loop to smooth over polygons
"Now, consider doing the loop"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Poly ID
t = 1
## SUbset polygon
poly1 <- IBA_maj %>% filter(poly_ID == t)

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


for(t in 2:max(IBA_maj$poly_ID)){
  ## SUbset polygon
  poly_temp <- IBA_maj %>% filter(poly_ID == t)
  
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
st_write(all_poly_smooth, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_Aggregated_Major_Smooth_v2.shp", sep=""), delete_layer = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"UPDATED SCRIPT (where I erase parts of overlapping polygons): START FROM HERE"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_poly_smooth <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_Aggregated_Major_Smooth_v2.shp")
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
s_non <- all_poly_smooth %>% filter(!(poly_ID %in% dp_unique))

## Now select polygons where there is overlap
s_over <- all_poly_smooth %>% filter(poly_ID %in% dp_unique)

plot(st_geometry(s_non))
plot(st_geometry(s_over))

"Owing to updated 66 IBAs in June 2020"
## also extract polygon 25 which completely engulfs some others


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Start editing the polygon pairs where there is overlap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"use here for an example"

## Specify list element to get.
p = 10
dp_poly <- unlist(dp_over[p])

## get the first and second polygons that overlap in this polygon pair
p1 <- s_over %>% filter(poly_ID == dp_poly[1])
p2 <- s_over %>% filter(poly_ID == dp_poly[2])

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
s_over <- s_over %>% filter(!(poly_ID %in% dp_poly))

## And add back the modified polygons
s_over <- rbind(s_over,p1,p2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Now create a loop to modified all pairs of overlapping polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"use here to loop over all. I.e. can start from 1"

for(p in 12:length(dp_over)){
  
  ## Specify list element to get.
  dp_poly <- unlist(dp_over[p])
  
  ## get the first and second polygons that overlap in this polygon pair
  p1 <- s_over %>% filter(poly_ID == dp_poly[1])
  p2 <- s_over %>% filter(poly_ID == dp_poly[2])
  
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
  s_over <- s_over %>% filter(!(poly_ID %in% dp_poly))
  
  ## And add back the modified polygons
  s_over <- rbind(s_over,p1,p2)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Merge all polygons together and save for viewiwing in ArcMap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
s_all <- rbind(s_non, s_over)
dim(s_all)

plot(st_geometry(s_all))

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
s_all$poly_ID
sort(s_all$poly_ID)

## Check projection
st_crs(s_all)
st_crs(all_poly_smooth)

## Check difference between file types
str(s_all)
str(all_poly_smooth)
s_all
all_poly_smooth

## Save
#save(s_all, file = paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothClippedOverlaps_v2.Rdata", sep=""))
load(paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothClippedOverlaps_v2.Rdata", sep=""))
plot(s_all)
head(s_all)

#"Hmm, R Keeps aborting here when I try to save this file... FML..."
#st_write(s_all, paste("./Data/Rasters/IBAs_CellsJune2020/","ALL_SmoothClippedOverlaps_v2.shp", sep=""), delete_layer = T)

## Try converting to spatial points data frame
s_all_spdf <- as_Spatial(s_all)
plot(s_all_spdf)
s_all_spdf
head(s_all_spdf)
writeOGR(obj = s_all_spdf, dsn = "./Data/Rasters/IBAs_CellsJune2020", layer = "ALL_mIBA_Antarctica_Smooth", driver="ESRI Shapefile", overwrite_layer = T)
