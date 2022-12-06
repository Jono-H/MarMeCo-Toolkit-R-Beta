## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## based on example using the boobies tracking dataset. See previous scripts.

## This scripts takes the outputs of track2KBA R package which have already been 
## assessed against IBA criteria.

## INPUT data: data from the findSite() function in track2KBA, with metadata included,
## which has also been assessed against IBA criteria, and filtered to only include
## data that meets IBA criteria

## OUTPUT data: spatial data, simple features object, representing polygons 
## which have had boundaries "cleaned up", and which IBA data summarised for
## each unique polygon

## Jono Handley, jonathan.m.handley@gmail.com

## R version 4.1.2 (2021-11-01) -- "Bird Hippie"

## June 2022

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries --------------------------------------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(readxl)
library(sf)
library(xlsx)
library(sp)
library(gridExtra)
library(viridis)
library(rgeos)
library(geosphere)
library(smoothr)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load input data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_output_for_KBA_boundary_clean.Rdata")

## explore
finalSite

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## simple plot ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## simple plot
ggplot() +
  geom_sf(data = st_union(finalSite), colour = "red", fill = NA, size = 0.5)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Summary ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## See how many unique rows of data there are.
## These relate to the different areas where individuals with tracking data were overlapping.
## The more rows, relates to more areas with more individuals overlapping.
## See column 'N_IND' for indication of number of individuals overlapping in the 
## original tracking data used for the analysis.
dim(finalSite)
nrow(finalSite)
summary(finalSite)

## 
head(data.frame(finalSite))

## See how many unique polygons there are for the areas identified as KBAs
finalSite %>% st_union(.) %>% 
  st_cast(., "POLYGON") %>% 
  summary()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot these areas 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## simplify and create sf object
finalSite.polygons <- finalSite %>% st_union(.) %>% 
  st_cast(., "POLYGON") %>% 
  st_as_sf(.)

## assign a clump ID to each unique polygon
finalSite.polygons$clumps <- 1:nrow(finalSite.polygons)


## Produce the plot
p <- ggplot() +
  geom_sf(data = finalSite.polygons, aes(fill = as.factor(clumps))) +
  #scale_fill_viridis_c(trans = "sqrt", alpha = 1) +
  scale_color_viridis(discrete = TRUE) +
  guides(fill=guide_legend(title='Polygon')) +
  ## add the polygon labels
  geom_sf_text(data = finalSite.polygons, aes(label = clumps), colour = "black", size = 5) +
  ## remove x y labels
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  # Title
  ggtitle(paste("Unique polygons meeting KBA criteria",sep=""))+
  theme(plot.title = element_text(hjust = 0.5))

p


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Drop small pieces of polygons that are annoying
## And also fill the holes of polygons that have them
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Convert to projected projection
mid_point<-data.frame(centroid(gConvexHull(as_Spatial(finalSite.polygons))))
Projection <- sp::CRS(paste("+proj=laea +lon_0=", mid_point[,1], " +lat_0=", mid_point[,2], " +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep=""))

## review
finalSite.polygons
proj4string(as_Spatial(finalSite.polygons))

## assign projection - needs to be projected for analysis
PolyGroup_prj <- spTransform(as_Spatial(finalSite.polygons), CRS=Projection)

## Remove "small pieces" of polygons that are annoying.
## NOTE: gArea works in meters for projected projections even if units in km.!
th=5 ## Percent of size
Percent <- (gArea(PolyGroup_prj)/100)*th
## Drop small pieces of polygon
Poly_NoCrumbs_prj <- drop_crumbs(PolyGroup_prj, threshold=Percent)
## Fill holes
Poly_filled <- fill_holes(Poly_NoCrumbs_prj, threshold=gArea(PolyGroup_prj))

## Plot to check
plot(PolyGroup_prj,col="green") # original
plot(Poly_filled, add=T, col="red") # new 

## NOTE: You still retain metadata
Poly_filled



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Need to determine which polygons form unique clusters
## Inspiration from: https://gis.stackexchange.com/questions/254519/group-and-union-polygons-that-share-a-border-in-r
## Look how far away each polygon unit is away from each other
warning("OPTION 1: Uses polygons with holes filled and small pieces dropped. See option 2 below for further consideration")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Specify objects
sfpolys <- st_as_sf(Poly_filled)
## Set units in m if you want to use a particular threshold
#thresh <- set_units(200000, m)

## Create distance matrix between polygons
dmat = st_distance(sfpolys)
dmat
max(dmat)
## Work out 5% distance threshold: units in meters
dmat_5 <- (max(dmat)/100*5)
## Perform cluster analysis based on distances: either specifed thresh of dist_5
#hc = hclust(as.dist(dmat>thresh), method="single")
if(nrow(sfpolys)>=2){
  hc = hclust(as.dist(dmat>dmat_5), method="single")
  hc
  
  ## Specify which polygons belong to which group
  sfpolys$groups = cutree(hc, h=0.5)
}else{
  sfpolys$groups=1
}

## Number of new groups
max(sfpolys$groups)
## Check
sfpolys

## Subset out the polygon group of interest
max(sfpolys$groups)


g=1
grp <- sfpolys[sfpolys$groups==g,]

## Convert back to sp object (st_convex_hull (sf) is doing something weird - or I'm being silly) 
#grp_sp <- sf:::as_Spatial(grp$geometry)
grp_sp <- sf:::as_Spatial(grp)

## Create new convex hull around the reduced groups of polygons
grp_hull <- gConvexHull(grp_sp)
plot(grp_hull,col='blue')
plot(st_geometry(grp),add=T,col='green') ## Compare :)

## Append to polygon and create SPDF
grp_hull_df <- SpatialPolygonsDataFrame(grp_hull, 
                                        data = data.frame(MetaData = paste("Add metadata for polygon ",g,sep=""),
                                                          UniquePolygon = g))

grp_hull_df


## Overall SPDF for binding onto - rename for later use
grp_polys <- grp_hull_df

## loop over remaining pieces of polygons
if(nrow(sfpolys)>=2){
for(g in 2:max(sfpolys$groups)){
  
  grp <- sfpolys[sfpolys$groups==g,]
  
  ## Convert back to sp object (st_convex_hull (sf) is doing something weird - or I'm being silly) 
  #grp_sp <- sf:::as_Spatial(grp$geometry)
  grp_sp <- sf:::as_Spatial(grp)
  
  ## Create new convex hull around the reduced groups of polygons
  grp_hull <- gConvexHull(grp_sp)
  #plot(grp_hull,col='blue')
  #plot(st_geometry(grp),add=T,col='green') ## Compare :)

  ## Append to polygon and create SPDF
  grp_hull_df <- SpatialPolygonsDataFrame(grp_hull, 
                                          data = data.frame(MetaData = paste("Add metadata for polygon ",g,sep=""),
                                                            UniquePolygon = g))  
  grp_polys <- rbind(grp_polys,grp_hull_df)
  }
}else{
  grp_polys
}
  
plot(grp_polys)
proj4string(grp_polys)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Need to determine which polygons form unique clusters
## Inspiration from: https://gis.stackexchange.com/questions/254519/group-and-union-polygons-that-share-a-border-in-r
## Look how far away each polygon unit is away from each other
warning("OPTION 2: Outlined below, uses the data where holes have not been filled, 
        and small polygons have not been removed")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Specify objects
sfpolys <- finalSite.polygons
## Set units in m if you want to use a particular threshold
#thresh <- set_units(200000, m)

## Create distance matrix between polygons
dmat = st_distance(sfpolys)
dmat
max(dmat)
## Work out 5% distance threshold: units in meters
dmat_5 <- (max(dmat)/100*5)
## Perform cluster analysis based on distances: either specifed thresh of dist_5
#hc = hclust(as.dist(dmat>thresh), method="single")
if(nrow(sfpolys)>=2){
  hc = hclust(as.dist(dmat>dmat_5), method="single")
  hc
  
  ## Specify which polygons belong to which group
  sfpolys$groups = cutree(hc, h=0.5)
}else{
  sfpolys$groups=1
}

## Number of new groups
max(sfpolys$groups)
## Check
sfpolys

## Subset out the polygon group of interest
max(sfpolys$groups)


g=1
grp <- sfpolys[sfpolys$groups==g,]

## Convert back to sp object (st_convex_hull (sf) is doing something weird - or I'm being silly) 
#grp_sp <- sf:::as_Spatial(grp$geometry)
grp_sp <- sf:::as_Spatial(grp)

## Create new convex hull around the reduced groups of polygons
grp_hull <- gConvexHull(grp_sp)
plot(grp_hull,col='blue')
plot(st_geometry(grp),add=T,col='green') ## Compare :)

## Append to polygon and create SPDF
grp_hull_df <- SpatialPolygonsDataFrame(grp_hull, 
                                        data = data.frame(MetaData = paste("Add metadata for polygon ",g,sep=""),
                                                          UniquePolygon = g))
grp_hull_df


## Overall SPDF for binding onto - rename for later use
grp_polys <- grp_hull_df

## loop over remaining pieces of polygons
if(nrow(sfpolys)>=2){
  for(g in 2:max(sfpolys$groups)){
    
    grp <- sfpolys[sfpolys$groups==g,]
    
    ## Convert back to sp object (st_convex_hull (sf) is doing something weird - or I'm being silly) 
    #grp_sp <- sf:::as_Spatial(grp$geometry)
    grp_sp <- sf:::as_Spatial(grp)
    
    ## Create new convex hull around the reduced groups of polygons
    grp_hull <- gConvexHull(grp_sp)
    plot(grp_hull,col='blue')
    plot(st_geometry(grp),add=T,col='green') ## Compare :)
    
    ## Append to polygon and create SPDF
    grp_hull_df <- SpatialPolygonsDataFrame(grp_hull, 
                                            data = data.frame(MetaData = paste("Add metadata for polygon ",g,sep=""),
                                                              UniquePolygon = g))
    
    grp_polys <- rbind(grp_polys,grp_hull_df)
  }
}else{
  grp_polys
}

plot(grp_polys)
proj4string(grp_polys)
st_centroid(grp_polys)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NEXT STEPS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## convert the objects back to sf
SmoothedPolygons <- st_as_sf(grp_polys)

## ensure the projection matches that of the data with criteria assessed
SmoothedPolygons <- st_transform(SmoothedPolygons, crs = st_crs(finalSite))

## calculate centroids
plot(st_geometry(SmoothedPolygons))
d <- st_centroid(SmoothedPolygons)
plot(st_geometry(d), add = T, col = 'red', pch = 19)

## get lat long data of centroids of polygons
SmoothedPolygons <- SmoothedPolygons %>% 
  mutate(Lat_Centroid_Polygon = st_coordinates(d)[,2],
         Lon_Centroid_Polygon = st_coordinates(d)[,1])

## plot again for review
plot(st_geometry(SmoothedPolygons))
plot(st_geometry(finalSite.polygons),col = "green", add = T)
plot(st_geometry(finalSite[1,]), col = "red", add = T)
plot(st_geometry(d), add = T, col = 'blue', pch = 19)

## Merge the Smoothed Polygons with finalSite data and assess where there is overlap
sf::sf_use_s2(FALSE) # run this if issues with st_intersection: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
FinalPolygons <- st_intersection(finalSite, SmoothedPolygons)

## Summarise the data
colnames(FinalPolygons)

"SUMMARISE - GET DATA FOR EACH SITES"
FinalPolygonData <- FinalPolygons %>% 
  group_by(UniquePolygon) %>% 
  summarise(UniquePolygon = UniquePolygon[1],
            Lat_Centroid_Polygon  = Lat_Centroid_Polygon[1],
            Lon_Centroid_Polygon = Lon_Centroid_Polygon[1],
            Best_MatureIndividuals_Min = round(min(Best_MatureIndividuals),0),
            Best_MatureIndividuals_Max = round(max(Best_MatureIndividuals),0),
            SiteName_Origin = SiteName[1],
            RegionName_Origin = RegionName[1],
            SpeciesComNam_RedList = Common.name[1],
            Scientific.name = Scientific.name[1],
            RL.Category = RL.Category[1],
            RL.Criteria = RL.Criteria[1],
            StartSurvey = StartSurvey[1],
            EndSurvey = EndSurvey[1],
            Season = Season[1],
            SeasonSpecific = SeasonSpecific[1],
            Seabird = Seabird[1],
            Waterbird = Waterbird[1],
            Landbird = Landbird[1],
            Migratory.status = Migratory.status[1],
            global.min.mat.ind = global.min.mat.ind[1],
            global.best.mat.ind = global.best.mat.ind[1],
            global.max.mat.ind = global.max.mat.ind[1],
            PropGlobalAtSite_min = round(min(PropGlobalAtSite),0),
            PropGlobalAtSite_max = round(max(PropGlobalAtSite),0),
            EUcountry = EUcountry[1],
            global.best.mat.ind.sub.sp.pop = global.best.mat.ind.sub.sp.pop[1],
            subspecies = subspecies[1],
            eu.pop.best.mat.ind = eu.pop.best.mat.ind[1],
            EU_Annex1BirdsDirective = EU_Annex1BirdsDirective[1],
            A1 = max(A1),
            B1a = max(B1a),
            C1 = max(C1),
            A4 = max(A4),
            B3b = max(B3b),
            C4 = max(C4),
            B3a = max(B3a),
            C2 = max(C2),
            IBA.Criteria.Met = IBA.Criteria.Met[1])



head(temp)



finalSite
finalSite.polygons

## get the sites that overlap with the IBA record
sf::sf_use_s2(FALSE) # run this if issues with st_intersection: https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
temp.id <- st_intersection(temp.iba.map, dat.sf) %>% 
  dplyr::select(BoundaryFileNameRevised)





## Filter spatial data for the records linked to the IBA and those not linked
records.overlap <- dat.sf %>% dplyr::filter(BoundaryFileNameRevised %in% temp.id$BoundaryFileNameRevised)
records.not.overlapping <- dat.sf %>% dplyr::filter(!BoundaryFileNameRevised %in% temp.id$BoundaryFileNameRevised)





"As of 20 July 2022, Jono Handley, consider script called IBaKBa_FINAL_LayersSmoothing.
This script as details about how I incoporated metadata into final KBA boundaries that
had been smoothed. I need to consider a better way to do this. I.e. for the final smoothed
polygon we want to get the species information and population records associated with the
area. We lost these during the smoothing process and merging of polygons. A simple intersect
and summary will probably do the trick."

"NB: After implementing these steps (3 learning track2KBA scripts), how you merge final sites when you have multi-species
data, or dealing with data over political boundaries, etc, will probably have to be considered case-by-case."

"More learing to following :)"



