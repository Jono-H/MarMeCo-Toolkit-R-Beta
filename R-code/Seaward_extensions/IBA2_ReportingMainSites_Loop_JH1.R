################################################################################
################################################################################

## PEW Antarctica project: Preparation of final marine IBA layers for penguins
## in Antarctica

## Updated script as of June 2020
## Jono Handley

## R version: 3.6.3

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

## Load files related to IBA main info and colonies that contributed to sites
IBA_colony_info <- read.csv(paste("./Data/Antarctica_Penguin_mIBAs/ColoniesInMainIBAs_June2020_v2.csv"))
IBA_main_info <- loadRData(paste("./Data/Rasters/IBAs_CellsJune2020/","64sites_mIBA_Antarctica_Info_v2.Rdata", sep=""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Note the updates to original data - already accounted for new CHP and missing EMP colonies
### not in the original MAPPPD datasource
### did some of this manually in excel. Big hack was adding bearing to each colony
### manually - did this by editing file in ArcMap. Future iterations of analysis
### should seek enhanced solution!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IBA_input_data <- read.csv(paste("./Data/PopData/",
                                 "mIBA_Antarctica_PenguinColPopData_ForagingRadius_June2020_Bearing.csv", sep=""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load main IBA shapefile - the version from ArcMap which has clockwise colony
## numbering associated with it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## IBA_Clipped <- st_read(paste("./Data/Shapefiles/mIBAs_49sites/","ALL_mIBAs_Antarctica_Clipped_v2CWise.shp", sep="")) ## OLD, original analysis - pre June 2020

#IBA_Clipped <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_mIBAs_Antarctica_Clipped_v1CWise.shp") # v1 - After checking Clock label 7 - deleted an unlikely section of this IBA
IBA_Clipped <- st_read("./Data/Rasters/IBAs_CellsJune2020/ALL_mIBAs_Antarctica_Clipped_v2CWise.shp")

## Load base layer shapefiles
Ant_high_shp <- st_read("./Data/Shapefiles/Coastline_high_res_polygon_v7.1/Coastline_high_res_polygon_v7.1.shp")
Ant_med_shp <- st_read("./Data/Shapefiles/Coastline_medium_res_polygon/Coastline_medium_res_polygon.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Specify basemap of relevance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
basemap <- Ant_med_shp


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Following checking of individual sites in ArcMap - may need to exclude some sites because
actually they are just weird artefacts of the input steps in the analysis."
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(IBA_Clipped)
IBA_Clipped <- IBA_Clipped %>% arrange(Poly_ID)
IBA_Clipped %>% arrange(Clock_Lbl)

## Keep or not Afterclip
unique(IBA_Clipped$AfterClip)
table(IBA_Clipped$AfterClip)

## Now check other relevant files
head(IBA_colony_info)
head(IBA_main_info)
head(IBA_Clipped)

## Find the Poly_ID of the polygon(s) to remove: N means No, do not keep after clip
p <- IBA_Clipped %>% filter(AfterClip == "N") %>% dplyr::select(Poly_ID)

## Now, remove this from the other files too
unique(IBA_Clipped$Poly_ID)
unique(IBA_main_info$Poly_ID)
unique(IBA_colony_info$Poly_ID)
## remove
IBA_Clipped <- IBA_Clipped %>% filter(!Poly_ID %in% p$Poly_ID)
IBA_main_info <- IBA_main_info %>% filter(!Poly_ID %in% p$Poly_ID) 
IBA_colony_info <- IBA_colony_info %>% filter(!Poly_ID %in% p$Poly_ID)
## Check again
unique(IBA_Clipped$Poly_ID)
unique(IBA_main_info$Poly_ID)
unique(IBA_colony_info$Poly_ID)


## FIRST for MAIN IBA INFO

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Arrange and rename Poly_IDs after having removed the sites no longer deemed
### adequate IBA boundaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Check  relevant files
head(IBA_colony_info)
head(IBA_main_info)
head(IBA_Clipped)

## Make sure each is arranged by Poly_ID, then re-number Poly_ID from 1 again - NOTING FOR EACH UNIQUE POLY_ID
IBA_colony_info <- IBA_colony_info %>% arrange(Poly_ID) %>% mutate(Poly_ID2 = cumsum(!duplicated(Poly_ID)))
IBA_main_info <- IBA_main_info %>% arrange(Poly_ID) %>% mutate(Poly_ID2 = cumsum(!duplicated(Poly_ID)))
IBA_Clipped <- IBA_Clipped %>% arrange(Poly_ID) %>% mutate(Poly_ID2 = cumsum(!duplicated(Poly_ID)))

## Check  relevant files
tail(IBA_colony_info)
tail(IBA_main_info)
tail(IBA_Clipped)
head(IBA_Clipped,10) ## NB Poly_ID from the original should now be removed and renamed here :)

## Select appropriate columns and delete checking columns
IBA_colony_info$Poly_ID <- IBA_colony_info$Poly_ID2
IBA_main_info$Poly_ID <- IBA_main_info$Poly_ID2
IBA_Clipped$Poly_ID <- IBA_Clipped$Poly_ID2

IBA_colony_info <- IBA_colony_info %>% dplyr::select(-Poly_ID2)
IBA_main_info <- IBA_main_info %>% dplyr::select(-Poly_ID2)
IBA_Clipped <- IBA_Clipped %>% dplyr::select(-Poly_ID2, -AfterClip) ## also removing the AfterClip column

## Check  relevant files
tail(IBA_colony_info)
tail(IBA_main_info)
tail(IBA_Clipped)
head(IBA_Clipped,10) ## NB Poly_ID from the original should now be removed and renamed here :)


## Plot to test and check these layers still align
t = 10
plot(st_geometry(IBA_Clipped))
IBA_Clipped %>% filter(Poly_ID == t) %>% st_geometry() %>% plot(add=T, border="red")
IBA_main_info %>% filter(Poly_ID == t)
IBA_colony_info %>% filter(Poly_ID == t)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Will now also need to amend the Clock_Lbl column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sort(unique(IBA_Clipped$Clock_Lbl))
## Make sure each is arranged by Clock_Lbl, then re-number Clock_Lbl from 1 again - NOTING FOR EACH UNIQUE Clock_Lbl
IBA_Clipped <- IBA_Clipped %>% arrange(Clock_Lbl) %>% mutate(Clock_Lbl2 = cumsum(!duplicated(Clock_Lbl)))
head(IBA_Clipped,10) 
tail(IBA_Clipped,10) 
## Select appropriate columns and delete checking columns
IBA_Clipped$Clock_Lbl <- IBA_Clipped$Clock_Lbl2

IBA_Clipped <- IBA_Clipped %>% dplyr::select(-Clock_Lbl2) 

## Check  relevant files
head(IBA_Clipped,10)
tail(IBA_Clipped,10)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final check that area of polygons is correct after all the fiddling between ArcMap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## re-calculate area of polygons
IBA_Clipped$area_m2 <- drop_units(st_area(IBA_Clipped))
dim(IBA_Clipped)

## Save new files
save(IBA_Clipped, file = "./Data/Rasters/IBAs_CellsJune2020/63mIBAs_Antarctica_Clipped_v2CWise.Rdata")
st_write(IBA_Clipped, "./Data/Rasters/IBAs_CellsJune2020/63mIBAs_Antarctica_Clipped_v2CWise.Shp", delete_layer = T)


## Save updated colony_info and main_info going into mIBAs
head(IBA_colony_info)
unique(IBA_colony_info$Poly_ID)
head(IBA_main_info)
unique(IBA_main_info$Poly_ID)

write.csv(IBA_colony_info, "./Data/Antarctica_Penguin_mIBAs/ColoniesIn_63_MainIBAs_June2020_v2.csv", row.names = F)
save(IBA_main_info, file = paste("./Data/Rasters/IBAs_CellsJune2020/","63sites_mIBA_Antarctica_Info_v2.Rdata", sep=""))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Considering height / width ratios for all plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Renaming of object for alignment with older script version
IBA_main_sf <- IBA_Clipped

## Create sf file for species colonies
col_locs <- st_as_sf(IBA_input_data, 
                     coords = c("longitude_epsg_4326", "latitude_epsg_4326"), crs=4326) # 4236 = geographic WGS84

## convert projection to basemap
col_locs <- st_transform(col_locs, crs = st_crs(basemap))

## Blank data frame
plot_ratio <- data.frame(Poly_ID = as.numeric(), Plot_ratio = as.numeric())

## Create extent object for plot
buff = 100000 ## in meters because of projection type

for(p in 1:max(IBA_main_sf$Poly_ID)){
  
  print(paste("Loop", p))
  ## Subset a polygon of relevance
  p_temp <- IBA_main_sf %>% filter(Poly_ID == p)
  
  ## SUbset colonies of relevance
  p_cols_temp <- IBA_colony_info %>% filter(Poly_ID == p)
  
  ## subset spatial part of layer relating to colonies
  col_iba <- col_locs %>% filter(common_name %in% unique(p_cols_temp$Species)) %>% 
    filter(site_id %in% unique(p_cols_temp$Colony))
  
  ## Merge the two spatial layers together: IBA of relevance plus associated colonies
  site <- st_union(p_temp, col_iba)
  
  ## Create extent object for plot
  #st_bbox(site)
  buff = 100000 ## in meters because of projection type
  ## extent object for tmap - note difference in x/y order compared to st_bbox
  ext_site <- extent(st_bbox(site)[[1]] - buff, #xmin
                     st_bbox(site)[[3]] + buff, #xmax
                     st_bbox(site)[[2]] - buff, #ymin
                     st_bbox(site)[[4]] + buff) #ymax
  
  ## Create inset map region based on extent object
  region_site = st_bbox(c(xmin = ext_site[1], xmax = ext_site[2],
                          ymin = ext_site[3], ymax = ext_site[4]),
                        crs = st_crs(basemap)) %>% 
    st_as_sfc()
  region_site <- st_sf(region_site)
  
  ## Consider plotting ratio
  width_plt = ext_site[2] - ext_site[1]
  height_plt =  ext_site[4] - ext_site[3]
  
  temp_ratio <- data.frame(Poly_ID = p, Plot_ratio = height_plt / width_plt)
  plot_ratio <- rbind(plot_ratio, temp_ratio)
}
plot_ratio
hist(plot_ratio$Plot_ratio)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Plot individual marine IBAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(p in 1:max(IBA_main_sf$Clock_Lbl)){

  print(paste("Loop",p,sep=" "))
  
## Subset a polygon of relevance
p_shp <- IBA_main_sf %>% filter(Clock_Lbl == p)
p_id <- p_shp$Poly_ID[1]
  
## Get associated data
p_cols <- IBA_colony_info %>% filter(Poly_ID == p_id)
p_iba <- IBA_main_info %>% filter(Poly_ID == p_id)
other_shp <- IBA_main_sf %>% filter(Poly_ID != p_id)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Buffer area around IBA polgon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Consider enhancing the size of the extent object to ensure inclusion of all colonies 
## that contributed to IBA of relevance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## SUbset colonies of relevance
col_iba <- col_locs %>% filter(common_name %in% unique(p_cols$Species)) %>% 
  filter(site_id %in% unique(p_cols$Colony))

## create new shapefile - nb: ensure projections are the same!
site <- st_union(p_shp, col_iba)

## Create extent object for plot
#st_bbox(site)
buff = 100000 ## in meters because of projection type
## extent object for tmap - note difference in x/y order compared to st_bbox
ext_site <- extent(st_bbox(site)[[1]] - buff, #xmin
              st_bbox(site)[[3]] + buff, #xmax
              st_bbox(site)[[2]] - buff, #ymin
              st_bbox(site)[[4]] + buff) #ymax

## Create inset map region based on extent object
region_site = st_bbox(c(xmin = ext_site[1], xmax = ext_site[2],
                   ymin = ext_site[3], ymax = ext_site[4]),
                 crs = st_crs(basemap)) %>% 
  st_as_sfc()
region_site <- st_sf(region_site)

#plot(st_geometry(IBA_main_sf))
#plot(st_geometry(region_site), add= T, border="#e34a33", fill=NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Use tmap - see script version previous to JH4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plotting with ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set line width of plots to enhance output once saving via ggsave/other
linewidth = 0.3

## Plot main base layer first. I.e The polygon of interest
base <- ggplot(data = p_shp) +
  geom_sf(fill = "#ef8a62", lwd = linewidth) +
  coord_sf(xlim = c(ext_site[1], ext_site[2]), #xmin, xmax
           ylim = c(ext_site[3], ext_site[4]))  #ymin, ymax

#base

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modify basemap colours & ## plot with north arrow + other layers
## E.g. https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(basemap$surface)
basemap <- basemap %>% mutate(surf_colour = case_when(
  surface == "land" ~ "#C0C0C0",
  surface == "ice shelf" ~ "#0080FF", 
  surface == "ice tongue" ~ "#0080FF",
  surface == "rumple" ~ "#0080FF"))
basemap
table(basemap$surface)
table(basemap$surf_colour)


base <- 
  base +
  geom_sf(data = other_shp, fill = "#fddbc7", lwd = linewidth) +
  geom_sf(data = basemap, fill = basemap$surf_colour, lwd = linewidth) +
  coord_sf(xlim = c(ext_site[1], ext_site[2]), #xmin, xmax
           ylim = c(ext_site[3], ext_site[4])) +  #ymin, ymax +
  # background
  theme(panel.background = element_rect(fill="lightblue"),
        #panel.grid.major = element_blank(),
        panel.grid.major = element_line(size = linewidth),
        panel.border = element_rect(fill=NA, colour = "black", size=0.5)) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         style = north_arrow_orienteering,
                         height = unit(0.3, "cm"),
                         width = unit(0.3, "cm")) + # Note, text size of N arrow changed within function. See line 230
  annotation_scale(location = "br", text_cex = 0.4)

#base



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Trying to include custom icons # See JH8 - Custom icons did not work the best...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read penguin icon in
#library(png)
#library(ggimage)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Update basemap to include species colony plot locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Colony plot with specified colours
base <- 
  base +
  geom_point(data = col_locs, 
             aes(x = st_coordinates(col_locs)[,1], 
                 y = st_coordinates(col_locs)[,2],
                 fill = common_name), pch = 21, size = 1.2, show.legend = F) +
  scale_fill_manual(breaks = c("ADP", "CHP", "GEP", "EMP"), 
                    values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571")) +
  theme(axis.title = element_blank())

#base

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Now update to include those colonies which triggered the relevant marine IBA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SUbset colonies of relevance
dim(col_locs)
col_iba <- col_locs %>% filter(common_name %in% unique(p_cols$Species)) %>% 
  filter(site_id %in% unique(p_cols$Colony))
dim(col_iba)


## Highlighting points with colonies which contributed - plot these again to
## try and get colony colours matching correctly.
base <- 
  base +
  geom_point(data = col_iba, 
             aes(x = st_coordinates(col_iba)[,1], 
                 y = st_coordinates(col_iba)[,2],
                 fill = common_name), pch = 21, size = 1.2, show.legend = F) +
  scale_fill_manual(breaks = c("ADP", "CHP", "GEP", "EMP"), 
                    values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571")) +
  theme(axis.title = element_blank())

#base


## Highlighting points with colonies which contributed - highlight with colour call
## which will highlight the outside of the circle
base <- 
  base +
  geom_point(data = col_iba, 
             aes(x = st_coordinates(col_iba)[,1], 
                 y = st_coordinates(col_iba)[,2]), 
             colour = "magenta", shape = 1, size = 1.2, show.legend = F, stroke = 0.8) +
  #scale_fill_manual(breaks = c("ADP", "CHP", "GEP", "EMP"), 
  #                  values=c("#a6611a", "#dfc27d", "#80cdc1", "#018571")) +
  theme(axis.title = element_blank())
#base

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Colony names for those colonies which contributed to the mIBA - using ggrepel
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#base <-  
# base +
#    geom_text_repel(data = col_iba, 
#               aes(x = st_coordinates(col_iba)[,1], 
#                   y = st_coordinates(col_iba)[,2], label = site_id), size = 1.5) +
#  theme(text = element_text(size=8))

#base    

## Check against overall area of Antarctica
## New plot required

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create inset map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a text
grob <- grobTree(textGrob(paste("Antarctica mIBA: ",p,sep=""), x=0.01,  y=0.03, hjust=0,
                          gp=gpar(col="black", fontsize=10)))

## Inset map with no margin and text on axes
inset_map <- ggplot(data=basemap) +
  geom_sf(lwd = 0.2,  fill = basemap$surf_colour)+
  theme(panel.background = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.ticks.length = unit(0, "pt"),
        plot.margin=grid::unit(c(0,0,0,0), "mm"),
        panel.border = element_rect(fill=NA, colour = "black", size=0.5)) +
  geom_sf(data = other_shp, fill = "#fddbc7", lwd = linewidth) +
  geom_sf(data = p_shp, fill = "#ef8a62", lwd = linewidth) +
  geom_sf(data = region_site, col = "red", fill=NA) +
  annotation_custom(grob)


  
#inset_map


## Key basemap and inset map (see previous script version for details about placement of inset map)
## Option to place inset outside of map and adjacent to right hand side
hw_ratio = ext_site[2] - ext_site[1]
base_inset <- base + annotation_custom(grob = ggplotGrob(inset_map),
                                       xmin = Inf, 
                                       xmax = ext_site[2]+hw_ratio,
                                       ymin = ext_site[3], 
                                       ymax = ext_site[4])

#base_inset

## Consider plotting ratio
ratio <- plot_ratio %>% filter(Poly_ID == p_id)
#ratio

ggsave(filename = paste("./Data/Antarctica_Penguin_mIBAs/Plots1_June2020/","Ant_mIBA_ClockID_",p,"_baseinset.png",sep=""), # from non colony labelled version
       base_inset, 
       dpi = 600, units = "mm", width = 180, 
       #height = 90),
       height = ifelse(ratio$Plot_ratio>1.3, 90,
                       ifelse(ratio$Plot_ratio <= 1.3 & ratio$Plot_ratio > 1.1,75,60)))


ggsave(filename = paste("./Data/Antarctica_Penguin_mIBAs/Plots1_June2020/","Ant_mIBA_ClockID_",p,"_base.png",sep=""), # from non colony labelled version
       base, 
       dpi = 600, units = "mm", width = 110, 
       height = 110)
       

ggsave(filename = paste("./Data/Antarctica_Penguin_mIBAs/Plots1_June2020/","Ant_mIBA_ClockID_",p,"_inset.png",sep=""), # from non colony labelled version
       inset_map, 
       dpi = 300, units = "mm", width = 90, 
       height = 90)
       
rm(grob, base, inset_map, base_inset)

}



#png(filename = paste("./Data/Antarctica_Penguin_mIBAs/Plots/","Ant_mIBA_PolyID_",p,".png",sep=""),
 #    units = "mm", width = 180, 
    #height = 90),
  #  height = ifelse(ratio$Plot_ratio>1.3, 90, 60))
#base_inset

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Additional data needed for reporting individual sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### MPA layers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"MAKE SURE YOU HAVE LATEST VERSIONS"

plot(st_geometry(basemap))

## Key Confirmed MPAs - Source: https://data.ccamlr.org/dataset/marine-protected-areas
MPA_confirmed <- st_read(paste("./Data/Enviro_Management_layers/confirmed_mpa_epsg/","mpa-shapefile-EPSG102020.shp", sep=""))
plot(st_geometry(MPA_confirmed), add=T)
MPA_confirmed
## South Orkney Islands & Ross Sea MPA
SORK_MPA <- MPA_confirmed %>% filter(GAR_ID == 92652)
plot(st_geometry(SORK_MPA), add=T, border = "red")
ROSS_MPA <- MPA_confirmed %>% filter(GAR_ID != 92652)
plot(st_geometry(ROSS_MPA), add=T, border = "blue")

## Weddell Sea Proposed MPAs - Source: Katharina Teschke (March 2020)
WSMPA <- st_read(paste("./Data/Shapefiles/Antarctica_ProposedMPAs/19-07 All MPAs/19-07 All MPAs/Weddell Sea/Mar2020/","WSMPA_2019boundaries_EPSG102020.shp", sep=""))
plot(st_geometry(WSMPA), add=T, border = "green")

## East Antarctica Proposed MPA
EARSMPA <- st_read(paste("./Data/Shapefiles/Antarctica_ProposedMPAs/19-07 All MPAs/19-07 All MPAs/East Antatrctic/","EARSMPA_Erase_09232015.shp", sep=""))
plot(st_geometry(EARSMPA), add=T, border = "orange")

## Domain 1 Antarctic Peninsula Proposed MPAs
#D1MPA <- st_read(paste("./Data/Shapefiles/Antarctica_ProposedMPAs/19-07 All MPAs/19-07 All MPAs/Antarctic Peninsula/","D1 MPA Preliminary Proposal.shp", sep=""))
D1MPA <- st_read(paste("./Data/Shapefiles/Antarctica_ProposedMPAs/19-07 All MPAs/19-07 All MPAs/Antarctic Peninsula/Apr2020/D1MPA-model-2019/","D1MPA-model-2019.shp", sep=""))
#plot(st_geometry(D1MPA), border = "brown")
plot(st_geometry(D1MPA), add=T, border = "brown")

"As of June 2020 - Including Weddell Sea MPA Phase 2 MPA PLANNING Polygon"
## Weddell Sea PHASE 2  -  Gary Griffith - Norweigan Polar Institute
WSMPA_phase2 <- st_read(paste("./Data/Shapefiles/Antarctica_ProposedMPAs/19-07 All MPAs/19-07 All MPAs/maud_polygon_Jun2020/","maud_polygon.shp", sep=""))
plot(st_geometry(WSMPA_phase2), add=T, border = "green")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Other layers of significance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Voluntary krill buffers - source: Phil Trathan (British Antarctic Surey)
KBUFF_gerlache <- st_read(paste("./Data/Enviro_Management_layers/voluntary krill buffers/","XY Gerlache buffers.shp", sep=""))
plot(st_geometry(KBUFF_gerlache), add=T, colour = "red")
KBUFF_peninsula <- st_read(paste("./Data/Enviro_Management_layers/voluntary krill buffers/","XY Peninsular buffers.shp", sep=""))
plot(st_geometry(KBUFF_peninsula), add=T, colour = "red")
KBUFF_shetland <- st_read(paste("./Data/Enviro_Management_layers/voluntary krill buffers/","XY SouthShetland buffers.shp", sep=""))
plot(st_geometry(KBUFF_shetland), add=T, colour = "red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Check projections
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
proj4string(as_Spatial(KBUFF_gerlache))
proj4string(as_Spatial(KBUFF_peninsula))
proj4string(as_Spatial(KBUFF_shetland))
proj4string(as_Spatial(D1MPA))
proj4string(as_Spatial(EARSMPA))
proj4string(as_Spatial(WSMPA))
proj4string(as_Spatial(WSMPA_phase2))
proj4string(as_Spatial(ROSS_MPA))
proj4string(as_Spatial(SORK_MPA))
proj4string(as_Spatial(MPA_confirmed))
#proj4string(as_Spatial(IBA_rast))
proj4string(as_Spatial(IBA_Clipped))
proj4string(as_Spatial(basemap))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Assign all to same projection as final marine IBAs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
KBUFF_gerlache <- st_transform(KBUFF_gerlache, crs = st_crs(basemap))
KBUFF_peninsula <- st_transform(KBUFF_peninsula, crs = st_crs(basemap))
KBUFF_shetland <- st_transform(KBUFF_shetland, crs = st_crs(basemap))
D1MPA <- st_transform(D1MPA, crs = st_crs(basemap))
EARSMPA <- st_transform(EARSMPA, crs = st_crs(basemap))
WSMPA <- st_transform(WSMPA, crs = st_crs(basemap))

WSMPA_phase2 <- st_set_crs(WSMPA_phase2, st_crs(basemap))

ROSS_MPA <- st_transform(ROSS_MPA, crs = st_crs(basemap))
SORK_MPA <- st_transform(SORK_MPA, crs = st_crs(basemap))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Plot all layers again to check
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(st_geometry(basemap))
plot(st_geometry(SORK_MPA), add=T, border = "red")
plot(st_geometry(ROSS_MPA), add=T, border = "blue")

plot(st_geometry(WSMPA), add=T, border = "green")
plot(st_geometry(WSMPA_phase2), add=T, border = "green")

plot(st_geometry(EARSMPA), add=T, border = "orange")
plot(st_geometry(D1MPA), add=T, border = "brown")
plot(st_geometry(KBUFF_gerlache), add=T, col = "red")
plot(st_geometry(KBUFF_peninsula), add=T, col = "red")
plot(st_geometry(KBUFF_shetland), add=T, col = "red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Simplify & Standardise MPA spatial layers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
KBUFF_gerlache <- KBUFF_gerlache %>%  mutate(Area_name = "Krill buffer: Gerlache", Reference = "Krill buffer: Gerlache") %>% 
  dplyr::select(!Id) %>% mutate(Type = "Krill buffer")

KBUFF_peninsula <- KBUFF_peninsula %>%  mutate(Area_name = "Krill buffer: Antarctic Peninsula", Reference = "Krill buffer: Antarctic Peninsula") %>% 
  dplyr::select(!Id) %>% mutate(Type = "Krill buffer")

KBUFF_shetland <- KBUFF_shetland %>%  mutate(Area_name = "Krill buffer: South Shetland Islands", Reference = "Krill buffer: South Shetland Islands") %>% 
  dplyr::select(!Id) %>% mutate(Type = "Krill buffer")

D1MPA <- D1MPA %>% mutate(Area_name = "D1MPA") %>% dplyr::select(Area_name, Reference) %>% mutate(Type = "Proposed")
EARSMPA <- EARSMPA %>% mutate(Area_name = "EARSMPA") %>% dplyr::select(Area_name, Name) %>% rename(Reference = Name) %>% mutate(Type = "Proposed")
WSMPA <- WSMPA %>% mutate(Area_name = "WSMPA_P1", Reference = "WSMPA_P1") %>% dplyr::select(Area_name, Reference) %>% mutate(Type = "Proposed")
ROSS_MPA <- ROSS_MPA %>% mutate(Area_name = "Ross Sea MPA") %>% dplyr::select(Area_name, LongLabel) %>% rename(Reference = LongLabel) %>% mutate(Type = "Confirmed")
SORK_MPA <- SORK_MPA %>% mutate(Area_name = "South Orkney Islands southern shelf MPA") %>% dplyr::select(Area_name, LongLabel) %>% rename(Reference = LongLabel) %>% mutate(Type = "Confirmed")

WSMPA_phase2 <- WSMPA_phase2 %>% mutate(Area_name = "WSMPA_P2", Reference = "WSMPA_P2") %>% dplyr::select(Area_name, Reference) %>% mutate(Type = "Planning_domain")


## Merge MPAs - easier to do for multiple polygons using sp package
KBUFF_gerlache <- as_Spatial(KBUFF_gerlache)
KBUFF_peninsula <- as_Spatial(KBUFF_peninsula)
KBUFF_shetland <- as_Spatial(KBUFF_shetland)
D1MPA <- as_Spatial(D1MPA)
EARSMPA <- as_Spatial(EARSMPA)

WSMPA <- as_Spatial(WSMPA)
WSMPA_phase2 <- as_Spatial(WSMPA_phase2)

ROSS_MPA <- as_Spatial(ROSS_MPA)
SORK_MPA <- as_Spatial(SORK_MPA)
SORK_MPA

## MPA file
all_MPAs <- bind(D1MPA, EARSMPA, WSMPA, WSMPA_phase2, ROSS_MPA, SORK_MPA)
data.frame(all_MPAs)

## Convert all_MPAs back to sf
all_MPAs_sf <- st_as_sf(all_MPAs)
plot(st_geometry(all_MPAs_sf))

## Clip out the portion of the IBAs that are in the MPAs
IBA_in_MPA <-st_intersection(IBA_main_sf, all_MPAs_sf)
dim(IBA_in_MPA)
data.frame(IBA_in_MPA)
IBA_in_MPA %>% arrange(Clock_Lbl)

## Krill buffer file
KBuffs <- bind(KBUFF_gerlache, KBUFF_shetland, KBUFF_peninsula)
data.frame(KBuffs)

## Convert all_MPAs back to sf
KBuffs_sf <- st_as_sf(KBuffs)
plot(st_geometry(KBuffs_sf))

## Clip out the portion of the IBAs that are in the Krill Buffer Zones
IBA_in_Kbuff <-st_intersection(IBA_main_sf, KBuffs_sf)
dim(IBA_in_Kbuff)
data.frame(IBA_in_Kbuff)
IBA_in_Kbuff %>% arrange(Clock_Lbl)
plot(st_geometry(IBA_in_Kbuff), add = T, border = "red")

## Key files
IBA_in_MPA
IBA_in_Kbuff


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reporting in basic word document
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#install.packages("officer")
#install.packages("flextable")
library(officer)
library(flextable)
source("./R_code_PEW_Antarctica/Functions_WordDoc.R")

## Create list of necessary plots
List_sites <- dir(path="./Data/Antarctica_Penguin_mIBAs/Plots1_June2020/",pattern="_base.png$",full.names=T)
List_overall <- dir(path="./Data/Antarctica_Penguin_mIBAs/Plots1_June2020/",pattern="_inset.png$",full.names=T)

## Subset a polygon of relevance
p_shp <- IBA_main_sf %>% filter(Clock_Lbl == 1)
p_id <- p_shp$Poly_ID[1]

## Get associated data
p_cols <- IBA_colony_info %>% filter(Poly_ID == p_id)
p_iba <- IBA_main_info %>% filter(Poly_ID == p_id)
p_mpa <- IBA_in_MPA %>% filter(Poly_ID == p_id)

## Get associated plot file names
site_plot <- grep(paste("ClockID_",p,sep=""), List_sites, value = T)
overall_plot <- grep(paste("ClockID_",p,sep=""), List_overall, value = T)

## Convert main IBA to WGS84 for decimal degree needs
p_wgs84 <- st_transform(p_shp, crs = 4326) %>% as_Spatial() # 4236 = geographic WGS84

## Consider key data needed for reporting
Site_name <- p_shp$Clock_Lbl[1] # Name of IBA site
Site_code <- p_shp$Clock_Lbl[1] # Code of IBA site
IBA_criteria <- "A4: Congregations" # Key IBA criteria triggered
IBA_Lat_Dec <- round(coordinates(p_wgs84)[2],3) # Latitude in Decimal degrees
IBA_Lon_Dec <- round(coordinates(p_wgs84)[1],3) # Longitude in Decimal degrees
IBA_Area_ha <- drop_units(st_area(p_shp))/10000 # Area in hectares
IBA_Area_km2 <- drop_units(st_area(p_shp))/1000000 # Area in km square
IBA_Species <- paste(unique(p_iba$Species), collapse=",") # Key species in marine IBA
Terrestrial_IBAs # Links to Harris et al. 2015 IBAs
MPA_Overlap <- p_mpa$Area_name[1]# Current conservation measures relevant to IBA
MPA_Type <- p_mpa$Type[1]
MPA_Zones <- paste(unique(p_mpa$Reference), collapse=",")# Current conservation measures relevant to IBA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Generate key data frames to go with the IBA in question
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Marine IBA SITE Info
p_site_table <- data.frame(Site_code = Site_code,
                           Species = IBA_Species,
                           IBA_criteria = IBA_criteria,
                           Decimal_Latitude = IBA_Lat_Dec,
                           Decimal_Longitude = IBA_Lon_Dec,
                           Area_ha = IBA_Area_ha,
                           Area_km2 = IBA_Area_km2,
                           MPA_Overlap = ifelse(is.na(MPA_Overlap),"",MPA_Overlap),
                           MPA_Type = MPA_Type,
                           MPA_Zones = ifelse(is.na(MPA_Zones),"",MPA_Zones))
                           
 
"Still to include"                          
#Terrestrial_IBAs # Links to Harris et al. 2015 IBAs
#IBA_Protection # Current conservation measures relevant to IBA

p_site_table

## Marine IBA SPECIES Info
p_iba <- p_iba %>% arrange(Species, 
                           fct_relevel(Count_type, "min", "max", "median", "recent"))

p_iba_table <- data.frame(Site_code = Site_code,
                          Species = p_iba[,2],
                          Count_type = p_iba[,3],
                          Lower = p_iba[,4],
                          Upper = p_iba[,5])

p_iba_table

## Colony info related to marine IBA
p_cols <- p_cols %>% arrange(Species, Colony,
                   fct_relevel(Count_type, "min", "max", "median", "recent"))

p_cols_table <- data.frame(Site_code = Site_code,
                           Species = p_cols[,3],
                           Colony = p_cols[,8], # consider full colony name here
                           Count_type = p_cols[,4],
                           Lower = p_cols[,5],
                           Upper = p_cols[,6],
                           Count_year = p_cols[,7])

p_cols_table

## Check key tables
p_site_table
p_iba_table
p_cols_table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## See: "https://davidgohel.github.io/officer/articles/offcran/word.html"
## See: https://davidgohel.github.io/officer/articles/offcran/word.html

## Create blank template document
rm(my_doc)
my_doc <- read_docx()

## add the report title and an empty line
#add.title(my_doc, paste("Antarctica marine IBA: ", Site_name, sep=""))

## or add the report title and an empty line via direct officer package
my_doc <- body_add_par(my_doc, 
                       paste("Antarctica marine IBA: ", Site_name, sep=""), 
                       style = "Normal", pos = "on")
## See options for text type
#body_add_par(my_doc, "Hello world!", style = "", pos = "on")

## Add a space after the title
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")

## Add table describing overall site details
test_table <- data.frame(Var = row.names(t(p_site_table)), Value = t(p_site_table)[,1])
##
my_doc <- body_add_table(my_doc, 
                         test_table, 
                         header = F,
                         first_row = F,
                         style = "Table Professional") 

## Add a space after the table
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")


## Add MAIn IBA figure to page (Keep same size ratio as saved plot from above)
#my_doc <- body_add_img(my_doc, src = paste("./Data/Antarctica_Penguin_mIBAs/Plots/","TESTggsave_Clock49.png",sep=""),
#             width = 7.08, 
#             height = ifelse(ratio$Plot_ratio>1.3, 3.54, 2.4), 
#             style = "Normal",
#             pos = "after")

my_doc <- body_add_img(my_doc, src = paste(site_plot),
             width = 4.33, 
             height = 4.33, 
             style = "centered",
             pos = "after")

my_doc <- body_add_img(my_doc, src = paste(overall_plot),
                       width = 4.33, 
                       height = 4.33, 
                       style = "centered",
                       pos = "after")


## Add a figure caption
my_doc <- body_add_par(my_doc, 
                       paste("Figure A", 
                             Site_code,".1: ",
                             "Focal Antarctica marine IBA ", Site_name,
                             " (orange polygon) for penguins, and other marine IBAs", 
                             " (light orange) for penguins identified during the project. ",
                             "Colonies circled in purple are those whose populations contributed ",
                             "to the marine IBA triggering IBA criteria A4 (>1% of the global population).",
                             " See Table A", Site_code,".2.",
                             sep=""), 
                       style = "Normal", pos = "after")


## Add an empty row after the figure caption
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")

## Add a title for the table describing the key population estimates for species
p_iba_table
my_doc <- body_add_par(my_doc, 
                       paste("Table A", 
                             Site_code,".1: ",
                             "Population estimates and count types (those that triggerd IBA Criteria A4, >1% of global population) ", 
                             "for key penguin species in Antarctic marine IBA  ",
                             Site_name,
                             ". Key sources of colony input data associated with",
                             " this marine IBA are detailed in Table A",
                             Site_code,".2.",sep=""), 
                       style = "Normal", pos = "after")

## Add an empty row after the table title
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")

## Add table describing the key population estimates for species
my_doc <- body_add_table(my_doc, 
                         p_iba_table, 
                         header = T,
                         first_row = T,
                         style = "Normal Table",
                         last_row = T) 

## Add an empty row after the table
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")

## Add a title for the table describing the population estimates used from each colony
p_cols_table
my_doc <- body_add_par(my_doc, 
                       paste("Table A", 
                             Site_code,".2: ",
                             "Population estimates and count types for penguin species colony(ies) ", 
                             "that contributed to the delineation of Antarctic marine IBA  ",
                             Site_name,
                             sep=""), 
                       style = "Normal", pos = "after")

## Add an empty row after the table title
my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")

## Add table describing the population estimates used from each colony
my_doc <- body_add_table(my_doc, 
                         p_cols_table, 
                         header = T,
                         first_row = T,
                         style = "Normal Table",
                         last_row = T) 

## End document by adding page break - therefore, new marine IBA site will start
## on a new page.
my_doc <- body_add_break(my_doc, pos = "after")

print(my_doc, target =  paste("./Data/Antarctica_Penguin_mIBAs/","TESTWORD.docx",sep=""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loop over for all sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create blank template document
rm(my_doc)
my_doc <- read_docx()
## create blank data frames for retrospective summary data table analysis
site_table <- data.frame()
iba_table <- data.frame()
cols_table <- data.frame()


for(p in 1:nrow(IBA_main_sf)){
  
  ##
  print(paste("Loop",p))
  
  ## Subset a polygon of relevance
  p_shp <- IBA_main_sf %>% filter(Clock_Lbl == p)
  p_id <- p_shp$Poly_ID[1]
  
  ## Get associated data
  p_cols <- IBA_colony_info %>% filter(Poly_ID == p_id)
  p_iba <- IBA_main_info %>% filter(Poly_ID == p_id)
  p_mpa <- IBA_in_MPA %>% filter(Poly_ID == p_id)
  
  ## Get associated plot file names
  site_plot <- grep(paste("ClockID_",p,"_",sep=""), List_sites, value = T)
  overall_plot <- grep(paste("ClockID_",p,"_",sep=""), List_overall, value = T)
  
  ## Convert main IBA to WGS84 for decimal degree needs
  p_wgs84 <- st_transform(p_shp, crs = 4326) %>% as_Spatial() # 4236 = geographic WGS84
  
  ## Consider key data needed for reporting
  Site_name <- p_shp$Clock_Lbl[1] # Name of IBA site
  Site_code <- p_shp$Clock_Lbl[1] # Code of IBA site
  IBA_criteria <- "A4: Congregations" # Key IBA criteria triggered
  IBA_Lat_Dec <- round(coordinates(p_wgs84)[2],3) # Latitude in Decimal degrees
  IBA_Lon_Dec <- round(coordinates(p_wgs84)[1],3) # Longitude in Decimal degrees
  IBA_Area_ha <- drop_units(st_area(p_shp))/10000 # Area in hectares
  IBA_Area_km2 <- drop_units(st_area(p_shp))/1000000 # Area in km square
  IBA_Species <- paste(unique(p_iba$Species), collapse=",") # Key species in marine IBA
  #Terrestrial_IBAs # Links to Harris et al. 2015 IBAs
  MPA_Overlap <- p_mpa$Area_name[1]# Current conservation measures relevant to IBA
  MPA_Type <- p_mpa$Type[1]
  MPA_Zones <- paste(unique(p_mpa$Reference), collapse=",")# Current conservation measures relevant to IBA
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Generate key data frames to go with the IBA in question
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Marine IBA SITE Info
  p_site_table <- data.frame(Site_code = Site_code,
                             Species = IBA_Species,
                             IBA_criteria = IBA_criteria,
                             Decimal_Latitude = IBA_Lat_Dec,
                             Decimal_Longitude = IBA_Lon_Dec,
                             Area_ha = IBA_Area_ha,
                             Area_km2 = IBA_Area_km2,
                             MPA_Overlap = ifelse(is.na(MPA_Overlap),"",MPA_Overlap),
                             MPA_Type = MPA_Type,
                             MPA_Zones = ifelse(is.na(MPA_Zones),"",MPA_Zones))
  
  
  "Still to include"                          
  #Terrestrial_IBAs # Links to Harris et al. 2015 IBAs
  #IBA_Protection # Current conservation measures relevant to IBA
  
  #p_site_table
  
  ## Marine IBA SPECIES Info
  p_iba <- p_iba %>% arrange(Species, 
                             fct_relevel(Count_type, "min", "max", "median", "recent"))
  
  p_iba_table <- data.frame(Site_code = Site_code,
                            Species = p_iba[,2],
                            Count_type = p_iba[,3],
                            Lower = p_iba[,4],
                            Upper = p_iba[,5])
  
  #p_iba_table
  
  ## Colony info related to marine IBA
  p_cols <- p_cols %>% arrange(Species, Colony,
                               fct_relevel(Count_type, "min", "max", "median", "recent"))
  
  p_cols_table <- data.frame(Site_code = Site_code,
                             Species = p_cols[,3],
                             Colony = p_cols[,8], # consider full colony name here
                             Count_type = p_cols[,4],
                             Lower = p_cols[,5],
                             Upper = p_cols[,6],
                             Count_year = p_cols[,7])
  
  #p_cols_table
  
  ## Check key tables
  #p_site_table
  #p_iba_table
  #p_cols_table
  
  ## overall dataframes
  site_table <- rbind(site_table, p_site_table)
  iba_table <- rbind(iba_table, p_iba_table)
  cols_table <- rbind(cols_table, p_cols_table)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## See: "https://davidgohel.github.io/officer/articles/offcran/word.html"
  ## See: https://davidgohel.github.io/officer/articles/offcran/word.html
  
  ## Create blank template document
  #rm(my_doc)
 #my_doc <- read_docx()
  
  ## add the report title and an empty line
  #add.title(my_doc, paste("Antarctica marine IBA: ", Site_name, sep=""))
  
  ## or add the report title and an empty line via direct officer package
  my_doc <- body_add_par(my_doc, 
                         paste("Antarctica marine IBA: ", Site_name, sep=""), 
                         style = "Normal", pos = "on")
  ## See options for text type
  #body_add_par(my_doc, "Hello world!", style = "", pos = "on")
  
  ## Add a space after the title
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  ## Add table describing overall site details
  test_table <- data.frame(Var = row.names(t(p_site_table)), Value = t(p_site_table)[,1])
  ##
  my_doc <- body_add_table(my_doc, 
                           test_table, 
                           header = F,
                           first_row = F,
                           style = "Table Professional") 
  
  ## Add a space after the table
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  
  ## Add MAIn IBA figure to page (Keep same size ratio as saved plot from above)
  #my_doc <- body_add_img(my_doc, src = paste("./Data/Antarctica_Penguin_mIBAs/Plots/","TESTggsave_Clock49.png",sep=""),
  #             width = 7.08, 
  #             height = ifelse(ratio$Plot_ratio>1.3, 3.54, 2.4), 
  #             style = "Normal",
  #             pos = "after")
  
  my_doc <- body_add_img(my_doc, src = paste(site_plot),
                         width = 4.33, 
                         height = 4.33, 
                         style = "centered",
                         pos = "after")
  
  my_doc <- body_add_img(my_doc, src = paste(overall_plot),
                         width = 4.33, 
                         height = 4.33, 
                         style = "centered",
                         pos = "after")
  
  
  ## Add a figure caption
  my_doc <- body_add_par(my_doc, 
                         paste("Figure A", 
                               Site_code,".1: ",
                               "Focal Antarctica marine IBA ", Site_name,
                               " (orange polygon) for penguins, and other marine IBAs", 
                               " (light orange) for penguins identified during the project. ",
                               "Colonies circled in purple are those whose populations contributed ",
                               "to the marine IBA triggering IBA criteria A4 (>1% of the global population).",
                               " See Table A", Site_code,".2.",
                               sep=""), 
                         style = "Normal", pos = "after")
  
  
  ## Add an empty row after the figure caption
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  ## Add a title for the table describing the key population estimates for species
  p_iba_table
  my_doc <- body_add_par(my_doc, 
                         paste("Table A", 
                               Site_code,".1: ",
                               "Population estimates and count types (those that triggered IBA Criteria A4, >1% of global population) ", 
                               "for key penguin species in Antarctic marine IBA  ",
                               Site_name,
                               ". Key sources of colony input data associated with",
                               " this marine IBA are detailed in Table A",
                               Site_code,".2. ",
                               "Population estimates refer to the lower and upper estimated number of adult breeding pairs ",
                               "likely to be using the marine IBA; derived from summing over all cells from individual colony density ",
                               "distribution surfaces which contributed to the identification of the marine IBA.",
                               sep=""), 
                         style = "Normal", pos = "after")
  
  ## Add an empty row after the table title
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  ## Add table describing the key population estimates for species
  my_doc <- body_add_table(my_doc, 
                           p_iba_table, 
                           header = T,
                           first_row = T,
                           style = "Normal Table",
                           last_row = T) 
  
  ## Add an empty row after the table
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  ## Add a title for the table describing the population estimates used from each colony
  p_cols_table
  my_doc <- body_add_par(my_doc, 
                         paste("Table A", 
                               Site_code,".2: ",
                               "Population estimates (adult breeding pairs) and count types from individual colony(ies) density ",
                               "distribution surfaces which contributed to the identification of marine IBA: ",Site_name, " .I.e. ",
                               "The lower and upper estimated number of adult breeding pairs from a given colony that ",
                               "may utilise the marine IBA. ",
                               "Note, the lower population estimate represents the lowest estimated number of breeding pairs ",
                               "from a given colony contributing to the delineation of the marine IBA site. The lower population ",
                               "estimate may therefore be zero when only part of the individual colony(ies) density ",
                               "distribution surface contributed to the marine IBA.",
                               sep=""),
                         style = "Normal", pos = "after")
  
  ## Add an empty row after the table title
  my_doc <- body_add_par(my_doc, "", style = "Normal", pos = "after")
  
  ## Add table describing the population estimates used from each colony
  my_doc <- body_add_table(my_doc, 
                           p_cols_table, 
                           header = T,
                           first_row = T,
                           style = "Normal Table",
                           last_row = T) 
  
  ## End document by adding page break - therefore, new marine IBA site will start
  ## on a new page.
  my_doc <- body_add_break(my_doc, pos = "after")

}

print(my_doc, target =  paste("./Data/Antarctica_Penguin_mIBAs/","63mIBA_SuppMaterial_June2020.docx",sep=""))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Overall summary table
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(site_table,10)
head(iba_table,2) 
head(cols_table,2) 

IBAsummary <- data.frame()
for(s in 1:nrow(IBA_main_sf)){

  #s = 7
  s1 <- iba_table %>% filter(Site_code == s)
  s2 <- s1 %>% group_by(Species) %>% 
    summarise(Site_code = min(Site_code), # a fudge because we know there is only one value for the site code
              Min_count = min(Minimum), 
              Max_count = max(Maximum))
  
  s3 <- site_table %>% filter(Site_code == s) %>% dplyr::select(-Species, -MPA_Zones)
  s4 <- s3 %>% slice(rep(1:n(), each = length(unique(s1$Species))))
  
  t1 <- cols_table %>% filter(Site_code == s) %>% group_by(Species) %>% 
    summarise(Unique_cols = n_distinct(Colony))
  
  s5 <- cbind(s2,s4, t1)
  IBAsummary <- rbind(IBAsummary,s5)

}

head(IBAsummary,10)
dim(IBAsummary)

write.csv(IBAsummary, paste("./Data/Antarctica_Penguin_mIBAs/","63mIBA_Summary_June2020.csv",sep=""),row.names=F)
