## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Script summary ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## This script brings in example raw tracking data from
"Chinstrap Penguins - data on Seabird Tracking Database"
"Datasets: 761 and 930"

## -	930: Akanori Takahashi (Chinstrap Penguins, King George Island)
## -	761: Phil Trathan (Chinstrap Penguins, Powell Island)


## Jono Handley, jonathan.m.handley@gmail.com / jonathan.handley@birdlife.org

## R version R version 4.2.2 (2022-10-31 ucrt) -- "Bird Hippie"

## Feb 2023

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load libraries ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"Had to install R version: R version 4.2.2 (2022-10-31 ucrt)"
## Options to install aniMotum package for animal track interpolation
#install.packages('aniMotum', repos = c('https://ianjonsen.r-universe.dev', 'https://cloud.r-project.org'))
#install.packages('TMB', type = 'source')
library("aniMotum")
## sf package for spatial data analyses (i.e. vector files such as points, lines, polygons)
library(sf)
## Tidyverse for data manipulation
library(tidyverse)
## ggplot2 for plotting opionts
library(ggplot2)
## rnaturalearth package for basemaps in R
library(rnaturalearth)

## leaflet package for interactive maps in R
#install.packages("leaflet")
library(leaflet)
##
library(purrr)
library(furrr)
## for time and dates
library(lubridate)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in global map layer for general plotting purposes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Source a world map from the rnaturalearth R package
## see details of function to change the type of map you get
worldmap <- rnaturalearth::ne_download(scale = "large",
                                       type = "countries",
                                       category = "cultural",
                                       destdir = tempdir(),
                                       load = TRUE,
                                       returnclass = "sf")
plot(st_geometry(worldmap))
st_crs(worldmap)

st_write(worldmap,
         "C:\\Users\\jonathan.handley\\OneDrive - BirdLife International\\JonoHandley_BirdLife\\Data_GlobalLayers\\World_Map_NaturalEarth\\WorldMap_NaturalEarth_ScaleLarge.shp",
         delete_layer = T)


## Projections for Antarctica and North Pole
"If you have polar data, you may need to change the projection of your base map
for better plotting of raw tracking data."

## change projection of your basemap if needed
worldmap_south <- st_transform(worldmap, crs=st_crs("CONSIDERING APPROPRIATE DETAILS FOR EPSG CODES:
                                                    See: https://www.esri.com/arcgis-blog/products/arcgis-pro/mapping/gcs_vs_pcs/#:~:text=What%20is%20the%20difference%20between,map%20or%20a%20computer%20screen.
                                                    See: https://epsg.io/"))
plot(st_geometry(worldmap_south))
st_crs(worldmap_south)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Specify projections / store needed CRS definitions as variables ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## World WGS84
wgs84 <- st_crs("EPSG:4326")
## Croatia specific
htrs96 <- st_crs("EPSG:3765")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load in raw tracking data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"NOTE: The process of loading in raw tracking data will vary by device type and
individual user preferences for storing data. FOR THE TOOLKIT: We can ony provide
general guidance about storing data in preparation for analysis."

## Reading in tracking data stored in seabird tracking database format

d1 <- read_csv("data-testing/tracking-data/Dataset_761_2019-05-20.csv")
head(data.frame(d1),2)

"NOTE: These raw tracks have not had trips split. Nor have they had data near the 
colony filtered. It will be good to do this before applying the interpolation
methods available via the aniMotum R package.

CONSIDER: One option might be to use the aniMotum R package as a standalone speed 
filter of the data, then one could run the script on the somewhat pre-filtered data
i.e. erroneous locations removed for speed and erroneous locations removed from the
colony / source of tracking data start."


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get raw tracking data into similar format as Seabird Tracking Database ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Consider if you need to format data accordingly

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## View the raw tracks for a unique animal ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NOTE: Should consider whether your data has been split into trips from individual
## animals yet if you are are potentially dealing with multiple trips from central
## place foraging species
length(unique(d1$bird_id))
length(unique(d1$track_id))

## Specify a unique number (length is total number of unique birds tracked)
i = 34

## subset the data from a unique bird
bird_track <- d1 %>% dplyr::filter(bird_id == unique(d1$bird_id)[i])
## add a column indicating start and end of tracks
bird_track <- bird_track %>% mutate(nlocs = 1:nrow(bird_track)) %>% 
  mutate(track_segment = if_else(nlocs <= 10, "track.start","track.journey")) %>% 
  ## note: if you have a track with less than 20 points, then you will overwrite 
  ## some of the previous data.
  mutate(track_segment = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"track.end",track_segment)) %>%
  ## add a column indicating colour for start and end of tracks
  ## colours from: https://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
  mutate(track_colour = if_else(nlocs <= 10, "#66c2a5","#8da0cb")) %>% 
  mutate(track_colour = if_else(nlocs %in% (nrow(bird_track)-9):(nrow(bird_track)),"#fc8d62",track_colour))


head(data.frame(bird_track),12)
tail(data.frame(bird_track),12)

## plot the tracks using leaflet package in R.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## label by nloc (location) number. Colour accordingly.
  addCircleMarkers(data = bird_track,
                   label = bird_track$nlocs, radius = 3,
                   fillColor = bird_track$track_colour,
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = bird_track$longitude,
               lat = bird_track$latitude, weight = 1,
               color = "white") 
map


## plot with legend
map %>% 
  addLegend(colors = unique(bird_track$track_colour),
            labels = unique(bird_track$track_segment))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Check sampling interval ----
## To implement track2KBA fully, you need data approximating an even sampling interval
## i.e. location points must be regularly spaced in time.
## Determine how "gappy" the tracking data is (time intervals between location data)
## This is an important step for almost all tracking data analyses.
## If your data is not filtered / cleaned correctly, results may be spurious.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## data for summarising
head(data.frame(bird_track),2)
summary(bird_track)

## Ensure date and time are merged as single column
bird_track <- bird_track %>% 
  mutate(dttm = paste(date_gmt, time_gmt)) %>% 
  mutate(dttm = as.POSIXct(dttm))


## Determine difference between consecutive timestamps 
## (NB: consecutive order of timestamps is critical here!)
"Consider appropriate grouping argument"
## Doing this by tripID, not individual ID - change the group_by argument if needed
timeDiff <- bird_track %>% 
  data.frame() %>% 
  group_by(bird_id) %>% 
  arrange(dttm) %>% 
  ## create new column called delta_secs: calculated by working out the difference in time between
  ## consecutive point locations, starting from the second point. I.e. time difference between
  ## point 2 and point 1, then point 3 and point 2, etc. So if there was a big time gap when an
  ## animal left a point (i.e. a burrow) and the next location (i.e. first new fix at sea), then
  ## the location that would be identified as having the big time gap would be the latter. So,
  ## it would be the location information between this latter point and the former, that might be
  ## speculative.
  mutate(delta_secs = as.numeric(difftime(dttm, lag(dttm, default = first(dttm)), units = "secs"))) %>% 
  slice(2:n()) 

## plot histogram of timediff between all points
"This plot will take time depending on size of dataset!"
p4 <- ggplot(timeDiff , aes(delta_secs)) +
  geom_histogram(colour = "darkgrey", fill = "cyan", binwidth = 200)+
  theme(
    axis.text=element_text(size=14, color="black"),
    axis.title=element_text(size=16),
    panel.background=element_rect(fill="white", colour="black")) +
  ylab("n locations") + xlab("Time diff between locations (secs)")

p4

## Summarise results by tripID
SummaryTimeDiff <- timeDiff %>% 
  group_by(bird_id) %>% 
  summarise(avg_timegap_secs = mean(delta_secs), 
            min_timegap_secs = min(delta_secs),
            max_timegap_secs = max(delta_secs)) %>% data.frame()

## View results
SummaryTimeDiff
head(SummaryTimeDiff)

## Sort data by maximum time gap first - then view.
## Consider if you have any outlier trips with massively different time gaps.
SummaryTimeDiff %>% arrange(-max_timegap_secs) %>% head()


## Print warning for users to consider
warning("Consider whether the sampling interval of your tracking data is appropriate
        for formally running the track2KBA functions. Remember, the time differences
        between each of your location points should be equal (or close enough to equal) 
        across all location points and individuals tracked. If the time difference
        between location points is not equal, the outputs you generate from track2KBA
        will not be valid because the underlying kernel density analysis implemented
        within the track2KBA functions will be invalid.")

timeDiff_summary <- data.frame(timeDiff) %>% dplyr::select(nlocs, delta_secs)

## add data about time differences onto original info about bird_track
## select top 10 of big gaps if deemed to be big gaps
bird_track_gaps <- left_join(bird_track, timeDiff_summary, by = "nlocs", keep = F) %>% 
  arrange(-delta_secs) %>% 
  slice(1:10)


head(data.frame(bird_track_gaps),42)
map %>% addCircleMarkers(data = bird_track_gaps,
                         label = bird_track_gaps$nlocs, radius = 5,
                         fillColor = "red",
                         fillOpacity = 0.5, stroke = F)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save as shapefile for viewing ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Review data 
head(data.frame(d1),2)
unique.birds <- unique(d1$bird_id)

## create sf spatial object
d1_sf <- d1 %>% st_as_sf(coords = c("longitude", "latitude"), crs = wgs84)

#st_write(laraud, "data-output/laraud_raw_tracking.gpkg") ## Can take ages to save the large file!

## seems easier to save outputs for individual birds given the files are so large! 
for(i in unique.birds){
  temp <- laraud %>% dplyr::filter(bird_id == i)
  temp2 <- temp %>% dplyr::select(device_id,
                                  UTC_datetime,
                                  name, comment, deployed_from, deployed_until, 
                                  deployment_loc, pair_id,  sex, species, bird_id)
  
  st_write(temp2, paste("data-output/laraud/raw-tracking/laraud_raw_tracking_",i,".gpkg", sep=""), delete_layer = T)
  
  print(i)
}

"View and assess quality of individual tracks based on outputs above"



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get example of track from single bird ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"Now done above."

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Process data with aniMotum, GPS data ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

head(data.frame(bird_track))

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 1: Format the data"
"~~~~~~~~~~~~~~~~~~~~~~~"

## format the data into format required for aniMotum
## NOTE: The format varies for Argos, GPS and GLS data - format accordingly
bird_track_am <- bird_track %>% mutate(lc = "G") %>% 
  dplyr::select(id = "bird_id",
                date = "dttm",
                lc,
                lon = "longitude",
                lat = "latitude")

## review the newly formated data
head(bird_track_am,20)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 2: Fit the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

"When fitting the model, there are some useful parameters to consider"
"CONSIDER SPEED - vmax - depending on the species"
## fit the state-space model
## SEE the help file: ?fit_ssm, to understand some of the arguments within the function
## NOTE: the function can do 3 things simultaneously: data formatting step, a pre-filtering step, and the actual model fitting
## INPUT: data.frame, tibble or sf-tibble of observations, depending on the tracking data type
fit <- fit_ssm(bird_track_am,
               ## specify what kind of model you want to fit. See details about different model types in paper.
               model = "crw",
               ## specify the speed at which data points could be considered outlier points (in m/s)
               vmax = 5,
               ## time.step in hours - specify time.step of new values to be predicted (interpolation)
               time.step = 0.25)

"NOTE: Depending on how you prefilter your data before running fit_ssm, you may
want to consider changing some of the function parameters. E.g. you might indicate
fit.to.subset = F, if you have filtered your date already adn are sure all your 
locations are true locations."


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 3: Review the model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

## review the model summary
## See: https://ianjonsen.github.io/aniMotum/articles/Overview.html
"Check that converged and phHess were True. NOTE: I'm not sure what it means if they are false"
fit
"Review overall summaries and SSM details for each individual. Again, not entirely sure what all the important bits are"
summary(fit)


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 4: Review the different tabular ouputs after fitting the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

## data.frame of SSM fitted values (location estimates corresponding to the observation times)
floc.fitted <- grab(fit, what = "fitted")

## data.frame of predicted values (corresponding to locations predicted at regular time.step intervals)
floc.predicted <- grab(fit, what = "predicted")

## data.frame of original data with a column indicating which locations to keep or not
floc.data <- grab(fit, what = "data")

## review the new data frames you get and your original data
head(data.frame(floc.fitted),2)
head(data.frame(floc.predicted),2)
head(data.frame(floc.data),2)
head(data.frame(bird_track),2)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 5: Plot the different tabular ouputs after fitting the model"
"~~~~~~~~~~~~~~~~~~~~~~~"

## plot the FITTED values over original data (i.e. locations used for fitting the model)
map %>% addCircleMarkers(data = floc.fitted,
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "lightgreen",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "lightgreen",
            labels = "fitted values")

## plot the PREDICTED values over original data (i.e. locations predcited from the model)
map %>% addCircleMarkers(data = floc.predicted,
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "cyan",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "cyan",
            labels = "predicted values")


## plot the REMOVED values over original data (i.e. locations that were removed from the prefiltering step)
map %>% addCircleMarkers(data = subset(floc.data, floc.data$keep == F),
                         #label = bird_track_gaps$nlocs, 
                         radius = 3,
                         fillColor = "red",
                         fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = "red",
            labels = "removed values")



"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 6: Visualising a model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

# plot time-series of the fitted values
plot(fit, what = "fitted", type = 1, pages = 1)

# plot time-series of the predcited values
plot(fit, what = "predicted", type = 1, pages = 1)

# plot fitted values as a 2-d track
plot(fit, what = "predicted", type = 2, pages = 1,
     ## 95 % confidence ellipses (orange-filled ellipses) around the predicted 
     ## values are also displayed, but can be faded away by choosing a low alpha value
     alpha = 0.05,
     ## Observations that failed the prefilter stage are displayed (black x’s) 
     ## by default but can be turned off with the argument outlier = FALSE)
     outlier = T)

# plot fitted values as a 2-d track
plot(fit, what = "predicted", type = 2, pages = 1,
     ## 95 % confidence ellipses (orange-filled ellipses) around the predicted 
     ## values are also displayed, but can be faded away by choosing a low alpha value
     alpha = 0.00,
     ## Observations that failed the prefilter stage are displayed (black x’s) 
     ## by default but can be turned off with the argument outlier = FALSE)
     outlier = T)

"CONSIDER: How can we help user to decide whether their data is of high enough
quality or not for a track2KBA styled analysis? Perhaps the outputs from 
grab(fit, what = predicted) can be of help? Here, see an indication of standard 
errors around predicted locations via (x.se, y.se in km)"

plot(floc.predicted$x.se)
plot(floc.predicted$y.se)

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 7: Further assessment of model fit"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://ianjonsen.github.io/aniMotum/articles/SSM_validation.html

"Does this assessment take into account all tracks simultanesouly? Or does it only
assess each track individually? What are the implications for this assessment in
the context of track2KBA? Not sure..."

# use patchwork package to arrange plot.osar options
library(patchwork)
# calculate & plot residuals
"NOTE: Computationally intensive! Takes time!!"
res.rw <- osar(fit)

(plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer())


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 8: Assess potential behaviours along track: Move_persistence_models"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://ianjonsen.github.io/aniMotum/articles/Move_persistence_models.html

## NOTE: You can fit this model in two ways

##
"NOTE: Speed and time setting again!"
fmp <- fit_ssm(bird_track_am,
               ## specify what kind of model you want to fit. See details about different model types in paper.
               model = "mp",
               ## specify the speed at which data points could be considered outlier points (in m/s)
               vmax = 5,
               ## time.step in hours - specify time.step of new values to be predicted (interpolation)
               time.step = 0.25,
               control = ssm_control(verbose = 0))

plot(fmp, what = "predicted", type = 3, normalise = T)

"CAN'T SEEM TO GET Move_persistence_models map plotting to work for now."
map(fmp, what = "predicted")
map(fit, what = "predicted", map_type = "cartodark", zoom = 4, progress = "none")
map(fit, 
    what = "p", 
    aes = aes_lst(mp_pal = hcl.colors(n=100, "RdBu")))
"CAN'T SEEM TO GET Move_persistence_models map plotting to work for now."

## data.frame of SSM fitted values (location estimates corresponding to the observation times)
fmp.fitted <- grab(fmp, what = "fitted")

## data.frame of predicted values (corresponding to locations predicted at regular time.step intervals)
fmp.predicted <- grab(fmp, what = "predicted")

## data.frame of original data with a column indicating which locations to keep or not
fmp.data <- grab(fmp, what = "data")

## review gamma (g) column
head(data.frame(fmp.fitted),2)
head(data.frame(fmp.predicted),2)
head(data.frame(fmp.data),2)

## review normalised gamma column (as is done in the plot function above)
fmp.fitted$gnorm <- (fmp.fitted$g-min(fmp.fitted$g))/(max(fmp.fitted$g)-min(fmp.fitted$g))
fmp.predicted$gnorm <- (fmp.predicted$g-min(fmp.predicted$g))/(max(fmp.predicted$g)-min(fmp.predicted$g))

range(fmp.fitted$gnorm)
hist(fmp.fitted$gnorm)
plot(fmp.fitted$gnorm)
"where animals spend more (low, g) or less (high, g) time"


"Plot move persistence model data"
## plot the tracks using leaflet package in R.
## set colour gradient
pal <- colorNumeric(palette = c("green", "red"), domain = fmp.predicted$gnorm)
"Animals spend more time in first colour (relates to low g),
and spend less time in second colour (relates to high g)."


map_mp <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## label by nloc (location) number. Colour accordingly.
  addCircleMarkers(data = fmp.predicted,
                   radius = 3,
                   fillColor = ~pal(fmp.predicted$gnorm),
                   fillOpacity = 0.5, stroke = F) 

map_mp


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 9: Reroute tracks that went overland back via the sea"
"~~~~~~~~~~~~~~~~~~~~~~~"

## NOTE: This will reroute the point locations only! So if you have a very detailed
## coastline, then it may appear the animals still move over land when plotting lines 
## between points. The success of the analysis is also dependent on the underlying
## basemap used. The natural earth map (used by default) is good, but not very finely
## detailed. i.e. resolution could be higher


## install packages
#install.packages("pathroutr", repos = "https://jmlondon.r-universe.dev")
library(pathroutr) # for rerouting tracks
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires) # for higher resolution natural earth map

## reroute the track using the predicted values of the previously fitted model
fit.reroute <- route_path(fit,
                          what = "predicted",
                          map_scale = 10,
                          dist = 10000,
                          append = T)

## data.frame of rerouted values 
## NOTE: Some of these locations may not be ecologically realistic anymore
## i.e. if you were to recalculate travel speeds, they may be unrealistic
## must consider trade-off of approach accordingly
floc.predicted.reroute <- grab(fit.reroute, what = "rerouted")

## review data
head(data.frame(floc.predicted),2)
head(data.frame(floc.predicted.reroute),2)

## plot

"PLOT ---"
## plot the tracks using leaflet package in R.
map <- leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## label by nloc (location) number. Colour accordingly.
  addCircleMarkers(data = bird_track,
                   label = bird_track$nlocs, radius = 3,
                   fillColor = bird_track$track_colour,
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = bird_track$longitude,
               lat = bird_track$latitude, weight = 1,
               color = "white") %>% 
  ## Predicted
  addCircleMarkers(data = floc.predicted,
                   #label = bird_track$nlocs, 
                   radius = 4,
                   fillColor = "green",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = floc.predicted$lon,
               lat = floc.predicted$lat, weight = 1,
               color = "green") %>% 
  ## RE-ROUTED
  addCircleMarkers(data = floc.predicted.reroute,
                   #label = bird_track$nlocs, 
                   radius = 2,
                   fillColor = "red",
                   fillOpacity = 0.5, stroke = F) %>% 
  ## plot lines between points
  addPolylines(lng = floc.predicted.reroute$lon,
               lat = floc.predicted.reroute$lat, weight = 1,
               color = "red")
map


"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 10: Reroute tracks that went overland back via the sea using pathroutr package"
"~~~~~~~~~~~~~~~~~~~~~~~"

## SEE: https://rdrr.io/github/jmlondon/pathroutr/f/vignettes/reroute_demo.Rmd

## Consider the tutorial for pathroutr

## NOTE: This is very computationally expensive when you have many data points
## and high resolution coastline data. Therefore, it may be worth subsetting 
## parts of the track that go over land and trying to reroute these parts only.
## Then you could merge these parts of the track back onto the remainder of the track

"~~~~~~~~~~~~~~~~~~~~~~~"
"STEP 11: Simulate animal tracks"
"~~~~~~~~~~~~~~~~~~~~~~~"

st <- sim_fit(fit, what="predicted", reps=5, 
              ## cpf: is the animal exhibiting central place foraging behaviour?
              cpf=T)

plot(st, zoom=TRUE)
