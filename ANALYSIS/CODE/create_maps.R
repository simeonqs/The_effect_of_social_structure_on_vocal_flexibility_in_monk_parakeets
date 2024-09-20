# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 16-01-2023
# Date last modified: 16-01-2024
# Author: Simeon Q. Smeele
# Description: Plots a map with the nests. One for each year. 
# NOTE: saveWidget has a very annoying way of storing files. Change the paths throughout the code to make 
# this work on your system. I tried to figure out relative paths, but failed. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('dplyr', 'leaflet', 'sf', 'stringr', 'maps', 'mapdata', 'webshot', 'htmlwidgets')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

##2020##

# Load utm data
files_20 = list.files(path_nest_points_2020, pattern = '*gpx', full.names = T)
points_20 = lapply(files_20, st_read, layer = 'waypoints', quiet = TRUE)
nesting_points_20 = data.frame(nest = files_20 %>% basename %>% str_remove('.gpx'))
nesting_points_20$long = sapply(points_20, function(point) as.numeric(point$geometry[[1]][1]))
nesting_points_20$lat = sapply(points_20, function(point) as.numeric(point$geometry[[1]][2]))

# Create a leaflet map
map = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  
  fitBounds(
    lng1 = min(nesting_points_20$long),
    lat1 = min(nesting_points_20$lat),
    lng2 = max(nesting_points_20$long),
    lat2 = max(nesting_points_20$lat)
  ) %>%  
  addCircleMarkers(
    lng = nesting_points_20$long,
    lat = nesting_points_20$lat,
    radius = 5,
    opacity = 1,
    fillOpacity = 1,
    color = 'red',
    label = nesting_points_20$nest,
    labelOptions = labelOptions(noHide = TRUE,
                                textsize = '10px',)
  )

# Save the map as a PDF
saveWidget(map, file = '/Users/ssmeele/Desktop/map_2020.html', selfcontained = FALSE)  
webshot::webshot(url = '/Users/ssmeele/Desktop/map_2020.html', file = '/Users/ssmeele/Desktop/map_2020.pdf',
                 vwidth = 1600, vheight = 1200)  

##2021##

# Load utm data
files_21 = list.files(path_nest_points_2021, pattern = '*gpx', full.names = T)
points_21 = lapply(files_21, st_read, layer = 'waypoints', quiet = TRUE)
nesting_points_21 = data.frame(nest = files_21 %>% basename %>% str_remove('.gpx') %>% 
                                 str_remove('Export of ') %>% str_remove('Exportación de '))
nesting_points_21$long = sapply(points_21, function(point) as.numeric(point$geometry[[1]][1]))
nesting_points_21$lat = sapply(points_21, function(point) as.numeric(point$geometry[[1]][2]))

# Create a leaflet map
map = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%  
  fitBounds(
    lng1 = min(nesting_points_21$long),
    lat1 = min(nesting_points_21$lat),
    lng2 = max(nesting_points_21$long),
    lat2 = max(nesting_points_21$lat)
  ) %>%  
  addCircleMarkers(
    lng = nesting_points_21$long,
    lat = nesting_points_21$lat,
    radius = 5,
    opacity = 1,
    fillOpacity = 1,
    color = 'red',
    label = nesting_points_21$nest,
    labelOptions = labelOptions(noHide = TRUE,
                                textsize = '10px',)
  )

# Save the map as a PDF
saveWidget(map, file = '/Users/ssmeele/Desktop/map_2021.html', selfcontained = FALSE)  
webshot::webshot(url = '/Users/ssmeele/Desktop/map_2021.html', file = '/Users/ssmeele/Desktop/map_2021.pdf',
                 vwidth = 1600, vheight = 1200)  
