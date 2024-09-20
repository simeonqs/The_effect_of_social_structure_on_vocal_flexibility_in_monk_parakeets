# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 03-10-2022
# Date last modified: 03-10-2022
# Author: Simeon Q. Smeele
# Description: Loads the nest point UTMs.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(readxl)
require(gpx)
require(rgdal)

load.nest.points = function(path_nest_points){
  
  # List files and load
  files = list.files(path_nest_points_2021, '.gpx', full.names = T)
  points = lapply(files, read_gpx)
  points_flat = lapply(points, function(point) point$waypoints[c('Latitude', 'Longitude')]) %>% bind_rows
  
  # Convert points
  cord_dec = SpatialPoints(cbind(points_flat$Longitude, -points_flat$Latitude), 
                           proj4string = CRS('+proj=longlat'))
  UTM = spTransform(cord_dec, CRS("+init=epsg:32631"))@coords %>% round
  UTM[,2] = -UTM[,2]
  
  # Name
  rownames(UTM) = files %>% basename %>% parse_number %>% as.numeric
  
  # Return
  return(UTM)
  
} # end load.nest.points