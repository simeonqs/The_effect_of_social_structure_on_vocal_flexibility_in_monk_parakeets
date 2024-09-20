# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 16-08-2022
# Date last modified: 16-01-2024
# Author: Simeon Q. Smeele
# Description: This script compiles a network based on network location. 
# This version includes the 2020 data. 
# This version removes plotKML functions. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'readxl', 'geosphere', 'sf', 'callsync')
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

# Load data 
nest_per_ind_20 = read.csv2(path_nest_per_ind_20, na.string = '')
nest_per_ind_20 = nest_per_ind_20[!is.na(nest_per_ind_20$tree_manual),]

# Load utm data
files_20 = list.files(path_nest_points_2020, pattern = '*gpx', full.names = T)
points_20 = lapply(files_20, st_read, layer = 'waypoints', quiet = TRUE)
nesting_points_20 = data.frame(nest = files_20 %>% basename %>% str_remove('.gpx'))
nesting_points_20$long = sapply(points_20, function(point) as.numeric(point$geometry[[1]][1]))
nesting_points_20$lat = sapply(points_20, function(point) as.numeric(point$geometry[[1]][2]))

# Create distance matrix for inds
inds_20 = nest_per_ind_20$id
l_20 = length(inds_20)
c_20 = combn(1:l_20, 2)
out_20 = sapply(1:ncol(c_20), function(x) {
  i = which(nesting_points_20$nest == nest_per_ind_20$tree_manual[nest_per_ind_20$id == inds_20[c_20[1,x]]])
  j = which(nesting_points_20$nest == nest_per_ind_20$tree_manual[nest_per_ind_20$id == inds_20[c_20[2,x]]])
  return( distm(nesting_points_20[i,c('long', 'lat')], 
                nesting_points_20[j,c('long', 'lat')], 
                fun = distHaversine) )
})

# Make it into a matrix
m_20 = o.to.m(out_20, inds_20)

##2021##

# Load data 
nest_per_ind_21 = read.csv2(path_nest_per_ind_21, na.string = '')
nest_per_ind_21 = nest_per_ind_21[!is.na(nest_per_ind_21$tree_manual),]

# Load utm data
files_21 = list.files(path_nest_points_2021, pattern = '*gpx', full.names = T)
points_21 = lapply(files_21, st_read, layer = 'waypoints', quiet = TRUE)
nesting_points_21 = data.frame(nest = files_21 %>% basename %>% str_remove('.gpx') %>% 
  str_remove('Export of ') %>% str_remove('Exportación de '))
nesting_points_21$long = sapply(points_21, function(point) as.numeric(point$geometry[[1]][1]))
nesting_points_21$lat = sapply(points_21, function(point) as.numeric(point$geometry[[1]][2]))

# Create distance matrix for inds
inds_21 = nest_per_ind_21$id
l_21 = length(inds_21)
c_21 = combn(1:l_21, 2)
out_21 = sapply(seq_len(ncol(c_21)), function(x) {
  i = which(nesting_points_21$nest == nest_per_ind_21$tree_manual[nest_per_ind_21$id == inds_21[c_21[1,x]]])
  j = which(nesting_points_21$nest == nest_per_ind_21$tree_manual[nest_per_ind_21$id == inds_21[c_21[2,x]]])
  return( distm(nesting_points_21[i,c('long', 'lat')], 
                nesting_points_21[j,c('long', 'lat')], 
                fun = distHaversine) )
})

# Make it into a matrix
m_21 = o.to.m(out_21, inds_21)

## Save network ##
spatial_network_20 = m_20
spatial_network_21 = m_21
save(spatial_network_20, spatial_network_21, file = path_spatial_network)

# Plot
sf = st_as_sf(nesting_points_20, coords = c('long', 'lat'))
ggplot(sf) + 
  geom_sf()
