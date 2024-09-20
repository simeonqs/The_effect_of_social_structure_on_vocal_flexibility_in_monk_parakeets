# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 14-04-2022
# Date last modified: 03-10-2022
# Author: Simeon Q. Smeele
# Description: Loads and cleans the social data from 2021.
# This version adds the UTM coordinates if they are missing. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(readxl)

load.social.data.21 = function(path_social_data_simeon_21, 
                               path_social_data_andres_21,
                               path_overview_recordings_21, 
                               path_nest_points_2021){
  
  # Load
  social_data_simeon = read_xlsx(path_social_data_simeon_21, guess_max = 2000)
  social_data_andres = read_xlsx(path_social_data_andres_21, guess_max = 2000)
  social_data = rbind(social_data_simeon, social_data_andres)
  overview_recordings = load.overview.recordings.21(path_overview_recordings_21, path_nest_points_2021)
  nest_points = load.nest.points(path_nest_points_2021)
  
  # Clean
  ## translate which
  social_data$which = ifelse(social_data$which %in% c('preen', 'preening'), 'preen', social_data$which)
  social_data$which = ifelse(social_data$which %in% c('touch', 'Touching', 'touching'), 'touch', 
                             social_data$which)
  social_data$which = ifelse(social_data$which %in% c('beak fight', 'beakfight', 'Beak fight'), 'beak fight', 
                             social_data$which)
  social_data$which = ifelse(social_data$which %in% c('attach', 'fight'), 'fight', social_data$which)
  ## remove which
  social_data = social_data[!social_data$which %in% c('arriving', 'flying', 'threat'),]
  ## translate IDs
  social_data$from = toupper(social_data$from)
  social_data$to = toupper(social_data$to)
  social_data$from = ifelse(social_data$from %in% 'collar', 'stump', social_data$from)
  social_data$to = ifelse(social_data$to %in% 'collar', 'stump', social_data$to)
  
  # Add UTM
  social_data$utm[is.na(social_data$utm) & !is.na(social_data$tree)] = 
    sapply(which(is.na(social_data$utm) & !is.na(social_data$tree)), function(i)
      paste(nest_points[as.character(social_data$tree[i]),], collapse = ' ')) 
  social_data$utm[social_data$utm %in% c('-', 'ns')] = NA
  social_data$utm[is.na(social_data$utm)] = sapply(which(is.na(social_data$utm)), function(i){
    y = overview_recordings$utm[which(overview_recordings$file == social_data$recording[i])]
    return(ifelse(length(y) == 0, NA, y))
  })
  
  # Return
  return(social_data)
  
}