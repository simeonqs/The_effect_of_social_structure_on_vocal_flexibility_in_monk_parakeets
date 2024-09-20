# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 03-10-2022
# Date last modified: 03-10-2022
# Author: Simeon Q. Smeele
# Description: Loads the overview of the recordings and adds the missing UTMs.
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(readxl)

load.overview.recordings.21 = function(path_overview_recordings_21,
                                       path_nest_points_2021){
  
  # Load
  overview_recordings = read_xlsx(path_overview_recordings_21)
  overview_recordings = overview_recordings[-which(overview_recordings$discard == 1 & 
                                                     is.na(overview_recordings$nest)),]
  overview_recordings = overview_recordings[!str_detect(overview_recordings$file, '2021_10_26'),]
  overview_recordings = overview_recordings[!str_detect(overview_recordings$file, '2021_10_27'),]

  # Add from nest points
  UTM = load.nest.points(path_nest_points_2021)
  overview_recordings$utm[is.na(overview_recordings$utm)] = 
    sapply(which(is.na(overview_recordings$utm)), function(i){
      y = overview_recordings$nest[i]
      return(ifelse(is.na(y), NA, paste(UTM[as.character(y),], collapse = ' ')))
    })
  
  # Return
  return(overview_recordings)
  
}
