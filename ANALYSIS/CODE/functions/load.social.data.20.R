# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 18-01-2021
# Date last modified: 02-03-2023
# Author: Simeon Smeele
# Description: Loading the social data. Based on previous script. 
# This version does not load the utms. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(tmaptools)
# require(plotKML)

load.social.data.20 = function(path_social_data_simeon_20,
                               path_social_data_andres_20,
                               path_social_data_mireia_20){

  # Load data Simeon and Andres
  dat_simeon = read.csv2(path_social_data_simeon_20, 
                         na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
  dat_andres = read.csv2(path_social_data_andres_20, 
                         na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
  dat_andres = dat_andres[, ! colnames(dat_andres) %in% c('X', 'X.1')]
  dat_andres$tree = dat_andres$tree %>% as.numeric
  dat_andres$group_size = dat_andres$group_size %>% as.integer
  dat_andres$entry = dat_andres$entry %>% as.integer
  dat = bind_rows(dat_simeon, dat_andres)
  
  # Clean data
  dat = dat[!is.na(dat$observer),]
  
  # Load utms nests
  # load(path_utm_nests)
  
  # Load Mireia data
  dat_mireia = read.csv(path_social_data_mireia_20, 
                        na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
  # utm_mireia = read_GPX('/DATA/waypoints/Mireia/23_des.gpx')[[1]]
  
  # Clean 
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Tidsstempel == '2020/11/09 12:04:29 PM CET'] = 'nest 24'
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Tidsstempel == '2020/11/09 12:08:16 PM CET'] = 'nest 24'
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Tidsstempel == '2020/11/09 12:09:22 PM CET'] = 'nest 25'
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Tidsstempel == '2020/11/09 12:10:00 PM CET'] = 'nest 25'
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Copy.paste.waypoint.name == '96'] = 'nest 83'
  dat_mireia$Who...2.m.or.whole.tree.[
    dat_mireia$Who...2.m.or.whole.tree. == 'K52, J21, P75, I06, un, un, un, un, un, un, un, un, '] = 
    'K52, J21, P75, I06, un, un, un, un, un, un, un, un'
  dat_mireia = dat_mireia[dat_mireia$Tidsstempel != '2020/11/03 8:32:33 PM CET',] # weird case, after dark
  dat_mireia = dat_mireia[dat_mireia$Tidsstempel != '2020/11/09 12:40:15 PM CET',] # waypoint missing
  dat_mireia$Copy.paste.waypoint.name[dat_mireia$Tidsstempel == '2020/11/26 10:26:02 AM CET'] = '59'
  
  # Remove duplicates
  na_replaced = dat_mireia
  na_replaced[is.na(na_replaced)] = 'NA_replace'
  remove = c()
  for(i in 2:nrow(na_replaced)){
    if(all(na_replaced[i,2:12] == na_replaced[i-1,2:12])) remove = c(remove, i)
  }
  dat_mireia = dat_mireia[-remove,]
  
  # Reformat
  new_dat_mireia = data.frame(observer = rep('mireia', nrow(dat_mireia)))
  new_dat_mireia$recording = NA
  new_dat_mireia$new_flock = NA
  new_dat_mireia$what = c(dat_mireia$How.) %>% tolower
  new_dat_mireia$food = NA
  new_dat_mireia$location = c(Ground = 'ground', Deciduoustree = 'd tree',
                              Palmtree = 'palm', Grass = 'ground', 
                              Pinetree = 'pine', Nest = 'nest')[str_remove_all(dat_mireia$Where., ' ')]
  new_dat_mireia$group_size = dat_mireia$Group.size
  new_dat_mireia$group = dat_mireia$Who...2.m.or.whole.tree.
  new_dat_mireia$from = dat_mireia$From
  new_dat_mireia$to = dat_mireia$To
  new_dat_mireia$which = dat_mireia$Type %>% tolower
  new_dat_mireia$winner = dat_mireia$Won.by
  new_dat_mireia$started = dat_mireia$Started.by
  new_dat_mireia$note = NA
  new_dat_mireia$date = dat_mireia$Tidsstempel %>% str_sub(1, 10) %>% str_replace_all('\\/', '_')
  time = dat_mireia$Tidsstempel %>% str_split(' ') %>% sapply(`[`, 2)
  hour = time %>% str_split(':') %>% sapply(`[`, 1) %>% as.numeric
  minute = time %>% str_split(':') %>% sapply(`[`, 2) 
  second = time %>% str_split(':') %>% sapply(`[`, 3) 
  am_pm = dat_mireia$Tidsstempel %>% str_split(' ') %>% sapply(`[`, 3)
  hour = ifelse(am_pm == 'PM' & hour != 12, hour + 12, hour)
  new_dat_mireia$time_start = paste(hour, minute, second, sep = '_')
  new_dat_mireia$tree = NA
  new_dat_mireia$nest = NA
  new_dat_mireia$entry = NA
  new_dat_mireia$utm = NA
  # new_latlong = as.data.frame(t(sapply(utm_mireia$geometry, unlist)))
  # new_latlong = SpatialPoints(new_latlong, CRS("+proj=longlat +datum=WGS84"))
  # new_utm = spTransform(new_latlong, CRS("+proj=utm +zone=31T +datum=WGS84")) %>% as.data.frame
  # new_dat_mireia$easting = sapply(1:nrow(dat_mireia), function(i){
  #   if(str_detect(dat_mireia$Copy.paste.waypoint.name[i], 'nest')){
  #     y = utm_nests$easting[utm_nests$tree == str_remove(dat_mireia$Copy.paste.waypoint.name[i], 'nest ')][1]
  #   } else {
  #     y = new_utm[,1][utm_mireia$name == dat_mireia$Copy.paste.waypoint.name[i]]
  #   }
  #   if(length(y) == 0) y = NA
  #   return(y)
  # })
  # new_dat_mireia$northing = sapply(1:nrow(dat_mireia), function(i){
  #   if(str_detect(dat_mireia$Copy.paste.waypoint.name[i], 'nest')){
  #     y = utm_nests$northing[utm_nests$tree == str_remove(dat_mireia$Copy.paste.waypoint.name[i], 'nest ')][1]
  #   } else {
  #     y = new_utm[,2][utm_mireia$name == dat_mireia$Copy.paste.waypoint.name[i]]
  #   }
  #   if(length(y) == 0) y = NA
  #   return(y)
  # }) %>% unlist
  
  # Bind it to the others
  dat[c('easting', 'northing')] = NA
  new_dat_mireia$northing = NA
  new_dat_mireia$easting = NA
  dat = rbind(dat, new_dat_mireia)

  # More cleaning
  dat$which = ifelse(dat$which %in% c('10cm', '10 cm group', '10', '10 cm '), '10 cm', dat$which)
  dat$which = ifelse(dat$which %in% c('beek fight (when they peck each other during preening)', 
                                      'beek fight', 'beekfight', 'beak fight'), 'beak fight', dat$which)
  dat$which = ifelse(dat$which %in% c('pecking', 'peck'), 'peck', dat$which)
  dat$which = ifelse(dat$which %in% c('fighting', 'fight'), 'fight', dat$which)
  dat$which = ifelse(dat$which %in% c('feeding', 'food gulp', 'feeding display'), 'feeding', dat$which)
  dat$group = dat$group %>%
    str_replace_all('-', 'X') %>%
    str_remove_all('\\?') %>%
    str_replace_all('F', 'E')
  dat$from = dat$from %>%
    str_remove('\\?') %>%
    str_replace_all('F', 'E')
  dat$to = dat$to %>% 
    str_replace_all('-', 'X') %>%
    str_replace_all('stimp', 'STUMP')%>%
    str_remove('\\?') %>%
    str_replace_all('F', 'E')
  dat$location = ifelse(dat$location %in% c('grass', 'ground'), 'ground', dat$location)
  dat$location = ifelse(dat$location %in% c('palm', 'palm tree'), 'palm', dat$location)
  dat$location = ifelse(dat$location %in% c('tree', 'nearby tree'), 'tree', dat$location)
  
  # Filling out partly missing tags
  dat$group = str_replace_all(dat$group, 'L3X', 'L35')
  dat$group = str_replace_all(dat$group, 'R81', 'R18')
  dat$group = str_replace_all(dat$group, 'X71', 'T71')
  
  # Fill out utm from nest and overview recording
  # overview_recordings = load.overview.recordings(wd)
  # for(i in which(is.na(dat$northing))){
  #   if(!is.na(dat$utm[i])){
  #     dat$easting[i] = str_split(dat$utm[i], ' ')[[1]][1] %>% as.numeric
  #     dat$northing[i] = str_split(dat$utm[i], ' ')[[1]][2] %>% as.numeric
  #     next
  #   }
  #   if(!is.na(dat$tree[i])){
  #     dat$easting[i] = utm_nests$easting[utm_nests$tree == dat$tree[i]][1]
  #     dat$northing[i] = utm_nests$northing[utm_nests$tree == dat$tree[i]][1]
  #     next
  #   }
  #   if(!is.na(dat$recording[i])){
  #     dat$easting[i] = overview_recordings$easting[overview_recordings$file == dat$recording[i]]
  #     dat$northing[i] = overview_recordings$northing[overview_recordings$file == dat$recording[i]]
  #   }
  # }
  
  # Return
  return(dat)
  
}