# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 18-01-2021
# Date last modified: 10-03-2023
# Author: Simeon Q. Smeele
# Description: Creates GBI from social data. 
# This version also works for the 2021 data structure. 
# This version switches to TRUE/FALSE instead of 1/0.
# This version adds the date and location to the GBI. 
# This version actually works for the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

create.gbi = function(dat, data_type = 2020){
  
  # Get the 'normal' groups out of there
  if(data_type == 2020){
    rownames(dat) = 1:nrow(dat)
    keep = dat[c('recording', 'date', 'time_start')] %>% unique %>% rownames %>% as.numeric
    keep = c(keep, which(dat$new_flock == 1)) %>% unique %>% sort
    group_dat = dat[keep,]
    
    # Remove nests for now
    group_dat = group_dat[group_dat$location %in% c('ground', 'tree', 'd tree', 'palm', 'pine', 'bush'),]
  } else group_dat = dat
  
  # Get all individuals and make a matrix with those as columns
  unique_ids = group_dat$group %>% na.omit %>% str_split(', ') %>% unlist %>% 
    str_split(',') %>% unlist %>% str_split(' ') %>% unlist %>% toupper %>% unique
  # print(unique_ids[str_detect(unique_ids, 'X')])
  unique_ids[unique_ids == 'COLLAR'] = 'STUMP'
  unique_ids = unique_ids[!unique_ids %in% '']
  gbi = matrix(FALSE, nrow = nrow(group_dat), ncol = length(unique_ids) + 2) %>% as.data.frame
  colnames(gbi) = c('date', 'cluster', unique_ids)
  
  # Include date and location for 2021 data
  if(data_type == 2021){
    # Fix the date column
    group_dat$date = ifelse(is.na(group_dat$date), 
                            group_dat$recording %>% 
                              str_sub(1,10) %>% 
                              str_replace_all('_', '-'),
                            group_dat$date)
    # Assign locations
    remove = which(is.na(group_dat$utm))
    message(sprintf('Removing %s missing UTMs.', length(remove)))
    group_dat = group_dat[-remove,]
    UTM = lapply(group_dat$utm, function(x) str_split(x, ' ')[[1]])
    UTM_1 = sapply(UTM, function(x) x[1]) %>% as.numeric
    UTM_2 = sapply(UTM, function(x) x[2]) %>% as.numeric
    clusters = kmeans(cbind(UTM_1, UTM_2), 3, nstart = 5, iter.max = 1000)
    plot(UTM_1, UTM_2, col = clusters$cluster)
  }
  
  # Fill it out
  for(i in 1:nrow(group_dat)){
    id_present = group_dat$group[i] %>% na.omit %>% str_split(', ') %>% unlist %>% 
      str_split(',') %>% unlist %>% str_split(' ') %>% unlist %>% toupper %>% unique
    id_present = id_present[id_present != '']
    if('COLLAR' %in% id_present) id_present[id_present == 'COLLAR'] = 'STUMP'
    for(id in id_present) gbi[i, id] = TRUE
    gbi[i, 'date'] = group_dat$date[i]
    # gbi[i, 'cluster'] = clusters$cluster[i]
    # gbi[i, 'utm'] = paste(UTM_1[i], UTM_2[i])
    gbi[i, 'cluster'] = NA
  }
  
  # Remove problem columns with X, UN, STUMP, UK
  gbi = gbi[,!colnames(gbi) %in% c('X', 'UN', 'UK', 'STUMP')]
  gbi = gbi[,!str_detect(colnames(gbi), 'X')]
  
  # Return
  return(gbi)
  
}
