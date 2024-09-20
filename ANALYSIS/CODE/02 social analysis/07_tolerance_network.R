# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 25-02-2023
# Date last modified: 05-04-2023
# Author: Simeon Q. Smeele
# Description: Compiling the close tolerance (10 cm) network. 
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'sna')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(lib, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 
set.seed(1)

# Paths
source('ANALYSIS/CODE/paths.R')

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

## 2020 ##

# Load data
social_data_20 = load.social.data.20(path_social_data_simeon_20,
                                     path_social_data_andres_20,
                                     path_social_data_mireia_20)

# Remove unknown individuals
social_data_20 = social_data_20[!is.na(social_data_20$to) & !is.na(social_data_20$from),]
social_data_20 = social_data_20[-which(str_detect(social_data_20$from, ',') |
                                         str_detect(social_data_20$from, 'UN') |
                                         str_detect(social_data_20$from, 'UK') |
                                         str_detect(social_data_20$from, 'un') |
                                         str_detect(social_data_20$from, 'STUMP') |
                                         str_detect(social_data_20$from, 'stump') |
                                         str_detect(social_data_20$from, 'X') |
                                         str_detect(social_data_20$from, '/') |
                                         str_detect(social_data_20$from, 'collar') |
                                         str_detect(social_data_20$to, ',') |
                                         str_detect(social_data_20$to, 'UN') |
                                         str_detect(social_data_20$to, 'UK') |
                                         str_detect(social_data_20$to, 'un') |
                                         str_detect(social_data_20$to, 'STUMP') |
                                         str_detect(social_data_20$to, 'stump') |
                                         str_detect(social_data_20$to, 'X') |
                                         str_detect(social_data_20$to, '/') |
                                         str_detect(social_data_20$to, 'collar')),]

# Fix which column for 2020
social_data_20$which = ifelse(social_data_20$which == '10 cm', 'close', 
                              ifelse(social_data_20$which == 'preening', 'preen', 
                                     ifelse(social_data_20$which == 'touching', 'touch', 
                                            social_data_20$which)))

# Include touching and preening as close tolerance
for(i in which(social_data_20$which %in% c('preen', 'touch'))){
  # subset for same recording session
  if(!is.na(social_data_20$recording[i])){
    sub = social_data_20[which(social_data_20$recording == social_data_20$recording[i]),]
  } else {
    sub = social_data_20[which(paste(social_data_20$date, social_data_20$time_start) == 
                                 paste(social_data_20$date[i], social_data_20$time_start[i])),]
  }
  # organise dyads
  dyads = vapply(seq_len(nrow(sub)), function(j) paste(sort(c(sub$from[j], sub$to[j])),
                                                       collapse = '-'),
                 character(1))
  sub = sub[which(dyads == paste(sort(c(social_data_20$from[i], social_data_20$to[i])), collapse = '-')),]
  # add close if missing
  if(!'close' %in% sub$which) 
    social_data_20 = bind_rows(social_data_20, 
                               data.frame(date = social_data_20$date[i],
                                          time_start = social_data_20$time_start[i],
                                          recording = social_data_20$recording[i],
                                          from = social_data_20$from[i],
                                          to = social_data_20$to[i],
                                          which = 'close'))
}

# Subset for aggressive interactions
tol_dat = social_data_20[which(social_data_20$which == 'close'),]

# Create network
inds = unique(c(tol_dat$from, tol_dat$to))
l = length(inds)
network = matrix(NA, nrow = l, ncol = l)
rownames(network) = colnames(network) = inds
for(i in seq_len(l))
  for(j in seq_len(l)) if(i != j) 
    network[i, j] = length(which((tol_dat$from == inds[i] & tol_dat$to == inds[j])|
                                   tol_dat$from == inds[j] & tol_dat$to == inds[i]))
tolerance_network_20 = network

## 2021 ##

# Load data
social_data_21 = load.social.data.21(path_social_data_simeon_21, 
                                     path_social_data_andres_21,
                                     path_overview_recordings_21)

# Remove unknown individuals
social_data_21 = social_data_21[-which(str_detect(social_data_21$from, ',') |
                                         str_detect(social_data_21$from, 'UN') |
                                         str_detect(social_data_21$from, 'X') |
                                         str_detect(social_data_21$from, '/') |
                                         str_detect(social_data_21$to, ',') |
                                         str_detect(social_data_21$to, 'UN') |
                                         str_detect(social_data_21$to, 'X') |
                                         str_detect(social_data_21$to, '/')),]

# Include touching and preening as close tolerance
for(i in which(social_data_21$which %in% c('preen', 'touch'))){
  # subset for same recording session
  if(!is.na(social_data_21$recording[i])){
    sub = social_data_21[which(social_data_21$recording == social_data_21$recording[i]),]
  } else {
    sub = social_data_21[which(paste(social_data_21$date, social_data_21$time_start) == 
                                 paste(social_data_21$date[i], social_data_21$time_start[i])),]
  }
  # organise dyads
  dyads = vapply(seq_len(nrow(sub)), function(j) paste(sort(c(sub$from[j], sub$to[j])),
                                                       collapse = '-'),
                 character(1))
  sub = sub[which(dyads == paste(sort(c(social_data_21$from[i], social_data_21$to[i])), collapse = '-')),]
  # add close if missing
  if(!'close' %in% sub$which) 
    social_data_21 = bind_rows(social_data_21, 
                               data.frame(date = social_data_21$date[i],
                                          time_start = social_data_21$time_start[i],
                                          recording = social_data_21$recording[i],
                                          from = social_data_21$from[i],
                                          to = social_data_21$to[i],
                                          which = 'close'))
}

# Subset for aggressive interactions
tol_dat = social_data_21[which(social_data_21$which == 'close'),]

# Create network
inds = unique(c(tol_dat$from, tol_dat$to))
l = length(inds)
network = matrix(NA, nrow = l, ncol = l)
rownames(network) = colnames(network) = inds
for(i in seq_len(l))
  for(j in seq_len(l)) if(i != j) 
    network[i, j] = length(which((tol_dat$from == inds[i] & tol_dat$to == inds[j])|
                                   tol_dat$from == inds[j] & tol_dat$to == inds[i]))
tolerance_network_21 = network

## Save ##
save(tolerance_network_20, tolerance_network_21, file = path_tolerance_network)