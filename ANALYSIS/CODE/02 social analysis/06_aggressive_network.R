# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 25-02-2023
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: Compiling the aggressive network. 
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

# Subset for aggressive interactions
ag_dat = social_data_20[which(social_data_20$which == 'displacement'),]

# Remove unknown individuals
ag_dat = ag_dat[-which(str_detect(ag_dat$from, ',') |
                         str_detect(ag_dat$from, 'UN') |
                         str_detect(ag_dat$from, 'UK') |
                         str_detect(ag_dat$from, 'un') |
                         str_detect(ag_dat$from, 'STUMP') |
                         str_detect(ag_dat$from, 'stump') |
                         str_detect(ag_dat$from, 'X') |
                         str_detect(ag_dat$from, '/') |
                         str_detect(ag_dat$to, ',') |
                         str_detect(ag_dat$to, 'UN') |
                         str_detect(ag_dat$to, 'UK') |
                         str_detect(ag_dat$to, 'un') |
                         str_detect(ag_dat$to, 'STUMP') |
                         str_detect(ag_dat$to, 'stump') |
                         str_detect(ag_dat$to, 'X') |
                         str_detect(ag_dat$to, '/')),]

# Swap from and two if winner is not from
for(i in seq_len(nrow(ag_dat))) if(!is.na(ag_dat$winner[i])) if(ag_dat$to[i] == ag_dat$winner[i]){
  fr = ag_dat$from[i]
  to = ag_dat$to[i]
  ag_dat$from[i] = to
  ag_dat$to[i] = fr
} 

# Create network
inds = unique(c(ag_dat$from, ag_dat$to))
l = length(inds)
network = matrix(NA, nrow = l, ncol = l)
rownames(network) = colnames(network) = inds
for(i in seq_len(l))
  for(j in seq_len(l)) if(i != j) 
    network[i, j] = length(which(ag_dat$from == inds[i] & ag_dat$to == inds[j]))
aggressive_network_20 = network

## 2021 ##

# Load data
social_data_21 = load.social.data.21(path_social_data_simeon_21, 
                                     path_social_data_andres_21,
                                     path_overview_recordings_21)

# Subset for aggressive interactions
ag_dat = social_data_21[which(social_data_21$which == 'displacement'),]

# Remove unknown individuals
ag_dat = ag_dat[-which(str_detect(ag_dat$from, ',') |
                         str_detect(ag_dat$from, 'UN') |
                         str_detect(ag_dat$from, 'UK') |
                         str_detect(ag_dat$from, 'un') |
                         str_detect(ag_dat$from, 'STUMP') |
                         str_detect(ag_dat$from, 'stump') |
                         str_detect(ag_dat$from, 'X') |
                         str_detect(ag_dat$from, '/') |
                         str_detect(ag_dat$to, ',') |
                         str_detect(ag_dat$to, 'UN') |
                         str_detect(ag_dat$to, 'UK') |
                         str_detect(ag_dat$to, 'un') |
                         str_detect(ag_dat$to, 'STUMP') |
                         str_detect(ag_dat$to, 'stump') |
                         str_detect(ag_dat$to, 'X') |
                         str_detect(ag_dat$to, '/')),]

# Swap from and two if winner is not from
for(i in seq_len(nrow(ag_dat))) if(!is.na(ag_dat$winner[i])) if(ag_dat$to[i] == ag_dat$winner[i]){
  fr = ag_dat$from[i]
  to = ag_dat$to[i]
  ag_dat$from[i] = to
  ag_dat$to[i] = fr
} 

# Create network
inds = unique(c(ag_dat$from, ag_dat$to))
l = length(inds)
network = matrix(NA, nrow = l, ncol = l)
rownames(network) = colnames(network) = inds
for(i in seq_len(l))
  for(j in seq_len(l)) if(i != j) 
    network[i, j] = length(which(ag_dat$from == inds[i] & ag_dat$to == inds[j]))
aggressive_network_21 = network

## Save ##
save(aggressive_network_20, aggressive_network_21, file = path_aggressive_network)
