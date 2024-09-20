# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 05-11-2022
# Date last modified: 16-07-2023
# Author: Simeon Q. Smeele
# Description: Preparing all the data for the similarity model.  
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data
load(path_data)
load(path_dtw_m)
load(path_foraging_network)
load(path_dat_mate_id)
rownames(dat_mate_id_20) = dat_mate_id_20$id
rownames(dat_mate_id_21) = dat_mate_id_21$id
load(path_spatial_network)
load(path_similarity_out)
overview_nesting_20 = read.csv2(path_nest_per_ind_20)
overview_nesting_21 = read.csv2(path_nest_per_ind_21)
load(path_wang_out)
load(path_tolerance_network)
load(path_aggressive_network)

## 2020 ##

# Compile dat
dat = data.frame(dyad = similarity_out_20$char,
                 acc_dist = similarity_out_20$mean_a_ind_pair,
                 acc_dist_se = similarity_out_20$sd_a_ind_pair)
dat$same_mate = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  mate = ifelse(split[1] %in% dat_mate_id_20$id & split[2] %in% dat_mate_id_20$id,
                ifelse(dat_mate_id_20[split[1],]$mate_id == dat_mate_id_20[split[2],]$mate_id,
                       1, 2), # 1 = same mate
                NA)
  return(mate)
})
dat$same_cluster = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  tree_1 = overview_nesting_20$tree_manual[overview_nesting_20$id == split[1]]
  tree_2 = overview_nesting_20$tree_manual[overview_nesting_20$id == split[2]]
  if(length(tree_1) != 0 & length(tree_2) != 0) same_cluster = ifelse(tree_1 == tree_2, 1, 2) else 
    same_cluster = NA
  return(same_cluster)
})
dat$for_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(foraging_network_20) & split[2] %in% rownames(foraging_network_20),
                  foraging_network_20[split[1], split[2]],
                  NA)
  return(weight)
})
dat$spat_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(spatial_network_20) & split[2] %in% rownames(spatial_network_20),
                  spatial_network_20[split[1], split[2]],
                  NA)
  return(weight)
})
# dat$same_location = sapply(dat$dyad, function(dyad){
#   split = strsplit(dyad, ' ')[[1]]
#   area_1 = overview_nesting_20$area[overview_nesting_20$id == split[1]]
#   area_2 = overview_nesting_20$area[overview_nesting_20$id == split[2]]
#   if(length(area_1) != 0 & length(area_2) != 0) same_location = ifelse(area_1 == area_2, 1, 2) else 
#     same_location = NA
#   return(same_location)
# })
dat$gen_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(wang_out) & split[2] %in% rownames(wang_out),
                  wang_out[split[1], split[2]],
                  NA)
  return(weight)
})
dat$tol_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(tolerance_network_20) & split[2] %in% rownames(tolerance_network_20),
                  tolerance_network_20[split[1], split[2]],
                  NA)
  return(weight)
})
dat$ag_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(aggressive_network_20) & 
                    split[2] %in% rownames(aggressive_network_20),
                  aggressive_network_20[split[1], split[2]],
                  NA)
  return(weight)
})

# Optionally make related mates 2
# dat$same_mate[which(dat$same_mate == 1 & dat$gen_edge_weight > 0.2)] = 2

# Plot
plot(dat)
dat_20 = dat

## 2021 ##

# Compile dat
dat = data.frame(dyad = similarity_out_21$char,
                 acc_dist = similarity_out_21$mean_a_ind_pair,
                 acc_dist_se = similarity_out_21$sd_a_ind_pair)
dat$same_mate = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  mate = ifelse(split[1] %in% dat_mate_id_21$id & split[2] %in% dat_mate_id_21$id,
                ifelse(dat_mate_id_21[split[1],]$mate_id == dat_mate_id_21[split[2],]$mate_id,
                       1, 2), # 1 = same mate
                NA)
  return(mate)
})
dat$same_cluster = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  tree_1 = overview_nesting_21$tree_manual[overview_nesting_21$id == split[1]]
  tree_2 = overview_nesting_21$tree_manual[overview_nesting_21$id == split[2]]
  if(length(tree_1) != 0 & length(tree_2) != 0) same_cluster = ifelse(tree_1 == tree_2, 1, 2) else 
    same_cluster = NA
  return(same_cluster)
})
dat$for_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(foraging_network_21) & split[2] %in% rownames(foraging_network_21),
                  foraging_network_21[split[1], split[2]],
                  NA)
  return(weight)
})
dat$spat_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(spatial_network_21) & split[2] %in% rownames(spatial_network_21),
                  spatial_network_21[split[1], split[2]],
                  NA)
  return(weight)
})
# dat$same_location = sapply(dat$dyad, function(dyad){
#   split = strsplit(dyad, ' ')[[1]]
#   area_1 = overview_nesting_21$area[overview_nesting_21$id == split[1]]
#   area_2 = overview_nesting_21$area[overview_nesting_21$id == split[2]]
#   if(length(area_1) != 0 & length(area_2) != 0) same_location = ifelse(area_1 == area_2, 1, 2) else 
#     same_location = NA
#   return(same_location)
# })
dat$gen_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(wang_out) & split[2] %in% rownames(wang_out),
                  wang_out[split[1], split[2]],
                  NA)
  return(weight)
})
dat$tol_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(tolerance_network_21) & split[2] %in% rownames(tolerance_network_21),
                  tolerance_network_21[split[1], split[2]],
                  NA)
  return(weight)
})
dat$ag_edge_weight = sapply(dat$dyad, function(dyad){
  split = strsplit(dyad, ' ')[[1]]
  weight = ifelse(split[1] %in% rownames(aggressive_network_21) & 
                    split[2] %in% rownames(aggressive_network_21),
                  aggressive_network_21[split[1], split[2]],
                  NA)
  return(weight)
})

# Optionally make related mates 2
# dat$same_mate[which(dat$same_mate == 1 & dat$gen_edge_weight > 0.2)] = 2

# Plot
plot(dat)
dat_21 = dat

## Save ##
save(dat_20, dat_21, file = path_dat_similarity)
