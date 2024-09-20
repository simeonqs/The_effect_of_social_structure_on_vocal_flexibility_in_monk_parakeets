# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 14-04-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: This script lists all individuals for which we have contact calls. Then it uses co-occurrence  
# in the nest to generate a network of mates. Afterwards is retrieves the mates based on preening. 
# This version also includes the preening data. 
# This version also includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'readxl')
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

# Load data 
load(path_data)
nesting_data_20 = read.csv2(path_nest_per_ind_20, na.string = '')
nesting_data_21 = read.csv2(path_nest_per_ind_21, na.string = '')
social_data_20 = load.social.data.20(path_social_data_simeon_20,
                                     path_social_data_andres_20,
                                     path_social_data_mireia_20)
social_data_21 = load.social.data.21(path_social_data_simeon_21, 
                                     path_social_data_andres_21,
                                     path_overview_recordings_21, 
                                     path_nest_points_2021)

# Get individuals
inds_20 = unique(st_20$bird[st_20$fs %in% data_sets_20$contact])
inds_21 = unique(st_21$bird[st_21$fs %in% data_sets_21$contact])

# Get mate
ind_dat_20 = data.frame(id = inds_20)
ind_dat_21 = data.frame(id = inds_21)
dat_20 = merge(ind_dat_20, nesting_data_20[,c('id', 'nest_manual')], by = 'id', all.x = T, all.y = F)
dat_21 = merge(ind_dat_21, nesting_data_21[,c('id', 'nest_manual')], by = 'id', all.x = T, all.y = F)
dat_20$mate_id_nest = as.integer(as.factor(dat_20$nest_manual))
dat_21$mate_id_nest = as.integer(as.factor(dat_21$nest_manual))

# Isolate preening data
preening_dat_20 = social_data_20[which(social_data_20$which == 'preen'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$from, 'X'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$from, 'UN'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$from, 'UK'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$from, 'STUMP'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$to, 'X'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$to, 'UN'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$to, 'UK'),]
preening_dat_20 = preening_dat_20[!str_detect(preening_dat_20$to, 'STUMP'),]
preening_dat_21 = social_data_21[which(social_data_21$which == 'preen'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$from, 'X'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$from, 'UN'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$from, 'UK'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$from, 'STUMP'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$to, 'X'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$to, 'UN'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$to, 'UK'),]
preening_dat_21 = preening_dat_21[!str_detect(preening_dat_21$to, 'STUMP'),]

# Little workaround to add preening based mate IDs
dat_20$mate_id_preening = NA
ii = 0
for(i in seq_len(nrow(dat_20))){
  if(!is.na(dat_20$mate_id_preening[i])) next
  preen_sub = preening_dat_20[preening_dat_20$from == dat_20$id[i] | preening_dat_20$to == dat_20$id[i],]
  if(nrow(preen_sub) == 0) next
  ii = ii + 1
  all_ids = unique(unlist(preen_sub[,c('from', 'to')]))
  dat_20$mate_id_preening[dat_20$id %in% all_ids] = ii
}
dat_21$mate_id_preening = NA
ii = 0
for(i in seq_len(nrow(dat_21))){
  if(!is.na(dat_21$mate_id_preening[i])) next
  preen_sub = preening_dat_21[preening_dat_21$from == dat_21$id[i] | preening_dat_21$to == dat_21$id[i],]
  if(nrow(preen_sub) == 0) next
  ii = ii + 1
  all_ids = unique(unlist(preen_sub[,c('from', 'to')]))
  dat_21$mate_id_preening[dat_21$id %in% all_ids] = ii
}

# Merging the two IDs
dat_20$mate_id_preening = -dat_20$mate_id_preening # make sure the two mate_ids are never the same across
dat_20$mate_id = vapply(seq_len(nrow(dat_20)), function(i){
  if(!is.na(dat_20$mate_id_nest[i])) return(dat_20$mate_id_nest[i])
  if(is.na(dat_20$mate_id_nest[i]) & is.na(dat_20$mate_id_preening[i])) return(NA)
  if(is.na(dat_20$mate_id_nest[i]) & !is.na(dat_20$mate_id_preening[i])){
    mate_id = dat_20$mate_id_nest[which(dat_20$mate_id_preening == dat_20$mate_id_preening[i])] |> 
      na.omit() |> unique()
    if(length(mate_id) > 1) stop('Problem mate ID in row ', i) 
    if(length(mate_id) == 0) return(dat_20$mate_id_preening[i]) else return(mate_id)
  }
}, numeric(1))
dat_21$mate_id_preening = -dat_21$mate_id_preening # make sure the two mate_ids are never the same across
dat_21$mate_id = vapply(seq_len(nrow(dat_21)), function(i){
  if(!is.na(dat_21$mate_id_nest[i])) return(dat_21$mate_id_nest[i])
  if(is.na(dat_21$mate_id_nest[i]) & is.na(dat_21$mate_id_preening[i])) return(NA)
  if(is.na(dat_21$mate_id_nest[i]) & !is.na(dat_21$mate_id_preening[i])){
    mate_id = dat_21$mate_id_nest[which(dat_21$mate_id_preening == dat_21$mate_id_preening[i])] |> 
      na.omit() |> unique()
    if(length(mate_id) > 1) stop('Problem mate ID in row ', i) 
    if(length(mate_id) == 0) return(dat_21$mate_id_preening[i]) else return(mate_id)
  }
}, numeric(1))

# Keeping individuals with ID
keep = !is.na(dat_20$mate_id)
dat_mate_id_20 = data.frame(id = dat_20$id[keep], mate_id = dat_20$mate_id[keep])
keep = !is.na(dat_21$mate_id)
dat_mate_id_21 = data.frame(id = dat_21$id[keep], mate_id = dat_21$mate_id[keep])

# Save
save(dat_mate_id_20, dat_mate_id_21, file = path_dat_mate_id)

# Making matrix
mate_network_20 = outer(dat_mate_id_20$mate_id, dat_mate_id_20$mate_id, "==") + 0
diag(mate_network_20) = NA
rownames(mate_network_20) = colnames(mate_network_20) = dat_mate_id_20$id
mate_network_21 = outer(dat_mate_id_21$mate_id, dat_mate_id_21$mate_id, "==") + 0
diag(mate_network_21) = NA
rownames(mate_network_21) = colnames(mate_network_21) = dat_mate_id_21$id

# Save
save(mate_network_20, mate_network_21, file = path_mate_network)
