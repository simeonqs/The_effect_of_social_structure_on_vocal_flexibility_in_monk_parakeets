# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 23-02-2023
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Calculate the information content for contact calls.
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'parallel', 'callsync')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(lib, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
load(path_data)

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

## 2020 ##

# Subset for loud contact calls
smooth_traces_20 = smooth_traces_20[names(smooth_traces_20) %in% data_sets_20$loud_contact]

# Calculate frequency modulation
fm_dat = data.frame(fs = names(smooth_traces_20))
fm_dat = merge(fm_dat, st_20, by = 'fs', all.x = TRUE, all.y = FALSE)
fm_dat$fm = vapply(smooth_traces_20, function(trace) calc.fm(trace, min_height = 30)$np, numeric(1))

# Load data age
load(path_age_dat)
inds_age = age_dat$ind %>% unique

# Load data sex
sexing_f = read.csv(path_sexing_f)
sexing_v = read_xlsx(path_sexing_v)
sexing = na.omit(rbind(sexing_f, sexing_v))
sexing$ID = toupper(sexing$ID)
sexing$sex = toupper(sexing$sex)
inds_sex = sexing$ID %>% unique

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_20$ind

# Make master list of individuals
master_dat = data.frame(ind = unique(c(fm_dat$bird, inds_age, inds_sex, inds_nesting)))

# Add age
age_dat$age = as.Date('2021-11-01') - as.Date(age_dat$hatch_max)
master_dat = merge(master_dat, age_dat[,c('ind', 'age')], by = 'ind', all.x = T, all.y = F)

# Add sex
master_dat = merge(master_dat, sexing, by.x = 'ind', by.y = 'ID', all.x = T, all.y = F)

# Add classical SNA measures and sample size
load(path_net_meas)
master_dat = merge(master_dat, net_meas_20, by = 'ind', all.x = T, all.y = F)

# Add nesting counts
master_dat = merge(master_dat, nesting_sizes_20, by = 'ind', all.x = T, all.y = F)

# Add nest type
nest_overview = read.csv2(path_nest_overview_20, na.strings = 'NA')
nest_per_ind = read.csv2(path_nest_per_ind_20)
nest_type_data = merge(nest_per_ind, nest_overview, 
                       by.x = 'tree_manual', by.y = 'tree', all.x = T, all.y = F)
# master_dat$tree_type = sapply(master_dat$ind, function(ind){
#   y = nest_type_data$type[nest_type_data$id == ind]
#   return(ifelse(length(y) > 0, y, NA))
# })

# Add tree ID
master_dat$tree_ID = as.character(sapply(master_dat$ind, function(ind){
  y = nest_per_ind$tree_manual[nest_per_ind$id == ind]
  return(ifelse(length(y) > 0, y, NA))
}))

# Make master_dat into data.frame with multiple entries per individual
master_dat_20 = merge(fm_dat[,c('bird', 'fm')], master_dat, by.x = 'bird', by.y = 'ind', all.x = T, all.y = F)

## 2021 ##

# Subset for loud contact calls
smooth_traces_21 = smooth_traces_21[names(smooth_traces_21) %in% data_sets_21$loud_contact]

# Calculate frequency modulation
fm_dat = data.frame(fs = names(smooth_traces_21))
fm_dat = merge(fm_dat, st_21, by = 'fs', all.x = TRUE, all.y = FALSE)
fm_dat$fm = vapply(smooth_traces_21, function(trace) calc.fm(trace, min_height = 30)$np, numeric(1))

# Load data age
load(path_age_dat)
inds_age = age_dat$ind %>% unique

# Load data sex
sexing_f = read.csv(path_sexing_f)
sexing_v = read_xlsx(path_sexing_v)
sexing = na.omit(rbind(sexing_f, sexing_v))
sexing$ID = toupper(sexing$ID)
sexing$sex = toupper(sexing$sex)
inds_sex = sexing$ID %>% unique

# Load nesting results
load(path_nesting_sizes)
inds_nesting = nesting_sizes_21$ind

# Make master list of individuals
master_dat = data.frame(ind = unique(c(fm_dat$bird, inds_age, inds_sex, inds_nesting)))

# Add age
age_dat$age = as.Date('2021-11-01') - as.Date(age_dat$hatch_max)
master_dat = merge(master_dat, age_dat[,c('ind', 'age')], by = 'ind', all.x = T, all.y = F)

# Add sex
master_dat = merge(master_dat, sexing, by.x = 'ind', by.y = 'ID', all.x = T, all.y = F)

# Add classical SNA measures and sample size
load(path_net_meas)
master_dat = merge(master_dat, net_meas_21, by = 'ind', all.x = T, all.y = F)

# Add nesting counts
master_dat = merge(master_dat, nesting_sizes_21, by = 'ind', all.x = T, all.y = F)

# Add nest type
nest_overview = read.csv2(path_nest_overview_21, na.strings = 'NA')
nest_per_ind = read.csv2(path_nest_per_ind_21)
nest_type_data = merge(nest_per_ind, nest_overview, 
                       by.x = 'tree_manual', by.y = 'tree', all.x = T, all.y = F)
# master_dat$tree_type = sapply(master_dat$ind, function(ind){
#   y = nest_type_data$type[nest_type_data$id == ind]
#   return(ifelse(length(y) > 0, y, NA))
# })

# Add tree ID
master_dat$tree_ID = as.character(sapply(master_dat$ind, function(ind){
  y = nest_per_ind$tree_manual[nest_per_ind$id == ind]
  return(ifelse(length(y) > 0, y, NA))
}))

# Make master_dat into data.frame with multiple entries per individual
master_dat_21 = merge(fm_dat[,c('bird', 'fm')], master_dat, by.x = 'bird', by.y = 'ind', all.x = T, all.y = F)

## Save ##
save(master_dat_20, master_dat_21, file = path_master_dat_info)

## Message ##
message(sprintf('2020 - inds: %s, calls: %s', length(unique(master_dat_20$bird)), nrow(master_dat_20)))
message(sprintf('2021 - inds: %s, calls: %s', length(unique(master_dat_21$bird)), nrow(master_dat_21)))
