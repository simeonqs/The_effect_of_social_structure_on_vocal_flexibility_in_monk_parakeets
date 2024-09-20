# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 15-04-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: Based on similar script from the voice paper. Runs dynamic time warping on the contact calls
# from both years and saves the distance matrix. 
# This version also includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'dtw', 'parallel', 'callsync')
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

# Run DTW per dataset
m_20 = run.dtw(smooth_traces_20[which(names(smooth_traces_20) %in% data_sets_20$contact)])
m_21 = run.dtw(smooth_traces_21[which(names(smooth_traces_21) %in% data_sets_21$contact)])

# Save
save(m_20, m_21, file = path_dtw_m)

# Report
message('Saved dtw results.')