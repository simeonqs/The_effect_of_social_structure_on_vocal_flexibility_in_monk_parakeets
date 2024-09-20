# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 02-07-2022
# Date last modified: 02-03-2023
# Author: Simeon Q. Smeele
# Description: Running the individual variability model on the DTW results for 2021. 
# This version also exports the summarised results. 
# This version also includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('cmdstanr', 'rstan', 'rethinking', 'tidyverse')
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

# Find most similar dyads
m_20 = m_20
m_21 = m_21
diag(m_20) = NA
diag(m_21) = NA
max_m_20 = max(m_20, na.rm = T)
max_m_21 = max(m_21, na.rm = T)
which_max_20 = which(m_20 == max_m_20, arr.ind = TRUE)
which_max_21 = which(m_21 == max_m_21, arr.ind = TRUE)
min_m_20 = min(m_20, na.rm = T)
min_m_21 = min(m_21, na.rm = T)
which_min_20 = which(m_20 == min_m_20, arr.ind = TRUE)
which_min_21 = which(m_21 == min_m_21, arr.ind = TRUE)
par(mfrow = c(2, 2))
plot(smooth_traces_20[[rownames(m_20)[which_max_20[1,1]]]])
plot(smooth_traces_20[[rownames(m_20)[which_max_20[1,2]]]])
plot(smooth_traces_20[[rownames(m_20)[which_min_20[1,1]]]])
plot(smooth_traces_20[[rownames(m_20)[which_min_20[1,2]]]])
plot(smooth_traces_21[[rownames(m_21)[which_max_21[1,1]]]])
plot(smooth_traces_21[[rownames(m_21)[which_max_21[1,2]]]])
plot(smooth_traces_21[[rownames(m_21)[which_min_21[1,1]]]])
plot(smooth_traces_21[[rownames(m_21)[which_min_21[1,2]]]])

