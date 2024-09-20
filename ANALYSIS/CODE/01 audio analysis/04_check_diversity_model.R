# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 04-11-2022
# Date last modified: 03-03-2023
# Author: Simeon Q. Smeele
# Description: Checking if the most and least variable individuals are actually correct. 
# This version also checks if variability is independent of sample size. 
# This version includes the 2020 data. 
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
load(path_div_results_slim)

# Find lowest and highest variability
low_ind = var_results_slim_21$ind[var_results_slim_21$average_div == min(var_results_slim_21$average_div)]
high_ind = var_results_slim_21$ind[var_results_slim_21$average_div == max(var_results_slim_21$average_div)]

# Print 12 random traces for both
pdf(path_pdf_test_diversity, 10, 10)
par(mfrow = c(3, 4))
files_low = st_21$fs[st_21$bird == low_ind] %>% sample(12)
files_high = st_21$fs[st_21$bird == high_ind] %>% sample(12)
for(file in files_low) plot(smooth_traces_21[[file]])
for(file in files_high) plot(smooth_traces_21[[file]])
dev.off()

# Check if output is correlated with sample size
inds_m = vapply(rownames(m_21), function(rn) st_21$bird[st_21$fs == rn], character(1))
ss_per_ind = vapply(var_results_slim_21$ind, function(ind) length(which(inds_m == ind)), numeric(1))
plot(log(ss_per_ind), var_results_slim_21$average_div)
