# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 04-11-2022
# Date last modified: 03-03-2023
# Author: Simeon Q. Smeele
# Description: Checking if the most and least variable individuals are actually correct. 
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
load(path_similarity_out)

# Find lowest and highest variability
low_dyad = similarity_out_21$char[similarity_out_21$mean_a_ind_pair == 
                                    min(similarity_out_21$mean_a_ind_pair)]
high_dyad = similarity_out_21$char[similarity_out_21$mean_a_ind_pair == 
                                     max(similarity_out_21$mean_a_ind_pair)]

# Print 12 random traces for both
pdf(path_pdf_test_similarity, 10, 10)
par(mfrow = c(4, 4))
split_low = strsplit(low_dyad, ' ')[[1]]
files_low_1 = st_21$fs[st_21$bird == split_low[1]] %>% sample(4, replace = T)
files_low_2 = st_21$fs[st_21$bird == split_low[2]] %>% sample(4, replace = T)
for(file in c(files_low_1, files_low_2)) plot(smooth_traces_21[[file]])
split_high = strsplit(high_dyad, ' ')[[1]]
files_high_1 = st_21$fs[st_21$bird == split_high[1]] %>% sample(4, replace = T)
files_high_2 = st_21$fs[st_21$bird == split_high[2]] %>% sample(4, replace = T)
for(file in c(files_high_1, files_high_2)) plot(smooth_traces_21[[file]])
dev.off()

# Plot against raw data
before_merge = as.data.frame(similarity_in_dat[c('acc_dist', 'ind_pair')])
merged = merge(before_merge, similarity_out, by.x = 'ind_pair', by.y = 'index',
               all.x = T, all.y = T)
plot(merged$acc_dist, merged$mean_a_ind_pair)


