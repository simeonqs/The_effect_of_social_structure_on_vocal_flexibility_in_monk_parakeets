# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 28-04-2022
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Loading the social data from 2021 and preparing the foraging network. 
# This version uses STRAND.
# This version saved the gbi for the simple Bayesian analysis.  
# This version removes the STRAND section and adds a normal network. 
# This version adds date and location to the gbi. 
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('tidyverse', 'asnipe')
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

# Load data
social_data_20 = load.social.data.20(path_social_data_simeon_20,
                                     path_social_data_andres_20,
                                     path_social_data_mireia_20)
social_data_21 = load.social.data.21(path_social_data_simeon_21, 
                                     path_social_data_andres_21,
                                     path_overview_recordings_21)

# Subset for foraging network
social_data_20$unique_per_rec = ifelse(is.na(social_data_20$recording),
                                       paste(social_data_20$date, social_data_20$time),
                                       social_data_20$recording)
social_data_20$food = ifelse(is.na(social_data_20$food), 'na', social_data_20$food)
for_dat_20 = social_data_20[which(social_data_20$what %in% c('foraging', 'perched') &
                                    !str_detect(social_data_20$food, 'bread') &
                                    !is.na(social_data_20$group) &
                                    (!duplicated(social_data_20$unique_per_rec) | 
                                       !is.na(social_data_20$new_flock))),]
social_data_21$food = ifelse(is.na(social_data_21$food), 'na', social_data_21$food)
for_dat_21 = social_data_21[which(social_data_21$what  %in% c('foraging', 'perched') &
                                    !str_detect(social_data_21$food, 'bread') &
                                    !is.na(social_data_21$group)),]

# Create GBI and save
gbi_20 = create.gbi(for_dat_20, data_type = 2020)
gbi_20 = gbi_20[,colnames(gbi_20) != 'UNK']
gbi_20 = gbi_20[,!str_detect(colnames(gbi_20), '\\/')]
save(gbi_20, file = path_real_dat_gbi_20)

gbi_21 = create.gbi(for_dat_21, data_type = 2021)
gbi_21 = gbi_21[,colnames(gbi_21) != 'UNK']
gbi_21 = gbi_21[,!str_detect(colnames(gbi_21), '\\/')]
save(gbi_21, file = path_real_dat_gbi_21)

# Create network and save
keep = c(1, 2, which(colSums(gbi_20[,-c(1, 2)]) > 5) + 2)
gbi_20 = gbi_20[,keep]
keep = c(1, 2, which(colSums(gbi_21[,-c(1, 2)]) > 5) + 2)
gbi_21 = gbi_21[,keep]
foraging_network_20 =  get_network(association_data = gbi_20[,-c(1, 2)],
                                   data_format = 'GBI',
                                   association_index = 'SRI')
foraging_network_21 =  get_network(association_data = gbi_21[,-c(1, 2)],
                                   data_format = 'GBI',
                                   association_index = 'SRI')
save(foraging_network_20, foraging_network_21, file = path_foraging_network)

# Plot distribution of proportions of unmarked conspecifics for each individual
## IF RUNNING THIS FOR 2020 MAKE SURE TO COUNT EACH GROUP ONLY ONCE
pdf(path_pdf_prop_missing)
par(mfrow = c(2, 2))
for(ind in colnames(gbi_21)[-(1:2)]){
  sub = for_dat_21[str_detect(for_dat_21$group, ind),]
  props = vapply(seq_len(nrow(sub)), function(i){
    split = strsplit(sub$group[i], ', ')[[1]]
    return(length(which(!split %in% colnames(gbi_21)))/length(split))
  }, numeric(1))
  unmarks = vapply(seq_len(nrow(sub)), function(i){
    split = strsplit(sub$group[i], ', ')[[1]]
    return(length(which(!split %in% colnames(gbi_21))))
  }, numeric(1))
  all = vapply(seq_len(nrow(sub)), function(i){
    split = strsplit(sub$group[i], ', ')[[1]]
    return(length(split))
  }, numeric(1))
  plot(props, all + rnorm(length(all), sd = 0.1), xlim = c(0, 1), ylim = c(0, 20),
       xlab = 'proportion unmarked', ylab = 'group size', main = ind)
  abline(v = 0.5, lty = 2)
}
dev.off()

# Save gbi sample size
samp_size_gbi = data.frame(ind = colnames(gbi_21[,-c(1, 2)]), samp_size_gbi = colSums(gbi_21[,-c(1, 2)]))
save(samp_size_gbi, file = path_samp_size_gbi)