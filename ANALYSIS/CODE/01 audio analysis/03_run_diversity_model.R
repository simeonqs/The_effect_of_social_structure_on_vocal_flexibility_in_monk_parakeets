# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 02-07-2022
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Running the individual diversity model on the DTW results for 2021. 
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

# Prep data
set.seed(1)
n_20 = rownames(m_20) # file_selection names
n_21 = rownames(m_21)
subber_20 = seq_len(length(n_20)) # create index
subber_21 = seq_len(length(n_21))
if(length(n_20) > 1500) subber_20 = sample(length(n_20), 1500) 
if(length(n_21) > 1500) subber_21 = sample(length(n_21), 1500)
inds_20 = st_20[n_20,]$bird[subber_20]
inds_21 = st_21[n_21,]$bird[subber_21]
recs_20 = paste(st_20[n_20,]$bird[subber_20], st_20[n_20,]$file[subber_20]) %>% as.factor %>% as.integer
recs_21 = paste(st_21[n_21,]$bird[subber_21], st_21[n_21,]$file[subber_21]) %>% as.factor %>% as.integer
m_20 =  m_20[subber_20, subber_20]
m_21 =  m_21[subber_21, subber_21]

# Make vectors
## acoustic distance
dist_20 = as.vector(as.dist(m_20))
dist_21 = as.vector(as.dist(m_21))
clean_dat_20 = data.frame(acc_dist = (dist_20-min(dist_20))/max(dist_20-min(dist_20)))
clean_dat_21 = data.frame(acc_dist = (dist_21-min(dist_21))/max(dist_21-min(dist_21)))
## getting dimensions
l_20 = nrow(m_20)
l_21 = nrow(m_21)
c_20 = combn(seq_len(l_20), 2)
c_21 = combn(seq_len(l_21), 2)
## call
clean_dat_20$call_i = c_20[1,]
clean_dat_21$call_i = c_21[1,]
clean_dat_20$call_j = c_20[2,]
clean_dat_21$call_j = c_21[2,]
## same rec = 1, else = 2
clean_dat_20$same_rec = sapply(seq_len(ncol(c_20)), function(x) 
  ifelse(recs_20[c_20[1,x]] == recs_20[c_20[2,x]], 1, 2))
clean_dat_21$same_rec = sapply(seq_len(ncol(c_21)), function(x) 
  ifelse(recs_21[c_21[1,x]] == recs_21[c_21[2,x]], 1, 2))
## ind
clean_dat_20$ind_char = inds_20[c_20[1,]]
clean_dat_21$ind_char = inds_21[c_21[1,]]
## rec pair
clean_dat_20$rec_pair = sapply(seq_len(ncol(c_20)), function(x) 
  paste(sort(c(recs_20[c_20[1,x]], recs_20[c_20[2,x]])), collapse = ' '))
clean_dat_21$rec_pair = sapply(seq_len(ncol(c_21)), function(x) 
  paste(sort(c(recs_21[c_21[1,x]], recs_21[c_21[2,x]])), collapse = ' '))
## subset for within ind only
clean_dat_20 = clean_dat_20[inds_20[c_20[1,]] == inds_20[c_20[2,]],]
clean_dat_21 = clean_dat_21[inds_21[c_21[1,]] == inds_21[c_21[2,]],]
## redo integers
clean_dat_20$ind = clean_dat_20$ind_char %>% as.factor %>% as.integer
clean_dat_21$ind = clean_dat_21$ind_char %>% as.factor %>% as.integer
clean_dat_20$rec_pair = clean_dat_20$rec_pair %>% as.factor %>% as.integer
clean_dat_21$rec_pair = clean_dat_21$rec_pair %>% as.factor %>% as.integer
## store ind ids
ind_char_20 = clean_dat_20$ind_char
ind_char_21 = clean_dat_21$ind_char
clean_dat_20$ind_char = NULL
clean_dat_21$ind_char = NULL

# Add sample sizes
clean_dat_20 = as.list(clean_dat_20)
clean_dat_21 = as.list(clean_dat_21)
clean_dat_20$N_ind = max(clean_dat_20$ind)
clean_dat_21$N_ind = max(clean_dat_21$ind)
clean_dat_20$N_rec_pair = max(clean_dat_20$rec_pair)
clean_dat_21$N_rec_pair = max(clean_dat_21$rec_pair)
clean_dat_20$N_call = max(clean_dat_20$call_j)
clean_dat_21$N_call = max(clean_dat_21$call_j)
clean_dat_20$N_obs = length(clean_dat_20$call_i)
clean_dat_21$N_obs = length(clean_dat_21$call_i)

# Run model
model = cmdstan_model(path_div_model)
fit_20 = model$sample(data = clean_dat_20, 
                      seed = 1, 
                      chains = 4, 
                      parallel_chains = 4,
                      refresh = 100, 
                      adapt_delta = 0.99,
                      max_treedepth = 15)
fit_21 = model$sample(data = clean_dat_21, 
                      seed = 1, 
                      chains = 4, 
                      parallel_chains = 4,
                      refresh = 100, 
                      adapt_delta = 0.99,
                      max_treedepth = 15)
fit_20$output_files() %>%
  rstan::read_stan_csv() %>%
  rethinking::extract.samples() -> post_20
fit_21$output_files() %>%
  rstan::read_stan_csv() %>%
  rethinking::extract.samples() -> post_21

# Plot
means_20 = apply(post_20$z_ind, 2, mean)
means_21 = apply(post_21$z_ind, 2, mean)
pis_20 =  apply(post_20$z_ind, 2, PI)
pis_21 =  apply(post_21$z_ind, 2, PI)

# Save
save(post_20, post_21, clean_dat_20, clean_dat_21, ind_char_20, ind_char_20, file = path_div_model_results)

# Save results in smaller format (per ind)
trans_inds_20 = data.frame(ind_char = ind_char_20, ind_num = clean_dat_20$ind)
trans_inds_21 = data.frame(ind_char = ind_char_21, ind_num = clean_dat_21$ind)
trans_inds_20 = unique(trans_inds_20)
trans_inds_21 = unique(trans_inds_21)
inds_20 = trans_inds_20$ind_char
inds_21 = trans_inds_21$ind_char
rownames(trans_inds_20) = trans_inds_20$ind_char
rownames(trans_inds_21) = trans_inds_21$ind_char
divs_20 = lapply(trans_inds_20$ind_char, function(ind){
  current_ind = trans_inds_20[ind,]$ind_num
  return(post_20$z_ind[,current_ind] * post_20$sigma_ind)
})
divs_21 = lapply(trans_inds_21$ind_char, function(ind){
  current_ind = trans_inds_21[ind,]$ind_num
  return(post_21$z_ind[,current_ind] * post_21$sigma_ind)
})
averages_20 = sapply(divs_20, mean)
averages_21 = sapply(divs_21, mean)
ses_20 = sapply(divs_20, sd)
ses_21 = sapply(divs_21, sd)
div_results_slim_20 = data.frame(ind = inds_20, average_div = averages_20, se_div = ses_20)
div_results_slim_21 = data.frame(ind = inds_21, average_div = averages_21, se_div = ses_21)
save(div_results_slim_20, div_results_slim_21, file = path_div_results_slim)
