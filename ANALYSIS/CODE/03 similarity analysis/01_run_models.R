# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 07-11-2022
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Running the models that explain call similarity. 
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('cmdstanr', 'rethinking', 'tidyverse')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data
load(path_dat_similarity)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 2020 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Run model foraging total effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'for_edge_weight', 
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$for_edge_weight = clean_dat$for_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_foraging_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_foraging_total_20
clean_dat_foraging_total_20 = clean_dat
fit_foraging_total_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_foraging_total_20)

# Run model foraging direct effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'for_edge_weight', 
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight',
                      'gen_edge_weight',
                      'tol_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$for_edge_weight = clean_dat$for_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$tol_edge_weight = clean_dat$tol_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_foraging_direct)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_foraging_direct_20
clean_dat_foraging_direct_20 = clean_dat
fit_foraging_direct_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_foraging_direct_20)

# Run model mate total effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'same_mate',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_mate_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_mate_total_20
clean_dat_mate_total_20 = clean_dat
fit_mate_total_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_mate_total_20)

# Run model relatedness total effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_gen_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_gen_total_20
clean_dat_gen_total_20 = clean_dat
fit_gen_total_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_gen_total_20)

# Run model relatedness direct effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'gen_edge_weight',
                      'same_cluster', 
                      'same_mate')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_gen_direct)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_gen_direct_20
clean_dat_gen_direct_20 = clean_dat
fit_gen_direct_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_gen_direct_20)

# Run model aggressive effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'ag_edge_weight',
                      'same_mate',
                      'same_cluster',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$ag_edge_weight = clean_dat$ag_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_aggressive)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_ag_20
clean_dat_ag_20 = clean_dat
fit_ag_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_ag_20)

# Run model spatial distance total effect ----
clean_dat = dat_20[,c('acc_dist', 
                      'acc_dist_se',
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight', 
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_spat_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_spat_total_20
clean_dat_spat_total_20 = clean_dat
fit_spat_total_20 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_spat_total_20)

# # Run model same cluster total effect ----
# clean_dat = dat_20[,c('acc_dist', 
#                       'acc_dist_se',
#                       'same_cluster')] %>% na.omit %>% as.list
# clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
# clean_dat$N_obs = length(clean_dat$acc_dist)
# model = cmdstan_model(path_m_clust_total)
# fit = model$sample(data = clean_dat, 
#                    seed = 1, 
#                    chains = 4, 
#                    parallel_chains = 4,
#                    refresh = 200, 
#                    adapt_delta = 0.99,
#                    max_treedepth = 15)
# fit$output_files() |>
#   rstan::read_stan_csv() |>
#   rethinking::extract.samples() -> post_clust_total_20
# clean_dat_clust_total_20 = clean_dat
# fit_clust_total_20 = fit$output_files() |>
#   rstan::read_stan_csv()
# precis(fit_clust_total_20)

# # Run model same location total effect ----
# clean_dat = dat_20[,c('acc_dist', 
#                       'acc_dist_se',
#                       'same_location')] %>% na.omit %>% as.list
# clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
# clean_dat$N_obs = length(clean_dat$acc_dist)
# model = cmdstan_model(path_m_location_total)
# fit = model$sample(data = clean_dat, 
#                    seed = 1, 
#                    chains = 4, 
#                    parallel_chains = 4,
#                    refresh = 200, 
#                    adapt_delta = 0.99,
#                    max_treedepth = 15)
# fit$output_files() |>
#   rstan::read_stan_csv() |>
#   rethinking::extract.samples() -> post_loc_total_20
# clean_dat_loc_total_20 = clean_dat
# fit_loc_total_20 = fit$output_files() |>
#   rstan::read_stan_csv()
# precis(fit_loc_total_20)
# 
# # Run model same location direct effect ----
# clean_dat = dat_20[,c('acc_dist', 
#                       'acc_dist_se',
#                       'same_location',
#                       'same_mate',
#                       'same_cluster',
#                       'for_edge_weight',
#                       'spat_edge_weight')] %>% na.omit %>% as.list
# clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
# clean_dat$N_obs = length(clean_dat$acc_dist)
# model = cmdstan_model(path_m_location_direct)
# fit = model$sample(data = clean_dat, 
#                    seed = 1, 
#                    chains = 4, 
#                    parallel_chains = 4,
#                    refresh = 200, 
#                    adapt_delta = 0.99,
#                    max_treedepth = 15)
# fit$output_files() |>
#   rstan::read_stan_csv() |>
#   rethinking::extract.samples() -> post_loc_direct_20
# clean_dat_loc_direct_20 = clean_dat
# fit_loc_direct_20 = fit$output_files() |>
#   rstan::read_stan_csv()
# precis(fit_loc_direct_20)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 2021 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Run model foraging total effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'for_edge_weight', 
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$for_edge_weight = clean_dat$for_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_foraging_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_foraging_total_21
clean_dat_foraging_total_21 = clean_dat
fit_foraging_total_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_foraging_total_21)

# Run model foraging direct effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'for_edge_weight', 
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight',
                      'gen_edge_weight',
                      'tol_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$for_edge_weight = clean_dat$for_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$tol_edge_weight = clean_dat$tol_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_foraging_direct)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_foraging_direct_21
clean_dat_foraging_direct_21 = clean_dat
fit_foraging_direct_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_foraging_direct_21)

# Run model mate total effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'same_mate',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_mate_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_mate_total_21
clean_dat_mate_total_21 = clean_dat
fit_mate_total_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_mate_total_21)

# Run model relatedness total effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_gen_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_gen_total_21
clean_dat_gen_total_21 = clean_dat
fit_gen_total_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_gen_total_21)

# Run model relatedness direct effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'gen_edge_weight',
                      'same_cluster', 
                      'same_mate')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_gen_direct)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_gen_direct_21
clean_dat_gen_direct_21 = clean_dat
fit_gen_direct_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_gen_direct_21)

# Run model aggressive effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'ag_edge_weight',
                      'same_mate',
                      'same_cluster',
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$ag_edge_weight = clean_dat$ag_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_aggressive)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_ag_21
clean_dat_ag_21 = clean_dat
fit_ag_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_ag_21)

# Run model spatial distance total effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'same_mate', 
                      'same_cluster',
                      'spat_edge_weight', 
                      'gen_edge_weight')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$spat_edge_weight = clean_dat$spat_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$gen_edge_weight = clean_dat$gen_edge_weight |> rethinking::normalize() |> as.numeric()
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_spat_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_spat_total_21
clean_dat_spat_total_21 = clean_dat
fit_spat_total_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_spat_total_21)

# Run model same cluster total effect ----
clean_dat = dat_21[,c('acc_dist', 
                      'acc_dist_se',
                      'same_cluster')] %>% na.omit %>% as.list
clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
clean_dat$N_obs = length(clean_dat$acc_dist)
model = cmdstan_model(path_m_clust_total)
fit = model$sample(data = clean_dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_clust_total_21
clean_dat_clust_total_21 = clean_dat
fit_clust_total_21 = fit$output_files() |>
  rstan::read_stan_csv()
precis(fit_clust_total_21)

# # Run model same location total effect ----
# clean_dat = dat_21[,c('acc_dist', 
#                       'acc_dist_se',
#                       'same_location')] %>% na.omit %>% as.list
# clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
# clean_dat$N_obs = length(clean_dat$acc_dist)
# model = cmdstan_model(path_m_location_total)
# fit = model$sample(data = clean_dat, 
#                    seed = 1, 
#                    chains = 4, 
#                    parallel_chains = 4,
#                    refresh = 200, 
#                    adapt_delta = 0.99,
#                    max_treedepth = 15)
# fit$output_files() |>
#   rstan::read_stan_csv() |>
#   rethinking::extract.samples() -> post_loc_total_21
# clean_dat_loc_total_21 = clean_dat
# fit_loc_total_21 = fit$output_files() |>
#   rstan::read_stan_csv()
# precis(fit_loc_total_21)
# 
# # Run model same location direct effect ----
# clean_dat = dat_21[,c('acc_dist', 
#                       'acc_dist_se',
#                       'same_location',
#                       'same_mate',
#                       'same_cluster',
#                       'for_edge_weight',
#                       'spat_edge_weight')] %>% na.omit %>% as.list
# clean_dat$acc_dist = clean_dat$acc_dist %>% scale %>% as.numeric
# clean_dat$N_obs = length(clean_dat$acc_dist)
# model = cmdstan_model(path_m_location_direct)
# fit = model$sample(data = clean_dat, 
#                    seed = 1, 
#                    chains = 4, 
#                    parallel_chains = 4,
#                    refresh = 200, 
#                    adapt_delta = 0.99,
#                    max_treedepth = 15)
# fit$output_files() |>
#   rstan::read_stan_csv() |>
#   rethinking::extract.samples() -> post_loc_direct_21
# clean_dat_loc_direct_21 = clean_dat
# fit_loc_direct_21 = fit$output_files() |>
#   rstan::read_stan_csv()
# precis(fit_loc_direct_21)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Save all output ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

save(clean_dat_foraging_total_20, clean_dat_foraging_direct_20, clean_dat_mate_total_20, 
     clean_dat_spat_total_20, clean_dat_ag_20, clean_dat_gen_total_20, clean_dat_gen_direct_20,
     fit_foraging_total_20, fit_foraging_direct_20, fit_mate_total_20, fit_spat_total_20, 
     fit_ag_20, fit_gen_total_20, fit_gen_direct_20,
     post_foraging_total_20, post_foraging_direct_20, post_mate_total_20, post_spat_total_20, post_ag_20, 
     post_gen_total_20, post_gen_direct_20,
     clean_dat_foraging_total_21, clean_dat_foraging_direct_21, clean_dat_mate_total_21, 
     clean_dat_spat_total_21, clean_dat_ag_21, clean_dat_gen_total_21, clean_dat_gen_direct_21,
     fit_foraging_total_21, fit_foraging_direct_21, fit_mate_total_21, fit_spat_total_21, 
     fit_ag_21, fit_gen_total_21, fit_gen_direct_21,
     post_foraging_total_21, post_foraging_direct_21, post_mate_total_21, post_spat_total_21, post_ag_21, 
     post_gen_total_21, post_gen_direct_21,
     file = path_sim_models)
