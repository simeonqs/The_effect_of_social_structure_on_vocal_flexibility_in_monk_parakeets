# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 23-02-2023
# Date last modified: 03-03-2023
# Author: Simeon Q. Smeele
# Description: Running the clean models for contact call information content.   
# This version includes the 2020 data. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Set-up ----

# Loading libraries
libraries = c('tidyverse', 'cmdstanr', 'rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Load data
load(path_master_dat_info)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2020 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Run model degree ----
clean_dat = data.frame(n = master_dat_20$fm,
                       ind_ID = master_dat_20$bird,
                       degree = as.numeric(scale(master_dat_20$degree)),
                       age = as.numeric(scale(master_dat_20$age)),
                       tree_size = as.numeric(scale(master_dat_20$tree_count)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_degree.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_degree_20
fit_degree_20 = fit
clean_dat_degree_20 = clean_dat

# Run model tree size total ----
clean_dat = data.frame(n = master_dat_20$fm,
                       ind_ID = master_dat_20$bird,
                       tree_size = as.numeric(scale(master_dat_20$tree_count)),
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       degree = as.numeric(scale(master_dat_20$degree)),
                       age = as.numeric(scale(master_dat_20$age)),
                       entry_size = as.numeric(scale(master_dat_20$entry_count)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_tree_total.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_tree_total_20
fit_tree_total_20 = fit
clean_dat_tree_total_20 = clean_dat

# Run model age total ----
clean_dat = data.frame(n = master_dat_20$fm,
                       ind_ID = master_dat_20$bird,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       age = as.numeric(scale(master_dat_20$age)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_age_total.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_age_total_20
fit_age_total_20 = fit
clean_dat_age_total_20 = clean_dat

## 2021 ## ----

# Run model degree ----
clean_dat = data.frame(n = master_dat_21$fm,
                       ind_ID = master_dat_21$bird,
                       degree = as.numeric(scale(master_dat_21$degree)),
                       age = as.numeric(scale(master_dat_21$age)),
                       tree_size = as.numeric(scale(master_dat_21$tree_count)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_degree.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_degree_21
fit_degree_21 = fit
clean_dat_degree_21 = clean_dat

# Run model tree size total ----
clean_dat = data.frame(n = master_dat_21$fm,
                       ind_ID = master_dat_21$bird,
                       tree_size = as.numeric(scale(master_dat_21$tree_count)),
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       degree = as.numeric(scale(master_dat_21$degree)),
                       age = as.numeric(scale(master_dat_21$age)),
                       entry_size = as.numeric(scale(master_dat_21$entry_count)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_tree_total.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_tree_total_21
fit_tree_total_21 = fit
clean_dat_tree_total_21 = clean_dat

# Run model age total ----
clean_dat = data.frame(n = master_dat_21$fm,
                       ind_ID = master_dat_21$bird,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       age = as.numeric(scale(master_dat_21$age)))
clean_dat = na.omit(clean_dat)
clean_dat$ind_ID = as.integer(as.factor(clean_dat$ind_ID))
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$n)
clean_dat$N_ind = max(clean_dat$ind_ID)
model = cmdstan_model('ANALYSIS/CODE/07 information content/m_info_age_total.stan')
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
print(precis(fit$output_files() |>
               rstan::read_stan_csv()))
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_age_total_21
fit_age_total_21 = fit
clean_dat_age_total_21 = clean_dat

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Save ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

save(post_degree_20, post_tree_total_20, post_age_total_20, fit_degree_20, fit_tree_total_20, 
     fit_age_total_20, clean_dat_degree_20, clean_dat_tree_total_20, clean_dat_age_total_20,
     post_degree_21, post_tree_total_21, post_age_total_21, fit_degree_21, fit_tree_total_21, 
     fit_age_total_21,clean_dat_degree_21, clean_dat_tree_total_21, clean_dat_age_total_21,
     file = path_all_results_info)

