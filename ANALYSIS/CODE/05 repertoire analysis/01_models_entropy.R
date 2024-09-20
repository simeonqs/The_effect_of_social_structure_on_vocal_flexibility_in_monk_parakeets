# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 27-02-2023
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Running all models for repertoire entropy 
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
load(path_master_dat_rep_size)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2020 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

message('2020 - entropy for ', length(which(!is.na(master_dat_20$entropy))), ' individuals')
message('2020 - degree for ', length(which(!is.na(master_dat_20$degree))), ' individuals')

# Run model age total ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_total.stan')
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

# Run model sex total ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]))
clean_dat = na.omit(clean_dat)
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_sex_total.stan')
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
  rethinking::extract.samples() -> post_sex_total_20
fit_sex_total_20 = fit
clean_dat_sex_total_20 = clean_dat

# Run model age direct ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       netpos = master_dat_20$degree,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_age_direct_20
fit_age_direct_20 = fit
clean_dat_age_direct_20 = clean_dat

# Run model degree ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       netpos = master_dat_20$degree,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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

# Run model eigenvector ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       netpos = master_dat_20$eig_cent,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_eigcent_direct_20
fit_eigcent_direct_20 = fit
clean_dat_eigcent_direct_20 = clean_dat

# Run model betweenness ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       netpos = master_dat_20$betweenness + 1e6,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_betweenness_direct_20
fit_betweenness_direct_20 = fit
clean_dat_betweenness_direct_20 = clean_dat

# Run model degree versatility ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       netpos = master_dat_20$deg_ver,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_deg_ver_direct_20
fit_deg_ver_direct_20 = fit
clean_dat_deg_ver_direct_20 = clean_dat

# Run model entry size total ----
clean_dat = data.frame(entropy = master_dat_20$entropy,
                       age = master_dat_20$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_20$sex]),
                       entry_size = master_dat_20$entry_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_entry_total.stan')
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
  rethinking::extract.samples() -> post_entry_total_20
fit_entry_total_20 = fit
clean_dat_entry_total_20 = clean_dat

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2021 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

message('2021 - entropy for ', length(which(!is.na(master_dat_21$entropy))), ' individuals')
message('2021 - degree for ', length(which(!is.na(master_dat_21$degree))), ' individuals')

# Run model age total ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_total.stan')
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

# Run model sex total ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]))
clean_dat = na.omit(clean_dat)
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_sex_total.stan')
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
  rethinking::extract.samples() -> post_sex_total_21
fit_sex_total_21 = fit
clean_dat_sex_total_21 = clean_dat

# Run model age direct ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       netpos = master_dat_21$degree,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_age_direct_21
fit_age_direct_21 = fit
clean_dat_age_direct_21 = clean_dat

# Run model degree ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       netpos = master_dat_21$degree,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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

# Run model eigenvector ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       netpos = master_dat_21$eig_cent,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_eigcent_direct_21
fit_eigcent_direct_21 = fit
clean_dat_eigcent_direct_21 = clean_dat

# Run model betweenness ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       netpos = master_dat_21$betweenness + 1e6,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_betweenness_direct_21
fit_betweenness_direct_21 = fit
clean_dat_betweenness_direct_21 = clean_dat

# Run model degree versatility ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       netpos = master_dat_21$deg_ver,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_age_direct.stan')
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
  rethinking::extract.samples() -> post_deg_ver_direct_21
fit_deg_ver_direct_21 = fit
clean_dat_deg_ver_direct_21 = clean_dat

# Run model entry size total ----
clean_dat = data.frame(entropy = master_dat_21$entropy,
                       age = master_dat_21$age,
                       sex = as.integer(c(F = 1, M = 2)[master_dat_21$sex]),
                       entry_size = master_dat_21$entry_count)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$entropy)
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/m_rep_ent_entry_total.stan')
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
  rethinking::extract.samples() -> post_entry_total_21
fit_entry_total_21 = fit
clean_dat_entry_total_21 = clean_dat

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Save ---- 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

save(clean_dat_age_direct_20, clean_dat_age_total_20, clean_dat_betweenness_direct_20, clean_dat_degree_20,
     clean_dat_eigcent_direct_20, post_age_direct_20, post_age_total_20, post_betweenness_direct_20, 
     post_degree_20, post_eigcent_direct_20, fit_age_direct_20, fit_age_total_20, fit_betweenness_direct_20, 
     fit_degree_20, fit_eigcent_direct_20, clean_dat_sex_total_20, post_sex_total_20, fit_sex_total_20, 
     post_entry_total_20, fit_entry_total_20, clean_dat_entry_total_20,
     clean_dat_age_direct_21, clean_dat_age_total_21, clean_dat_betweenness_direct_21, clean_dat_degree_21,
     clean_dat_eigcent_direct_21, post_age_direct_21, post_age_total_21, post_betweenness_direct_21, 
     post_degree_21, post_eigcent_direct_21, fit_age_direct_21, fit_age_total_21, fit_betweenness_direct_21, 
     fit_degree_21, fit_eigcent_direct_21, clean_dat_sex_total_21, post_sex_total_21, fit_sex_total_21, 
     post_entry_total_21, fit_entry_total_21, clean_dat_entry_total_21,
     clean_dat_deg_ver_direct_20, clean_dat_deg_ver_direct_21, post_deg_ver_direct_20, post_deg_ver_direct_21,
     fit_deg_ver_direct_20, fit_deg_ver_direct_21,
     file = path_all_results_rep_ent)