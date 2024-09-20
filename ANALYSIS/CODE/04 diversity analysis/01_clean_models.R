# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 03-02-2023
# Date last modified: 09-03-2023
# Author: Simeon Q. Smeele
# Description: Trying to create an overview of all the models we need and plot their outcome. 
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
load(path_master_dat)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2020 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Print sample sizes ----

sample_sizes = c(diversity = length(which(!is.na(master_dat_20$average_div))),
                 age = length(which(!is.na(master_dat_20$age))),
                 degree = length(which(!is.na(master_dat_20$degree))),
                 sex = length(which(!is.na(master_dat_20$sex))),
                 entry = length(which(!is.na(master_dat_20$entry_count))),
                 tree = length(which(!is.na(master_dat_20$tree_count))))
write.table(sample_sizes, 'ANALYSIS/RESULTS/04 diversity analysis/sample sizes 2020.txt', col.names = FALSE)

# Run total effect age ----
clean_dat = data.frame(age = master_dat_20$age,
                       average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$age)
model = cmdstan_model(path_age_un_model)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_age_20
clean_dat_total_age_20 = clean_dat
fit_total_age_20 = fit

# Run model direct effect age ----
clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       entry_size = master_dat_20$entry_count,
                       degree = master_dat_20$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_age)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_age_20
clean_dat_direct_age_20 = clean_dat
fit_direct_age_20 = fit

# Run model total and direct effect degree ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count,
                       degree = master_dat_20$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_degree)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_degree_20
clean_dat_degree_20 = clean_dat
fit_degree_20 = fit

# Run total effect sex ----
clean_dat = data.frame(sex = c(F = 1, M = 2)[master_dat_20$sex],
                       average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div)
clean_dat = na.omit(clean_dat)
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_div_total_sex)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_sex_20
clean_dat_total_sex_20 = clean_dat
fit_total_sex_20 = fit

# Run model total effect EntrySize ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       entry_size = master_dat_20$entry_count,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_total_effect_entry)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_entry_20
clean_dat_total_entry_20 = clean_dat
fit_total_entry_20 = fit

# Run model direct effect EntrySize ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_entry)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_entry_20
clean_dat_direct_entry_20 = clean_dat
fit_direct_entry_20 = fit

# Run model total effect tree type ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       entry_size = master_dat_20$entry_count,
                       tree_type = ifelse(master_dat_20$tree_count < 10, 1L, 2L),
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_total_effect_tree_type)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_tree_type_20
clean_dat_total_tree_type_20 = clean_dat
fit_total_tree_type_20 = fit

# Run model direct effect tree type ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       entry_size = master_dat_20$entry_count,
                       tree_type = ifelse(master_dat_20$tree_count < 10, 1L, 2L),
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       degree = master_dat_20$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_tree_type)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_tree_type_20
clean_dat_direct_tree_type_20 = clean_dat
fit_direct_tree_type_20 = fit

# Run model direct effect nest size ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$nest_count,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       degree = master_dat_20$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_degree)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_nest_size_20
clean_dat_direct_nest_size_20 = clean_dat
fit_direct_nest_size_20 = fit

# Run model total and direct effect eigenvector centrality ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count,
                       netpos = master_dat_20$eig_cent)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_eigcent_20
clean_dat_eigcent_20 = clean_dat
fit_eigcent_20 = fit

# Run model total and direct effect betweenness centrality ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count,
                       netpos = master_dat_20$betweenness + 1e6)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_betweenness_20
clean_dat_betweenness_20 = clean_dat
fit_betweenness_20 = fit

# Run model total and direct effect degree versatility ----

clean_dat = data.frame(average_div = master_dat_20$average_div,
                       se_div = master_dat_20$se_div,
                       age = master_dat_20$age,
                       sex = as.numeric(as.factor(master_dat_20$sex)),
                       entry_size = master_dat_20$entry_count,
                       tree_size = master_dat_20$tree_count,
                       netpos = master_dat_20$deg_ver + 1e6)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_deg_ver_20
clean_dat_deg_ver_20 = clean_dat
fit_deg_ver_20 = fit

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## 2021 ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Print sample sizes ----

sample_sizes = c(diversity = length(which(!is.na(master_dat_21$average_div))),
                 age = length(which(!is.na(master_dat_21$age))),
                 degree = length(which(!is.na(master_dat_21$degree))),
                 sex = length(which(!is.na(master_dat_21$sex))),
                 entry = length(which(!is.na(master_dat_21$entry_count))),
                 tree = length(which(!is.na(master_dat_21$tree_count))))
write.table(sample_sizes, 'ANALYSIS/RESULTS/04 diversity analysis/sample sizes 2020.txt', col.names = FALSE)

# Run total effect age ----
clean_dat = data.frame(age = master_dat_21$age,
                       average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$age)
model = cmdstan_model(path_age_un_model)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_age_21
clean_dat_total_age_21 = clean_dat
fit_total_age_21 = fit

# Run model direct effect age ----
clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       entry_size = master_dat_21$entry_count,
                       degree = master_dat_21$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_age)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_age_21
clean_dat_direct_age_21 = clean_dat
fit_direct_age_21 = fit

# Run model total and direct effect degree ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count,
                       degree = master_dat_21$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_degree)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_degree_21
clean_dat_degree_21 = clean_dat
fit_degree_21 = fit

# Run total effect sex ----
clean_dat = data.frame(sex = c(F = 1, M = 2)[master_dat_21$sex],
                       average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div)
clean_dat = na.omit(clean_dat)
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_div_total_sex)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_sex_21
clean_dat_total_sex_21 = clean_dat
fit_total_sex_21 = fit

# Run model total effect EntrySize ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       entry_size = master_dat_21$entry_count,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_total_effect_entry)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_entry_21
clean_dat_total_entry_21 = clean_dat
fit_total_entry_21 = fit

# Run model direct effect EntrySize ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_entry)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_entry_21
clean_dat_direct_entry_21 = clean_dat
fit_direct_entry_21 = fit

# Run model total effect tree type ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       entry_size = master_dat_21$entry_count,
                       tree_type = ifelse(master_dat_21$tree_count < 10, 1L, 2L),
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)))
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_total_effect_tree_type)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_total_tree_type_21
clean_dat_total_tree_type_21 = clean_dat
fit_total_tree_type_21 = fit

# Run model direct effect tree type ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       entry_size = master_dat_21$entry_count,
                       tree_type = ifelse(master_dat_21$tree_count < 10, 1L, 2L),
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       degree = master_dat_21$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_direct_effect_tree_type)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_tree_type_21
clean_dat_direct_tree_type_21 = clean_dat
fit_direct_tree_type_21 = fit

# Run model direct effect nest size ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$nest_count,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       degree = master_dat_21$degree)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$degree = clean_dat$degree |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_degree)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)

fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_direct_nest_size_21
clean_dat_direct_nest_size_21 = clean_dat
fit_direct_nest_size_21 = fit

# Run model total and direct effect eigenvector centrality ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count,
                       netpos = master_dat_21$eig_cent)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_eigcent_21
clean_dat_eigcent_21 = clean_dat
fit_eigcent_21 = fit

# Run model total and direct effect betweenness centrality ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count,
                       netpos = master_dat_21$betweenness + 1e6)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_betweenness_21
clean_dat_betweenness_21 = clean_dat
fit_betweenness_21 = fit

# Run model total and direct effect degree versatility ----

clean_dat = data.frame(average_div = master_dat_21$average_div,
                       se_div = master_dat_21$se_div,
                       age = master_dat_21$age,
                       sex = as.numeric(as.factor(master_dat_21$sex)),
                       entry_size = master_dat_21$entry_count,
                       tree_size = master_dat_21$tree_count,
                       netpos = master_dat_21$deg_ver + 1e6)
clean_dat = na.omit(clean_dat)
clean_dat$age = clean_dat$age |> as.numeric() |> log() |> scale() |> as.numeric()
clean_dat$entry_size = clean_dat$entry_size |> scale() |> as.numeric()
clean_dat$tree_size = clean_dat$tree_size |> scale() |> as.numeric()
clean_dat$netpos = clean_dat$netpos |> log() |> scale() |> as.numeric()
clean_dat = as.list(clean_dat)
clean_dat$N_obs = length(clean_dat$average_div)
model = cmdstan_model(path_m_netpos)
fit = model$sample(data = clean_dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,
                   refresh = 200)
fit$output_files() |>
  rstan::read_stan_csv() |>
  rethinking::extract.samples() -> post_deg_ver_21
clean_dat_deg_ver_21 = clean_dat
fit_deg_ver_21 = fit

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Store all posts ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

save(post_total_age_20, post_degree_20, post_direct_age_20, post_direct_entry_20, post_total_entry_20, 
     post_total_sex_20, post_eigcent_20, post_betweenness_20, post_deg_ver_20,
     clean_dat_total_age_20, clean_dat_degree_20, clean_dat_direct_age_20, clean_dat_direct_entry_20, 
     clean_dat_total_entry_20, clean_dat_total_sex_20, fit, fit_degree_20, fit_direct_age_20, 
     fit_direct_entry_20, clean_dat_eigcent_20, clean_dat_betweenness_20, clean_dat_deg_ver_20,
     fit_total_age_20, fit_total_entry_20, fit_total_sex_20, post_total_tree_type_20, 
     clean_dat_total_tree_type_20, fit_total_tree_type_20, fit_eigcent_20, fit_betweenness_20, fit_deg_ver_20,
     post_total_age_21, post_degree_21, post_direct_age_21, post_direct_entry_21, post_total_entry_21, 
     post_total_sex_21, post_eigcent_21, post_betweenness_21, post_deg_ver_21,
     clean_dat_total_age_21, clean_dat_degree_21, clean_dat_direct_age_21, clean_dat_direct_entry_21, 
     clean_dat_total_entry_21, clean_dat_total_sex_21, fit, fit_degree_21, fit_direct_age_21, 
     fit_direct_entry_21, clean_dat_eigcent_21, clean_dat_betweenness_21, clean_dat_deg_ver_21,
     fit_total_age_21, fit_total_entry_21, fit_total_sex_21, post_total_tree_type_21, 
     clean_dat_total_tree_type_21, fit_total_tree_type_21, fit_eigcent_21, fit_betweenness_21, fit_deg_ver_21,
     file = path_all_results_div)

message('All models saved.')
