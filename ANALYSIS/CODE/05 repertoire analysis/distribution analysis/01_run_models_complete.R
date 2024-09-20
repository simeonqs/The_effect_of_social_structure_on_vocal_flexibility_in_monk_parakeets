# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 25-05-2023
# Date last modified: 23-06-2023
# Author: Simeon Q. Smeele
# Description: Running the distribution models on real data. This version includes all models with correct
# covariates. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Start ----

# Loading libraries
libraries = c('cmdstanr', 'rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Define own function to make simplex
make.simplex = function(v) v/sum(v)

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
load(path_master_dat_rep_dists)
source(path_call_type_classification)

## 2020 ## ----
rownames(master_dat_20) = master_dat_20$ind
X_20 = X_20[!rownames(X_20) %in% c('other_tonal', 'frill'),]
print(rowSums(X_20))
master_dat_20$sex = ifelse(master_dat_20$sex == 'F', 'FEMALE', master_dat_20$sex)

# Age total ----

# Prepare data for model
inds = colnames(X_20)
inds = inds[inds %in% master_dat_20$ind[!is.na(master_dat_20$age)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_20),
           X = X_20[,inds],
           age = (master_dat_20[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_age_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    
                   max_treedepth = 15,
                   refresh = 200)
stan_obj_age = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_age, depth = 2))
post_age_total_2020 = extract.samples(stan_obj_age)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/age total - 2020.pdf')
par(mfrow = c(2, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(0, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(apply(post_age_total_2020$A_bar, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_total_2020$A_bar, 2, PI)[,i], col = 2)

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_total_2020$B_age, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_total_2020$B_age, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_20))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$age)), function(age)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_total_2020$lp__)), function(j)
        exp(log(post_age_total_2020$A_bar[j,i]) + post_age_total_2020$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_total_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$age, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$age)), p_post[i,], col = 2)
}
dev.off()

dat_age_total_2020 = dat

# Age direct ----

# Prepare data for model
inds = colnames(X_20)
inds = inds[inds %in% master_dat_20$ind[!is.na(master_dat_20$age) &
                                          !is.na(master_dat_20$entry_count) &
                                          !is.na(master_dat_20$degree) &
                                          !is.na(master_dat_20$sex) &
                                          !is.na(master_dat_20$tree_count)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_20),
           X = X_20[,inds],
           age = (master_dat_20[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           entry_size = master_dat_20[inds,]$entry_count |> as.numeric() |> scale() |> as.numeric(),
           net_pos = master_dat_20[inds,]$degree |> as.numeric() |> scale() |> as.numeric(),
           tree_size = master_dat_20[inds,]$tree_count |> as.numeric() |> scale() |> as.numeric(),
           sex = c(FEMALE = 1, M = 2)[master_dat_20[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_age_direct.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    
                   max_treedepth = 15,
                   refresh = 200)
stan_obj_age_direct = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_age_direct, depth = 2))
post_age_direct_2020 = extract.samples(stan_obj_age_direct)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/age direct - 2020.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_direct_2020$B_age, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_direct_2020$B_age, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_20))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$age)), function(age)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$age, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$age)), p_post[i,], col = 2)
}
dev.off()

dat_age_direct_2020 = dat

# Versatility ----

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/versatility - 2020.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_direct_2020$B_net_pos, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_direct_2020$B_net_pos, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_20))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$net_pos)), function(np)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$net_pos, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$net_pos)), p_post[i,], col = 2)
}
dev.off()

# Entry total ----

# Prepare data for model
inds = colnames(X_20)
inds = inds[inds %in% master_dat_20$ind[!is.na(master_dat_20$age) &
                                          !is.na(master_dat_20$entry_count) &
                                          !is.na(master_dat_20$sex)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_20),
           X = X_20[,inds],
           age = (master_dat_20[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           entry_size = master_dat_20[inds,]$entry_count |> as.numeric() |> scale() |> as.numeric(),
           sex = c(FEMALE = 1, M = 2)[master_dat_20[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_entry_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_entry_total = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_entry_total, depth = 2))
post_entry_total_2020 = extract.samples(stan_obj_entry_total)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/entry total - 2020.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_entry_total_2020$B_entry, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_entry_total_2020$B_entry, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_20))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$entry_size)), function(es)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_entry_total_2020$lp__)), function(j)
        exp(log(mean(post_entry_total_2020$A_bar[j,,i])) + post_entry_total_2020$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_entry_total_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$entry_size, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$entry_size)), p_post[i,], col = 2)
}
dev.off()

dat_entry_total_2020 = dat

# Sex total ----

# Prepare data for model
inds = colnames(X_20)
inds = inds[inds %in% master_dat_20$ind[!is.na(master_dat_20$sex)]]

dat = list(N_ind = length(inds),
           N_cat = nrow(X_20),
           X = X_20[,inds],
           sex = c(FEMALE = 1, M = 2)[master_dat_20[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_sex_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_sex_total = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_sex_total, depth = 2))
post_sex_total_2020 = extract.samples(stan_obj_sex_total)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/sex total - 2020.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-0.5, 0.5), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_sex_total_2020$cont_sex, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_sex_total_2020$cont_sex, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_20))

par(mfrow = c(3, 3))
for(i in seq_len(dat$N_cat)){
  plot(dat$sex + rnorm(dat$N_ind, 0, 0.05), dat$X[i,]/colSums(dat$X), 
       ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
  axis(1, 1:2, c('female', 'male'))
  points(apply(post_sex_total_2020$A_bar[,,i], 2, mean), col = 2, cex = 1.5, pch = 16)
  for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2020$A_bar[,j,i]), col = 2)
}
dev.off()

dat_sex_total_2020 = dat

## 2021 ## ----
rownames(master_dat_21) = master_dat_21$ind
X_21 = X_21[!rownames(X_21) %in% c('other_tonal', 'frill'),]
print(rowSums(X_21))
master_dat_21$sex = ifelse(master_dat_21$sex == 'F', 'FEMALE', master_dat_21$sex)

# Age total ----

# Prepare data for model
inds = colnames(X_21)
inds = inds[inds %in% master_dat_21$ind[!is.na(master_dat_21$age)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_21),
           X = X_21[,inds],
           age = (master_dat_21[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_age_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_age = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_age, depth = 2))
post_age_total_2021 = extract.samples(stan_obj_age)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/age total - 2021.pdf')
par(mfrow = c(2, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(0, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(apply(post_age_total_2021$A_bar, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_total_2021$A_bar, 2, PI)[,i], col = 2)

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_total_2021$B_age, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_total_2021$B_age, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_21))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$age)), function(age)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_total_2021$lp__)), function(j)
        exp(log(post_age_total_2021$A_bar[j,i]) + post_age_total_2021$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_total_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$age, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$age)), p_post[i,], col = 2)
}
dev.off()

dat_age_total_2021 = dat

# Age direct ----

# Prepare data for model
inds = colnames(X_21)
inds = inds[inds %in% master_dat_21$ind[!is.na(master_dat_21$age) &
                                          !is.na(master_dat_21$entry_count) &
                                          !is.na(master_dat_21$degree) &
                                          !is.na(master_dat_21$sex) &
                                          !is.na(master_dat_21$tree_count)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_21),
           X = X_21[,inds],
           age = (master_dat_21[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           entry_size = master_dat_21[inds,]$entry_count |> as.numeric() |> scale() |> as.numeric(),
           net_pos = master_dat_21[inds,]$degree |> as.numeric() |> scale() |> as.numeric(),
           tree_size = master_dat_21[inds,]$tree_count |> as.numeric() |> scale() |> as.numeric(),
           sex = c(FEMALE = 1, M = 2)[master_dat_21[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_age_direct.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_age_direct = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_age_direct, depth = 2))
post_age_direct_2021 = extract.samples(stan_obj_age_direct)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/age direct - 2021.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_direct_2021$B_age, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_direct_2021$B_age, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_21))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$age)), function(age)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$age, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$age)), p_post[i,], col = 2)
}
dev.off()

dat_age_direct_2021 = dat

# Versatility ----

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/versatility - 2021.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_age_direct_2021$B_net_pos, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_age_direct_2021$B_net_pos, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_21))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$net_pos)), function(np)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$net_pos, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$net_pos)), p_post[i,], col = 2)
}
dev.off()

# Entry total ----

# Prepare data for model
inds = colnames(X_21)
inds = inds[inds %in% master_dat_21$ind[!is.na(master_dat_21$age) &
                                          !is.na(master_dat_21$entry_count) &
                                          !is.na(master_dat_21$sex)]]
dat = list(N_ind = length(inds),
           N_cat = nrow(X_21),
           X = X_21[,inds],
           age = (master_dat_21[inds,]$age |> as.numeric() / 365) |> scale() |> as.numeric(),
           entry_size = master_dat_21[inds,]$entry_count |> as.numeric() |> scale() |> as.numeric(),
           sex = c(FEMALE = 1, M = 2)[master_dat_21[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_entry_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_entry_total = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_entry_total, depth = 2))
post_entry_total_2021 = extract.samples(stan_obj_entry_total)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/entry total - 2021.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-1, 1), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_entry_total_2021$B_entry, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_entry_total_2021$B_entry, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_21))

par(mfrow = c(3, 3))
p_post = 
  vapply(unique(sort(dat$entry_size)), function(es)
    vapply(seq_len(dat$N_cat), function(i) 
      vapply(seq_len(length(post_entry_total_2021$lp__)), function(j)
        exp(log(mean(post_entry_total_2021$A_bar[j,,i])) + post_entry_total_2021$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_entry_total_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat$N_cat))

for(i in seq_len(dat$N_cat)){
  plot(dat$entry_size, dat$X[i,]/colSums(dat$X), ylim = c(0, 1))
  lines(unique(sort(dat$entry_size)), p_post[i,], col = 2)
}
dev.off()

dat_entry_total_2021 = dat

# Sex total ----

# Prepare data for model
inds = colnames(X_21)
inds = inds[inds %in% master_dat_21$ind[!is.na(master_dat_21$sex)]]

dat = list(N_ind = length(inds),
           N_cat = nrow(X_21),
           X = X_21[,inds],
           sex = c(FEMALE = 1, M = 2)[master_dat_21[inds,]$sex] |> as.integer(),
           alpha = c(5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

# Run model
model = cmdstan_model('ANALYSIS/CODE/06 repertoire analysis/distribution analysis/m_sex_total.stan')
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   adapt_delta = 0.99,                    max_treedepth = 15,
                   refresh = 200)
stan_obj_sex_total = fit$output_files() |> rstan::read_stan_csv()
print(precis(stan_obj_sex_total, depth = 2))
post_sex_total_2021 = extract.samples(stan_obj_sex_total)

# Plot
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/sex total - 2021.pdf')
par(mfrow = c(1, 1), oma = c(5, 3, 1, 1), mar = c(1, 1, 1, 1))

plot(NULL, ylim = c(-0.5, 0.5), xlim = c(1, dat$N_cat), pch = 16, xaxt = 'n')
points(seq_len(dat$N_cat), apply(post_sex_total_2021$cont_sex, 2, mean), col = 2)
for(i in seq_len(dat$N_cat)) lines(rep(i, 2), apply(post_sex_total_2021$cont_sex, 2, PI)[,i], col = 2)
abline(h = 0, lty = 2)
axis(1, 1:dat$N_cat, rownames(X_21))

for(i in seq_len(dat$N_cat)){
  plot(dat$sex + rnorm(dat$N_ind, 0, 0.05), dat$X[i,]/colSums(dat$X), 
       ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
  axis(1, 1:2, c('female', 'male'))
  points(apply(post_sex_total_2021$A_bar[,,i], 2, mean), col = 2, cex = 1.5, pch = 16)
  for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2021$A_bar[,j,i]), col = 2)
}
dev.off()

dat_sex_total_2021 = dat

# Save output
save(post_age_direct_2020, post_age_direct_2021, post_age_total_2020, post_age_total_2021,
     post_entry_total_2020, post_entry_total_2021, post_sex_total_2020, post_sex_total_2021,
     dat_age_direct_2020, dat_age_direct_2021, dat_age_total_2020, dat_age_total_2021,
     dat_entry_total_2020, dat_entry_total_2021, dat_sex_total_2020, dat_sex_total_2021,
     file = 'ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/model_results.RData')
