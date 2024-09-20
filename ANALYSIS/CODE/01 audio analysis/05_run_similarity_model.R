# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 04-11-2022
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Model to measure similarity between dyads. 
# This version removes within recordings and recording pair. 
# This version also includes the 2020 data. 
# source('ANALYSIS/CODE/01 audio analysis/05_run_similarity_model.R')
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

# Settings
set.seed(1)
N_sub = 15 # max number of calls per ind to include

# Load data
load(path_data)
load(path_dtw_m)

## 2020 ##

# Adjust st to m
rownames(st_20) = st_20$fs
st_20 = st_20[rownames(m_20),]

# Prep data
ind_id = sapply(rownames(m_20), function(fs) st_20$bird[st_20$fs == fs])
## subset calls
keep = sapply(unique(ind_id), function(id){
  w = which(st_20$bird == id)
  if(length(w) < N_sub) return(w) else return(sample(w, N_sub))
}) %>% unlist
ind_id = ind_id[keep]
m_20 = m_20[keep, keep]
rec_id = sapply(rownames(m_20), function(fs) st_20$file[st_20$fs == fs])
call_id = 1:length(ind_id)
l = length(call_id) # number of calls
c = combn(1:l, 2) # all combinations
c = c[,ind_id[c[1,]] != ind_id[c[2,]]] # remove within ID
c = c[,rec_id[c[1,]] != rec_id[c[2,]]] # remove within rec
ind_pair = as.integer(as.factor(sapply(1:ncol(c), function(x)
  paste(sort(c(ind_id[c[1,x]], ind_id[c[2,x]])), collapse = ' '))))
same_rec = ifelse(rec_id[c[1,]] == rec_id[c[2,]], 1, 2)
rec_pair =  as.integer(as.factor(sapply(1:ncol(c), function(x)
  paste(sort(c(rec_id[c[1,x]], rec_id[c[2,x]])), collapse = ' '))))
ind_pair_char = sapply(1:ncol(c), function(x)
  paste(sort(c(ind_id[c[1,x]], ind_id[c[2,x]])), collapse = ' '))
dat = list(acc_dist = as.numeric(scale(sapply(1:ncol(c), function(i) m_20[c[1,i], c[2,i]]))),
           ind_pair = ind_pair,
           # same_rec = as.numeric(same_rec),
           rec_pair = rec_pair,
           # call_i = c[1,],
           # call_j = c[2,],
           N_ind_pair = max(ind_pair),
           N_rec_pair = max(rec_pair),
           # N_call = max(unlist(c)),
           N_obs = ncol(c))
print(str(dat))

# Run model
model = cmdstan_model(path_m_sim)
fit = model$sample(data = dat,
                   seed = 1,
                   chains = 4,
                   parallel_chains = 4,
                   refresh = 200,
                   adapt_delta = 0.99,
                   max_treedepth = 15)
print(fit$summary())
fit$output_files() %>%
  rstan::read_stan_csv() %>%
  rethinking::extract.samples() -> post
post_20 = post

# Plot diagnostics
# traceplot(fit$output_files() |> rstan::read_stan_csv())
# trankplot(fit$output_files() |> rstan::read_stan_csv())

# Summarise per dyad
similarity_out = data.frame(index = ind_pair, char = ind_pair_char) %>% unique
similarity_out$mean_a_ind_pair = sapply(similarity_out$index, function(i){
  mean(sapply(1:nrow(post$z_ind_pair), function(j) post$z_ind_pair[j,i] * post$sigma_ind_pair[j]))
})
similarity_out$sd_a_ind_pair = sapply(similarity_out$index, function(i){
  sd(sapply(1:nrow(post$z_ind_pair), function(j) post$z_ind_pair[j,i] * post$sigma_ind_pair[j]))
})
plot(similarity_out$mean_a_ind_pair)
for(i in 1:nrow(similarity_out))
  lines(rep(i, 2), similarity_out$mean_a_ind_pair[i] + c(-2, 2) * similarity_out$sd_a_ind_pair[i])
similarity_in_dat_20 = dat
similarity_out_20 = similarity_out

## 2021 ##

# Adjust st to m
rownames(st_21) = st_21$fs
st_21 = st_21[rownames(m_21),]

# Prep data
ind_id = sapply(rownames(m_21), function(fs) st_21$bird[st_21$fs == fs])
## subset calls
keep = sapply(unique(ind_id), function(id){
  w = which(st_21$bird == id)
  if(length(w) < N_sub) return(w) else return(sample(w, N_sub))
}) %>% unlist
ind_id = ind_id[keep]
m_21 = m_21[keep, keep]
rec_id = sapply(rownames(m_21), function(fs) st_21$file[st_21$fs == fs])
call_id = 1:length(ind_id)
l = length(call_id) # number of calls
c = combn(1:l, 2) # all combinations 
c = c[,ind_id[c[1,]] != ind_id[c[2,]]] # remove within ID
c = c[,rec_id[c[1,]] != rec_id[c[2,]]] # remove within rec
ind_pair = as.integer(as.factor(sapply(1:ncol(c), function(x) 
  paste(sort(c(ind_id[c[1,x]], ind_id[c[2,x]])), collapse = ' '))))
same_rec = ifelse(rec_id[c[1,]] == rec_id[c[2,]], 1, 2)
rec_pair =  as.integer(as.factor(sapply(1:ncol(c), function(x) 
  paste(sort(c(rec_id[c[1,x]], rec_id[c[2,x]])), collapse = ' '))))
ind_pair_char = sapply(1:ncol(c), function(x) 
  paste(sort(c(ind_id[c[1,x]], ind_id[c[2,x]])), collapse = ' '))
dat = list(acc_dist = as.numeric(scale(sapply(1:ncol(c), function(i) m_21[c[1,i], c[2,i]]))),
           ind_pair = ind_pair,
           # same_rec = as.numeric(same_rec),
           rec_pair = rec_pair,
           # call_i = c[1,],
           # call_j = c[2,],
           N_ind_pair = max(ind_pair),
           N_rec_pair = max(rec_pair),
           # N_call = max(unlist(c)),
           N_obs = ncol(c))
print(str(dat))

# Run model
model = cmdstan_model(path_m_sim)
fit = model$sample(data = dat, 
                   seed = 1, 
                   chains = 4, 
                   parallel_chains = 4,
                   refresh = 200, 
                   adapt_delta = 0.99,
                   max_treedepth = 15)
print(fit$summary())
fit$output_files() %>%
  rstan::read_stan_csv() %>%
  rethinking::extract.samples() -> post
post_21 = post

# Plot diagnostics
# traceplot(fit$output_files() |> rstan::read_stan_csv())
# trankplot(fit$output_files() |> rstan::read_stan_csv())

# Summarise per dyad
similarity_out = data.frame(index = ind_pair, char = ind_pair_char) %>% unique
similarity_out$mean_a_ind_pair = sapply(similarity_out$index, function(i){
  mean(sapply(1:nrow(post$z_ind_pair), function(j) post$z_ind_pair[j,i] * post$sigma_ind_pair[j]))
})
similarity_out$sd_a_ind_pair = sapply(similarity_out$index, function(i){
  sd(sapply(1:nrow(post$z_ind_pair), function(j) post$z_ind_pair[j,i] * post$sigma_ind_pair[j]))
})
plot(similarity_out$mean_a_ind_pair)
for(i in 1:nrow(similarity_out)) 
  lines(rep(i, 2), similarity_out$mean_a_ind_pair[i] + c(-2, 2) * similarity_out$sd_a_ind_pair[i])
similarity_in_dat_21 = dat
similarity_out_21 = similarity_out

## Save ##
save(similarity_out_20, similarity_out_21, similarity_in_dat_20, similarity_in_dat_21,
     file = path_similarity_out)
save(post_20, post_21, file = path_post_similarity)
