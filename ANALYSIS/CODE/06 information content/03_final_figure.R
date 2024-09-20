# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 24-02-2023
# Date last modified: 30-03-2023
# Author: Simeon Q. Smeele
# Description: Plotting outcome of all models.  
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
load(path_all_results_info)

# Report sigma
message('Sigma ind for degree model 2020: ', round(mean(post_degree_20$sigma_ind_ID), 2))
message('Sigma ind for degree model 2021: ', round(mean(post_degree_21$sigma_ind_ID), 2))
message('Sigma ind for age model 2020: ', round(mean(post_age_total_20$sigma_ind_ID), 2))
message('Sigma ind for age model 2021: ', round(mean(post_age_total_21$sigma_ind_ID), 2))
message('Sigma ind for tree model 2020: ', round(mean(post_tree_total_20$sigma_ind_ID), 2))
message('Sigma ind for tree model 2021: ', round(mean(post_tree_total_21$sigma_ind_ID), 2))

## 2020 ##

# Plot
pdf(path_pdf_all_results_info_20, 8, 6)
par(mfrow = c(2, 3), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## degree
plot(mean(post_degree_20$b_degree), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_degree_20$b_degree), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree', 3, 1)
## tree
plot(mean(post_tree_total_20$b_tree), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_tree_total_20$b_tree), rep(1, 2))
points(mean(post_degree_20$b_tree), 2)
lines(PI(post_degree_20$b_tree), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('tree', 3, 1)
## age
plot(mean(post_age_total_20$b_age), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_age_total_20$b_age), rep(1, 2))
points(mean(post_degree_20$b_age), 2)
lines(PI(post_degree_20$b_age), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('age', 3, 1)
## scatter plots
## degree
plot(clean_dat_degree_20$degree + rnorm(length(clean_dat_degree_20$degree), sd = 0.02), 
     clean_dat_degree_20$n + rnorm(length(clean_dat_degree_20$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised degree', 1, 3, cex = 0.75)
degrees = seq(min(clean_dat_degree_20$degree), max(clean_dat_degree_20$degree), length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(degrees, function(degree) post_degree_20$a_bar[i] + 
               post_degree_20$b_degree[i] * degree, numeric(1))))
for(ind in 1:clean_dat_degree_20$N_ind) 
  points(clean_dat_degree_20$degree[clean_dat_degree_20$ind_ID == ind][1], 
         mean(clean_dat_degree_20$n[clean_dat_degree_20$ind_ID == ind]), col = 2, cex = 2, pch = 16)
for(i in 1:20) lines(degrees, pred_n[[i]])
## tree
plot(clean_dat_degree_20$tree_size + rnorm(length(clean_dat_degree_20$tree_size), sd = 0.02), 
     clean_dat_degree_20$n + rnorm(length(clean_dat_degree_20$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised tree size', 1, 3, cex = 0.75)
tree_sizes = seq(min(clean_dat_degree_20$tree_size), max(clean_dat_degree_20$tree_size), 
                 length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(tree_sizes, function(ts) 
    post_degree_20$a_bar[i] + post_degree_20$b_tree[i] * ts, numeric(1))))
for(i in 1:20) lines(tree_sizes, pred_n[[i]])
## age
plot(clean_dat_age_total_20$age + rnorm(length(clean_dat_age_total_20$age), sd = 0.02), 
     clean_dat_age_total_20$n + rnorm(length(clean_dat_age_total_20$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised age', 1, 3, cex = 0.75)
ages = seq(min(clean_dat_age_total_20$age), max(clean_dat_age_total_20$age), 
           length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(ages, function(age) 
    post_degree_20$a_bar[i] + post_degree_20$b_tree[i] * age, numeric(1))))
for(i in 1:20) lines(ages, pred_n[[i]])
dev.off()

## 2021 ##

# Plot
pdf(path_pdf_all_results_info_21, 8, 6)
par(mfrow = c(2, 3), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## degree
plot(mean(post_degree_21$b_degree), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_degree_21$b_degree), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree', 3, 1)
## tree
plot(mean(post_tree_total_21$b_tree), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_tree_total_21$b_tree), rep(1, 2))
points(mean(post_degree_21$b_tree), 2)
lines(PI(post_degree_21$b_tree), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('tree', 3, 1)
## age
plot(mean(post_age_total_21$b_age), 1,
     xlim = c(-0.4, 0.4), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_age_total_21$b_age), rep(1, 2))
points(mean(post_degree_21$b_age), 2)
lines(PI(post_degree_21$b_age), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('age', 3, 1)
## scatter plots
## degree
plot(clean_dat_degree_21$degree + rnorm(length(clean_dat_degree_21$degree), sd = 0.02), 
     clean_dat_degree_21$n + rnorm(length(clean_dat_degree_21$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised degree', 1, 3, cex = 0.75)
degrees = seq(min(clean_dat_degree_21$degree), max(clean_dat_degree_21$degree), length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(degrees, function(degree) post_degree_21$a_bar[i] + 
               post_degree_21$b_degree[i] * degree, numeric(1))))
for(ind in 1:clean_dat_degree_21$N_ind) 
  points(clean_dat_degree_21$degree[clean_dat_degree_21$ind_ID == ind][1], 
         mean(clean_dat_degree_21$n[clean_dat_degree_21$ind_ID == ind]), col = 2, cex = 2, pch = 16)
for(i in 1:20) lines(degrees, pred_n[[i]])
## tree
plot(clean_dat_degree_21$tree_size + rnorm(length(clean_dat_degree_21$tree_size), sd = 0.02), 
     clean_dat_degree_21$n + rnorm(length(clean_dat_degree_21$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised tree size', 1, 3, cex = 0.75)
tree_sizes = seq(min(clean_dat_degree_21$tree_size), max(clean_dat_degree_21$tree_size), 
                 length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(tree_sizes, function(ts) 
    post_degree_21$a_bar[i] + post_degree_21$b_tree[i] * ts, numeric(1))))
for(i in 1:20) lines(tree_sizes, pred_n[[i]])
## age
plot(clean_dat_age_total_21$age + rnorm(length(clean_dat_age_total_21$age), sd = 0.02), 
     clean_dat_age_total_21$n + rnorm(length(clean_dat_age_total_21$n), sd = 0.1), xlab = '', ylab = '')
mtext('standardised age', 1, 3, cex = 0.75)
ages = seq(min(clean_dat_age_total_21$age), max(clean_dat_age_total_21$age), 
           length.out = 100)
pred_n = lapply(1:20, function(i)  
  exp(vapply(ages, function(age) 
    post_degree_21$a_bar[i] + post_degree_21$b_tree[i] * age, numeric(1))))
for(i in 1:20) lines(ages, pred_n[[i]])
dev.off()
