# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 01-03-2023
# Date last modified: 06-03-2023
# Author: Simeon Q. Smeele
# Description: Plotting outcome of all models for entropy.  
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
load(path_all_results_rep_ent)

## 2020 ---- 

# Plot
pdf(path_pdf_all_results_rep_ent_20, 15, 5)
par(mfrow = c(2, 8), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## age
plot(mean(post_age_total_20$b), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('age', 3, 1)
lines(PI(post_age_total_20$b), rep(1, 2))
points(mean(post_age_direct_20$b_age), 2)
lines(PI(post_age_direct_20$b_age), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
## degree
plot(mean(post_degree_20$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_degree_20$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree', 3, 1)
## eigenvector cent
plot(mean(post_eigcent_direct_20$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_eigcent_direct_20$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('eigenvector centrality', 3, 1)
## betweenness
plot(mean(post_betweenness_direct_20$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_betweenness_direct_20$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('betweenness', 3, 1)
## degree versatility
plot(mean(post_deg_ver_direct_20$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_deg_ver_direct_20$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree versatility', 3, 1)
## sex
cont_total = post_sex_total_20$a_sex[,1] - post_sex_total_20$a_sex[,2]
cont_direct = post_degree_20$a_sex[,1] - post_degree_20$a_sex[,2]
plot(mean(cont_total), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(cont_total), rep(1, 2))
points(mean(cont_direct), 2)
lines(PI(cont_direct), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('sex', 3, 1)
## entry
plot(mean(post_entry_total_20$b_entry), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_entry_total_20$b_entry), rep(1, 2))
points(mean(post_age_direct_20$b_entry), 2)
lines(PI(post_age_direct_20$b_entry), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('entry', 3, 1)
## tree
plot(mean(post_age_direct_20$b_tree), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_age_direct_20$b_tree), rep(1, 2))
points(mean(post_degree_20$b_tree), 2)
lines(PI(post_degree_20$b_tree), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('tree', 3, 1)
## scatter plots
## age
plot(clean_dat_age_direct_20$age, clean_dat_age_direct_20$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised age', 1, 3, cex = 0.75)
mtext('repertoire entropy', 2, 3, cex = 0.75)
ages = seq(min(clean_dat_age_direct_20$age), max(clean_dat_age_direct_20$age), length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(ages, function(age) post_age_direct_20$a_bar[i] + post_age_direct_20$b_age[i] * age, numeric(1)))
for(i in 1:16) lines(ages, pred_ents[[i]])
## degree
plot(clean_dat_degree_20$netpos, clean_dat_degree_20$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised degree', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_degree_20$netpos), max(clean_dat_degree_20$netpos), length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_degree_20$a_bar[i] + post_degree_20$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## eigenvector
plot(clean_dat_eigcent_direct_20$netpos, clean_dat_eigcent_direct_20$entropy, xlab = '', ylab = '', 
     ylim = c(-1, 3))
mtext('standardised eigenvector', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_eigcent_direct_20$netpos), max(clean_dat_eigcent_direct_20$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) 
    post_eigcent_direct_20$a_bar[i] + post_eigcent_direct_20$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## betweenness
plot(clean_dat_betweenness_direct_20$netpos, clean_dat_betweenness_direct_20$entropy, 
     xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised betweenness', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_betweenness_direct_20$netpos), max(clean_dat_betweenness_direct_20$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_betweenness_direct_20$a_bar[i] + 
           post_betweenness_direct_20$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## degree versatility
plot(clean_dat_deg_ver_direct_20$netpos, clean_dat_deg_ver_direct_20$entropy, xlab = '', ylab = '', 
     ylim = c(-1, 3))
mtext('standardised versatility', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_deg_ver_direct_20$netpos), max(clean_dat_deg_ver_direct_20$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_deg_ver_direct_20$a_bar[i] + 
           post_deg_ver_direct_20$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## sex
plot(clean_dat_degree_20$sex, clean_dat_degree_20$entropy, xlab = '', ylab = '', xaxt = 'n', ylim = c(-1, 3), 
     xlim = c(0.5, 2.5))
axis(1, c(1, 2), c('female', 'male'))
mtext('sex', 1, 3, cex = 0.75)
pred_ents = lapply(1:16, function(i)  
  vapply(1:2, function(sex) 
    post_degree_20$a_bar[i] + post_degree_20$z_sex[i,sex] * post_degree_20$sigma_sex[i], numeric(1)))
for(i in 1:16) lines(1:2, pred_ents[[i]])
## entry
plot(clean_dat_age_direct_20$entry_size, clean_dat_age_direct_20$entropy, 
     xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised entry size', 1, 3, cex = 0.75)
entry_sizes = seq(min(clean_dat_age_direct_20$entry_size), max(clean_dat_age_direct_20$entry_size), 
                  length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(entry_sizes, function(es) 
    post_age_direct_20$a_bar[i] + post_age_direct_20$b_entry[i] * es, numeric(1)))
for(i in 1:16) lines(entry_sizes, pred_ents[[i]])
## tree
plot(clean_dat_degree_20$tree_size, clean_dat_degree_20$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised tree size', 1, 3, cex = 0.75)
tree_sizes = seq(min(clean_dat_degree_20$tree_size), max(clean_dat_degree_20$tree_size), 
                 length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(tree_sizes, function(ts) 
    post_degree_20$a_bar[i] + post_degree_20$b_tree[i] * ts, numeric(1)))
for(i in 1:16) lines(tree_sizes, pred_ents[[i]])
dev.off()

## 2021 ---- 

# Plot
pdf(path_pdf_all_results_rep_ent_21, 15, 5)
par(mfrow = c(2, 8), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## age
plot(mean(post_age_total_21$b), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('age', 3, 1)
lines(PI(post_age_total_21$b), rep(1, 2))
points(mean(post_age_direct_21$b_age), 2)
lines(PI(post_age_direct_21$b_age), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
## degree
plot(mean(post_degree_21$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_degree_21$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree', 3, 1)
## eigenvector cent
plot(mean(post_eigcent_direct_21$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_eigcent_direct_21$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('eigenvector centrality', 3, 1)
## betweenness
plot(mean(post_betweenness_direct_21$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_betweenness_direct_21$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('betweenness', 3, 1)
## degree versatility
plot(mean(post_deg_ver_direct_21$b_netpos), 1,
     xlim = c(-1, 1), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_deg_ver_direct_21$b_netpos), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect'))  
mtext('degree versatility', 3, 1)
## sex
cont_total = post_sex_total_21$a_sex[,1] - post_sex_total_21$a_sex[,2]
cont_direct = post_degree_21$a_sex[,1] - post_degree_21$a_sex[,2]
plot(mean(cont_total), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(cont_total), rep(1, 2))
points(mean(cont_direct), 2)
lines(PI(cont_direct), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('sex', 3, 1)
## entry
plot(mean(post_entry_total_21$b_entry), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_entry_total_21$b_entry), rep(1, 2))
points(mean(post_age_direct_21$b_entry), 2)
lines(PI(post_age_direct_21$b_entry), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('entry', 3, 1)
## tree
plot(mean(post_age_direct_21$b_tree), 1,
     xlim = c(-1, 1), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_age_direct_21$b_tree), rep(1, 2))
points(mean(post_degree_21$b_tree), 2)
lines(PI(post_degree_21$b_tree), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('tree', 3, 1)
## scatter plots
## age
plot(clean_dat_age_direct_21$age, clean_dat_age_direct_21$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised age', 1, 3, cex = 0.75)
mtext('repertoire entropy', 2, 3, cex = 0.75)
ages = seq(min(clean_dat_age_direct_21$age), max(clean_dat_age_direct_21$age), length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(ages, function(age) post_age_direct_21$a_bar[i] + post_age_direct_21$b_age[i] * age, numeric(1)))
for(i in 1:16) lines(ages, pred_ents[[i]])
## degree
plot(clean_dat_degree_21$netpos, clean_dat_degree_21$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised degree', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_degree_21$netpos), max(clean_dat_degree_21$netpos), length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_degree_21$a_bar[i] + post_degree_21$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## eigenvector
plot(clean_dat_eigcent_direct_21$netpos, clean_dat_eigcent_direct_21$entropy, xlab = '', ylab = '', 
     ylim = c(-1, 3))
mtext('standardised eigenvector', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_eigcent_direct_21$netpos), max(clean_dat_eigcent_direct_21$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) 
    post_eigcent_direct_21$a_bar[i] + post_eigcent_direct_21$b_netpos[i] * netposs, 
    numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## betweenness
plot(clean_dat_betweenness_direct_21$netpos, clean_dat_betweenness_direct_21$entropy, 
     xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised betweenness', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_betweenness_direct_21$netpos), max(clean_dat_betweenness_direct_21$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_betweenness_direct_21$a_bar[i] + 
           post_betweenness_direct_21$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## degree versatility
plot(clean_dat_deg_ver_direct_21$netpos, clean_dat_deg_ver_direct_21$entropy, xlab = '', ylab = '', 
     ylim = c(-1, 3))
mtext('standardised versatility', 1, 3, cex = 0.75)
netposs = seq(min(clean_dat_deg_ver_direct_21$netpos), max(clean_dat_deg_ver_direct_21$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(netposs) post_deg_ver_direct_21$a_bar[i] + 
           post_deg_ver_direct_21$b_netpos[i] * netposs, 
         numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]])
## sex
plot(clean_dat_degree_21$sex, clean_dat_degree_21$entropy, xlab = '', ylab = '', xaxt = 'n', ylim = c(-1, 3), 
     xlim = c(0.5, 2.5))
axis(1, c(1, 2), c('female', 'male'))
mtext('sex', 1, 3, cex = 0.75)
pred_ents = lapply(1:16, function(i)  
  vapply(1:2, function(sex) 
    post_degree_21$a_bar[i] + post_degree_21$z_sex[i,sex] * post_degree_21$sigma_sex[i], numeric(1)))
for(i in 1:16) lines(1:2, pred_ents[[i]])
## entry
plot(clean_dat_age_direct_21$entry_size, clean_dat_age_direct_21$entropy, 
     xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised entry size', 1, 3, cex = 0.75)
entry_sizes = seq(min(clean_dat_age_direct_21$entry_size), max(clean_dat_age_direct_21$entry_size), 
                  length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(entry_sizes, function(es) 
    post_age_direct_21$a_bar[i] + post_age_direct_21$b_entry[i] * es, numeric(1)))
for(i in 1:16) lines(entry_sizes, pred_ents[[i]])
## tree
plot(clean_dat_degree_21$tree_size, clean_dat_degree_21$entropy, xlab = '', ylab = '', ylim = c(-1, 3))
mtext('standardised tree size', 1, 3, cex = 0.75)
tree_sizes = seq(min(clean_dat_degree_21$tree_size), max(clean_dat_degree_21$tree_size), 
                 length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(tree_sizes, function(ts) 
    post_degree_21$a_bar[i] + post_degree_21$b_tree[i] * ts, numeric(1)))
for(i in 1:16) lines(tree_sizes, pred_ents[[i]])
dev.off()

## Combined figure ----
pdf(path_pdf_repertoire_main_figure, 4.5, 7)
layout(matrix(c(1, 2, 3,
                1, 4, 5,
                6, 7, 8,
                6, 9, 10), nrow = 4, ncol = 3, byrow = TRUE))
## 2020
par(mar = c(3.5, 3, 2.5, 0.5), oma = c(0, 0.5, 0, 0))
## effects
## degree
plot(mean(post_deg_ver_direct_20$b_netpos), 1, col = '#303F9F', 
     xlim = c(-1, 1.5), ylim = c(10.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
axis(1, c(-1, 0, 1), c(-1, 0, 1))
points(mean(post_deg_ver_direct_20$b_netpos), 1, col = alpha('#303F9F', 0.5), pch = 16)
mtext('2020', 3, 1)
lines(PI(post_deg_ver_direct_20$b_netpos), rep(1, 2), col = '#303F9F',)
abline(v = 0, lty = 2)  
abline(h = 2)
axis(2, 1, labels = 'total', las = 2)  
## sex
points(mean(post_sex_total_20$cont_sex), 3, col = '#00796B')
points(mean(post_sex_total_20$cont_sex), 3, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_sex_total_20$cont_sex), rep(3, 2), col = '#00796B')
points(mean(post_degree_20$cont_sex), 4, col = '#00796B')
points(mean(post_degree_20$cont_sex), 4, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_degree_20$cont_sex), rep(4, 2), col = '#00796B')
abline(h = 5)
axis(2, c(3, 4), labels = c('total', 'direct'), las = 2)  
## age
points(mean(post_age_total_20$b), 6, col = '#E64A19')
points(mean(post_age_total_20$b), 6, col = alpha('#E64A19', 0.5), pch = 16)
lines(PI(post_age_total_20$b), rep(6, 2), col = '#E64A19')
points(mean(post_age_direct_20$b_age), 7, col = '#E64A19')
points(mean(post_age_direct_20$b_age), 7, col = alpha('#E64A19', 0.5), pch = 16)
lines(PI(post_age_direct_20$b_age), rep(7, 2), col = '#E64A19')
abline(h = 8)
axis(2, c(6, 7), labels = c('total', 'direct'), las = 2)  
## tree
points(mean(post_age_direct_20$b_tree), 9, col = '#5D4037')
points(mean(post_age_direct_20$b_tree), 9, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_age_direct_20$b_tree), rep(9, 2), col = '#5D4037')
points(mean(post_degree_20$b_tree), 10, col = '#5D4037')
points(mean(post_degree_20$b_tree), 10, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_degree_20$b_tree), rep(10, 2), col = '#5D4037')
axis(2, c(9, 10), labels = c('total', 'direct'), las = 2)  
## scatter plots
## versatility
plot(clean_dat_deg_ver_direct_20$netpos, clean_dat_deg_ver_direct_20$entropy, 
     ylim = c(0, 2.5), col = '#C5CAE9', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand vers', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('versatility', 3, 0.25, cex = 0.75)
netposs = seq(min(clean_dat_deg_ver_direct_20$netpos), max(clean_dat_deg_ver_direct_20$netpos), 
           length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(np) 
    post_deg_ver_direct_20$a_bar[i] + post_deg_ver_direct_20$b_netpos[i] * np, numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]], col = '#303F9F', lwd = 2)
## sex
plot(clean_dat_degree_20$sex, clean_dat_degree_20$entropy, 
     col = '#B2DFDB', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 2.5), xlim = c(0.75, 2.25))
pred_ents = lapply(1:16, function(i)  
  vapply(1:2, function(s) 
    post_degree_20$a_bar[i] + post_degree_20$z_sex[i,s] * post_degree_20$sigma_sex[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_ents[[i]], col = '#00796B', lwd = 2)
axis(1, c(1, 2), c('female', 'male'))
axis(2, c(0, 1, 2))
mtext('sex', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('sex', 3, 0.25, cex = 0.75)
## age
plot(clean_dat_age_direct_20$age, clean_dat_age_direct_20$entropy, 
     ylim = c(0, 2.5), col = '#FFCCBC', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
ages = seq(min(clean_dat_age_direct_20$age), max(clean_dat_age_direct_20$age), 
            length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(ages, function(age) post_age_direct_20$a_bar[i] + post_age_direct_20$b_age[i] * age, numeric(1)))
for(i in 1:16) lines(ages, pred_ents[[i]], col = '#E64A19', lwd = 2)
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand age', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('age', 3, 0.25, cex = 0.75)
## tree
plot(clean_dat_degree_20$tree_size, clean_dat_degree_20$entropy, 
     col = '#D7CCC8', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 2.5))
tree_sizes = seq(min(clean_dat_degree_20$tree_size), max(clean_dat_degree_20$tree_size), 
                 length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(tree_sizes, function(ts) 
    post_degree_20$a_bar[i] + post_degree_20$b_tree[i] * ts, numeric(1)))
for(i in 1:16) lines(tree_sizes, pred_ents[[i]], col = '#5D4037', lwd = 2)
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand size', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('tree', 3, 0.25, cex = 0.75)
## 2021
## effects
## degree
plot(mean(post_deg_ver_direct_21$b_netpos), 1, col = '#303F9F', 
     xlim = c(-1, 1.5), ylim = c(10.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
axis(1, c(-1, 0, 1), c(-1, 0, 1))
points(mean(post_deg_ver_direct_21$b_netpos), 1, col = alpha('#303F9F', 0.5), pch = 16)
mtext('2021', 3, 1)
lines(PI(post_deg_ver_direct_21$b_netpos), rep(1, 2), col = '#303F9F',)
abline(v = 0, lty = 2)  
abline(h = 2)
axis(2, 1, labels = 'total', las = 2)  
## sex
points(mean(post_sex_total_21$cont_sex), 3, col = '#00796B')
points(mean(post_sex_total_21$cont_sex), 3, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_sex_total_21$cont_sex), rep(3, 2), col = '#00796B')
points(mean(post_degree_21$cont_sex), 4, col = '#00796B')
points(mean(post_degree_21$cont_sex), 4, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_degree_21$cont_sex), rep(4, 2), col = '#00796B')
abline(h = 5)
axis(2, c(3, 4), labels = c('total', 'direct'), las = 2)  
## age
points(mean(post_age_total_21$b), 6, col = '#E64A19')
points(mean(post_age_total_21$b), 6, col = alpha('#E64A19', 0.5), pch = 16)
lines(PI(post_age_total_21$b), rep(6, 2), col = '#E64A19')
points(mean(post_age_direct_21$b_age), 7, col = '#E64A19')
points(mean(post_age_direct_21$b_age), 7, col = alpha('#E64A19', 0.5), pch = 16)
lines(PI(post_age_direct_21$b_age), rep(7, 2), col = '#E64A19')
abline(h = 8)
axis(2, c(6, 7), labels = c('total', 'direct'), las = 2)  
## tree
points(mean(post_age_direct_21$b_tree), 9, col = '#5D4037')
points(mean(post_age_direct_21$b_tree), 9, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_age_direct_21$b_tree), rep(9, 2), col = '#5D4037')
points(mean(post_degree_21$b_tree), 10, col = '#5D4037')
points(mean(post_degree_21$b_tree), 10, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_degree_21$b_tree), rep(10, 2), col = '#5D4037')
axis(2, c(9, 10), labels = c('total', 'direct'), las = 2)  
## scatter plots
## versatility
plot(clean_dat_deg_ver_direct_21$netpos, clean_dat_deg_ver_direct_21$entropy, 
     ylim = c(0, 2.5), col = '#C5CAE9', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand vers', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('versatility', 3, 0.25, cex = 0.75)
netposs = seq(min(clean_dat_deg_ver_direct_21$netpos), max(clean_dat_deg_ver_direct_21$netpos), 
              length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(netposs, function(np) 
    post_deg_ver_direct_21$a_bar[i] + post_deg_ver_direct_21$b_netpos[i] * np, numeric(1)))
for(i in 1:16) lines(netposs, pred_ents[[i]], col = '#303F9F', lwd = 2)
## sex
plot(clean_dat_degree_21$sex, clean_dat_degree_21$entropy, 
     col = '#B2DFDB', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(0.75, 2.25), ylim = c(0, 2.5))
pred_ents = lapply(1:16, function(i)  
  vapply(1:2, function(s) 
    post_degree_21$a_bar[i] + post_degree_21$z_sex[i,s] * post_degree_21$sigma_sex[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_ents[[i]], col = '#00796B', lwd = 2)
axis(1, c(1, 2), c('female', 'male'))
axis(2, c(0, 1, 2))
mtext('sex', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('sex', 3, 0.25, cex = 0.75)
## age
plot(clean_dat_age_direct_21$age, clean_dat_age_direct_21$entropy, 
     ylim = c(0, 2.5), col = '#FFCCBC', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
ages = seq(min(clean_dat_age_direct_21$age), max(clean_dat_age_direct_21$age), 
           length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(ages, function(age) post_age_direct_21$a_bar[i] + post_age_direct_21$b_age[i] * age, numeric(1)))
for(i in 1:16) lines(ages, pred_ents[[i]], col = '#E64A19', lwd = 2)
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand age', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('age', 3, 0.25, cex = 0.75)
## tree
plot(clean_dat_degree_21$tree_size, clean_dat_degree_21$entropy, 
     col = '#D7CCC8', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 2.5))
tree_sizes = seq(min(clean_dat_degree_21$tree_size), max(clean_dat_degree_21$tree_size), 
                 length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(tree_sizes, function(ts) 
    post_degree_21$a_bar[i] + post_degree_21$b_tree[i] * ts, numeric(1)))
for(i in 1:16) lines(tree_sizes, pred_ents[[i]], col = '#5D4037', lwd = 2)
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('stand size', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
mtext('tree', 3, 0.25, cex = 0.75)
dev.off()

## Combined ## ----

pdf(path_pdf_repertoire_main_figure_reduced, 5, 1.5)
par(mfrow = c(1, 3), mar = c(3.5, 3, 0.5, 0.5), oma = c(0, 0.5, 0, 0))
## effect plot sex 2020
plot(NULL, xlim = c(-1.5, 2.5), ylim = c(5.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
axis(1, c(-1, 0, 1, 2))
abline(v = 0, lty = 2)  
points(mean(post_sex_total_20$cont_sex), 1, col = '#00796B')
points(mean(post_sex_total_20$cont_sex), 1, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_sex_total_20$cont_sex), rep(1, 2), col = '#00796B')
points(mean(post_degree_20$cont_sex), 2, col = '#00796B')
points(mean(post_degree_20$cont_sex), 2, col = alpha('#00796B', 0.5), pch = 16)
lines(PI(post_degree_20$cont_sex), rep(2, 2), col = '#00796B')
abline(h = 3)
axis(2, c(1, 2), labels = c('total', 'direct'), las = 2)  
## effect plot tree 2021
points(mean(post_age_direct_21$b_tree), 4, col = '#5D4037')
points(mean(post_age_direct_21$b_tree), 4, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_age_direct_21$b_tree), rep(4, 2), col = '#5D4037')
points(mean(post_degree_21$b_tree), 5, col = '#5D4037')
points(mean(post_degree_21$b_tree), 5, col = alpha('#5D4037', 0.5), pch = 16)
lines(PI(post_degree_21$b_tree), rep(5, 2), col = '#5D4037')
axis(2, c(4, 5), labels = c('total', 'direct'), las = 2)  
mtext(expression(beta), 1, 2.1, cex = 0.75)
text(2.2, 0.9, 'a)', font = 2)
# mtext(expression(beta ~ 'estimates'), 3, 0.25, cex = 0.75)
## scatter sex 2020
plot(clean_dat_degree_20$sex, clean_dat_degree_20$entropy, 
     col = '#B2DFDB', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 2.5), xlim = c(0.75, 2.25))
pred_ents = lapply(1:16, function(i)  
  vapply(1:2, function(s) 
    post_degree_20$a_bar[i] + post_degree_20$z_sex[i,s] * post_degree_20$sigma_sex[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_ents[[i]], col = '#00796B', lwd = 2)
axis(1, c(1, 2), c('female', 'male'))
axis(2, c(0, 1, 2))
mtext('sex', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
text(2.12, 2.25, 'b)', font = 2)
## scatter tree 2021
plot(clean_dat_degree_21$tree_size, clean_dat_degree_21$entropy, 
     col = '#D7CCC8', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 2.5))
tree_sizes = seq(min(clean_dat_degree_21$tree_size), max(clean_dat_degree_21$tree_size), 
                 length.out = 100)
pred_ents = lapply(1:16, function(i)  
  vapply(tree_sizes, function(ts) 
    post_degree_21$a_bar[i] + post_degree_21$b_tree[i] * ts, numeric(1)))
for(i in 1:16) lines(tree_sizes, pred_ents[[i]], col = '#5D4037', lwd = 2)
axis(1, c(-1, 0, 1))
axis(2, c(0, 1, 2))
mtext('standardised size', 1, 2.1, cex = 0.75)
mtext('entropy', 2, 2.1, cex = 0.75)
text(1.4, 2.25, 'c)', font = 2)
dev.off()

message('All plots done.')