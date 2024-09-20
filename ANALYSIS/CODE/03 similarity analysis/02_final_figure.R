# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 13-02-2023
# Date last modified: 06-04-2023
# Author: Simeon Q. Smeele
# Description: Figures with the similarity outcomes and 'raw' data. 
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
load(path_sim_models)

## 2020 ## ----

# Plot
pdf(path_pdf_similarity_models_20, 16, 5)
par(mfrow = c(2, 7), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## foraging
plot(mean(post_foraging_total_20$b_for), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('foraging', 3, 1)
lines(PI(post_foraging_total_20$b_for), rep(1, 2))
points(mean(post_foraging_direct_20$b_for), 2)
lines(PI(post_foraging_direct_20$b_for), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect')) 
## mate
plot(mean(post_mate_total_20$cont_mate), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_mate_total_20$cont_mate), rep(1, 2))
points(mean(post_foraging_direct_20$cont_mate), 2)
lines(PI(post_foraging_direct_20$cont_mate), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('mate', 3, 1)
## spat
plot(mean(post_spat_total_20$b_spat), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_spat_total_20$b_spat), rep(1, 2))
points(mean(post_foraging_total_20$b_spat), 2)
lines(PI(post_foraging_total_20$b_spat), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('spatial network', 3, 1)
## cluster
plot(mean(post_gen_direct_20$cont_clust), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_direct_20$cont_clust), rep(1, 2))
points(mean(post_foraging_direct_20$cont_clust), 2)
lines(PI(post_foraging_direct_20$cont_clust), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('same cluster', 3, 1)
## aggressive
plot(mean(post_ag_20$b_ag), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('aggressive', 3, 1)
lines(PI(post_ag_20$b_ag), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## tolerance
plot(mean(post_foraging_direct_20$b_tol), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('tolerance', 3, 1)
lines(PI(post_foraging_direct_20$b_tol), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## relatedness
plot(mean(post_gen_total_20$b_gen), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_total_20$b_gen), rep(1, 2))
points(mean(post_gen_direct_20$b_gen), 2)
lines(PI(post_gen_direct_20$b_gen), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('relatedness', 3, 1)
# ## location
# plot(mean(post_loc_total_20$cont_loc), 1,
#      xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
# lines(PI(post_loc_total_20$cont_loc), rep(1, 2))
# points(mean(post_loc_direct_20$cont_loc), 2)
# lines(PI(post_loc_direct_20$cont_loc), rep(2, 2))
# abline(v = 0, lty = 2)  
# axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
# mtext('same location', 3, 1)
## relatedness
## scatter plots
## foraging
plot(clean_dat_foraging_direct_20$for_edge_weight, clean_dat_foraging_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('standardised foraging edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
fors = seq(min(clean_dat_foraging_direct_20$for_edge_weight), 
           max(clean_dat_foraging_direct_20$for_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(fors, function(f) post_foraging_direct_20$a_bar[i] + post_foraging_direct_20$b_for[i] * f, 
         numeric(1)))
for(i in 1:16) lines(fors, pred_dists[[i]])
## mate
plot(clean_dat_foraging_direct_20$same_mate, clean_dat_foraging_direct_20$acc_dist, xlab = '', ylab = '', 
     xlim = c(1, 2))
mtext('same mate', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_20$a_bar[i] + 
      post_foraging_direct_20$z_mate[i,m] * post_foraging_direct_20$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## spat
plot(clean_dat_foraging_total_20$spat_edge_weight, clean_dat_foraging_total_20$acc_dist, xlab = '', ylab = '')
mtext('standardised spatial edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
spats = seq(min(clean_dat_foraging_total_20$spat_edge_weight), 
            max(clean_dat_foraging_total_20$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_foraging_total_20$a_bar[i] + 
           post_foraging_total_20$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]])
## clust
plot(clean_dat_foraging_direct_20$same_clust, clean_dat_foraging_direct_20$acc_dist, xlab = '', ylab = '')
mtext('same cluster', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_20$a_bar[i] + 
      post_foraging_direct_20$z_clust[i,m] * post_foraging_direct_20$sigma_clust[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## aggressive
plot(clean_dat_ag_20$ag_edge_weight, clean_dat_ag_20$acc_dist, xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
ags = seq(min(clean_dat_ag_20$ag_edge_weight), max(clean_dat_ag_20$ag_edge_weight), 
          length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(ags, function(ag) 
    post_ag_20$a_bar[i] + post_ag_20$b_ag[i] * ag, 
    numeric(1)))
for(i in 1:16) lines(ags, pred_dists[[i]])
## tolerance
plot(clean_dat_foraging_direct_20$tol_edge_weight, clean_dat_foraging_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_20$tol_edge_weight), 
           max(clean_dat_foraging_direct_20$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_20$a_bar[i] + post_foraging_direct_20$b_tol[i] * tol, 
    numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]])
## relatedness
plot(clean_dat_gen_direct_20$gen_edge_weight, clean_dat_gen_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
gens = seq(min(clean_dat_gen_direct_20$gen_edge_weight), 
           max(clean_dat_gen_direct_20$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_direct_20$a_bar[i] + post_gen_direct_20$b_gen[i] * gen, 
    numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]])
dev.off()

## 2021 ## ----

# Plot
pdf(path_pdf_similarity_models_21, 16, 5)
par(mfrow = c(2, 7), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## foraging
plot(mean(post_foraging_total_21$b_for), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('foraging', 3, 1)
lines(PI(post_foraging_total_21$b_for), rep(1, 2))
points(mean(post_foraging_direct_21$b_for), 2)
lines(PI(post_foraging_direct_21$b_for), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect')) 
## mate
plot(mean(post_mate_total_21$cont_mate), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_mate_total_21$cont_mate), rep(1, 2))
points(mean(post_foraging_direct_21$cont_mate), 2)
lines(PI(post_foraging_direct_21$cont_mate), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('mate', 3, 1)
## spat
plot(mean(post_spat_total_21$b_spat), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_spat_total_21$b_spat), rep(1, 2))
points(mean(post_foraging_total_21$b_spat), 2)
lines(PI(post_foraging_total_21$b_spat), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('spatial network', 3, 1)
## cluster
plot(mean(post_gen_direct_21$cont_clust), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_direct_21$cont_clust), rep(1, 2))
points(mean(post_foraging_direct_21$cont_clust), 2)
lines(PI(post_foraging_direct_21$cont_clust), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('same cluster', 3, 1)
## aggressive
plot(mean(post_ag_21$b_ag), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('aggressive', 3, 1)
lines(PI(post_ag_21$b_ag), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## tolerance
plot(mean(post_foraging_direct_21$b_tol), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('tolerance', 3, 1)
lines(PI(post_foraging_direct_21$b_tol), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## relatedness
plot(mean(post_gen_total_21$b_gen), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_total_21$b_gen), rep(1, 2))
points(mean(post_gen_direct_21$b_gen), 2)
lines(PI(post_gen_direct_21$b_gen), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('relatedness', 3, 1)
# ## location
# plot(mean(post_loc_total_21$cont_loc), 1,
#      xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
# lines(PI(post_loc_total_21$cont_loc), rep(1, 2))
# points(mean(post_loc_direct_21$cont_loc), 2)
# lines(PI(post_loc_direct_21$cont_loc), rep(2, 2))
# abline(v = 0, lty = 2)  
# axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
# mtext('same location', 3, 1)
## relatedness
## scatter plots
## foraging
plot(clean_dat_foraging_direct_21$for_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('standardised foraging edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
fors = seq(min(clean_dat_foraging_direct_21$for_edge_weight), 
           max(clean_dat_foraging_direct_21$for_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(fors, function(f) post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_for[i] * f, 
         numeric(1)))
for(i in 1:16) lines(fors, pred_dists[[i]])
## mate
plot(clean_dat_foraging_direct_21$same_mate, clean_dat_foraging_direct_21$acc_dist, xlab = '', ylab = '', 
     xlim = c(1, 2))
mtext('same mate', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_21$a_bar[i] + 
      post_foraging_direct_21$z_mate[i,m] * post_foraging_direct_21$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## spat
plot(clean_dat_foraging_total_21$spat_edge_weight, clean_dat_foraging_total_21$acc_dist, xlab = '', ylab = '')
mtext('standardised spatial edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
spats = seq(min(clean_dat_foraging_total_21$spat_edge_weight), 
            max(clean_dat_foraging_total_21$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_foraging_total_21$a_bar[i] + 
           post_foraging_total_21$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]])
## clust
plot(clean_dat_foraging_direct_21$same_clust, clean_dat_foraging_direct_21$acc_dist, xlab = '', ylab = '')
mtext('same cluster', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_21$a_bar[i] + 
      post_foraging_direct_21$z_clust[i,m] * post_foraging_direct_21$sigma_clust[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## aggressive
plot(clean_dat_ag_21$ag_edge_weight, clean_dat_ag_21$acc_dist, xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
ags = seq(min(clean_dat_ag_21$ag_edge_weight), max(clean_dat_ag_21$ag_edge_weight), 
          length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(ags, function(ag) 
    post_ag_21$a_bar[i] + post_ag_21$b_ag[i] * ag, 
    numeric(1)))
for(i in 1:16) lines(ags, pred_dists[[i]])
## tolerance
plot(clean_dat_foraging_direct_21$tol_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_21$tol_edge_weight), 
           max(clean_dat_foraging_direct_21$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_tol[i] * tol, 
    numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]])
## relatedness
plot(clean_dat_gen_direct_21$gen_edge_weight, clean_dat_gen_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
gens = seq(min(clean_dat_gen_direct_21$gen_edge_weight), 
           max(clean_dat_gen_direct_21$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_direct_21$a_bar[i] + post_gen_direct_21$b_gen[i] * gen, 
    numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]])
dev.off()

## Combined ## ----

pdf(path_pdf_similarity_main_figure, 4.5, 7)
layout(matrix(c(1, 2, 3,
                1, 4, 5,
                6, 7, 8,
                6, 9, 10), nrow = 4, ncol = 3, byrow = TRUE))
## 2020
## effects
## tolerance
par(mar = c(3.5, 3, 2.5, 0.5), oma = c(0, 0.5, 0, 0))
plot(mean(post_foraging_direct_20$b_tol), 1, col = '#C5CAE9', pch = 16,
     xlim = c(-1.5, 2.5), ylim = c(10.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
axis(1, c(-1, 0, 1, 2))
points(mean(post_foraging_direct_20$b_tol), 1, col = '#303F9F')
mtext('2020', 3, 1)
lines(PI(post_foraging_direct_20$b_tol), rep(1, 2), col = '#303F9F',)
abline(v = 0, lty = 2)  
abline(h = 2)
axis(2, 1, labels = 'total', las = 2)  
## mate
points(mean(post_mate_total_20$cont_mate), 3, col = '#B2DFDB', pch = 16)
points(mean(post_mate_total_20$cont_mate), 3, col = '#00796B')
lines(PI(post_mate_total_20$cont_mate), rep(3, 2), col = '#00796B')
points(mean(post_foraging_direct_20$cont_mate), 4, col = '#B2DFDB', pch = 16)
points(mean(post_foraging_direct_20$cont_mate), 4, col = '#00796B')
lines(PI(post_foraging_direct_20$cont_mate), rep(4, 2), col = '#00796B')
abline(h = 5)
axis(2, c(3, 4), labels = c('total', 'direct'), las = 2)  
## spat
points(mean(post_spat_total_20$b_spat), 6, col = '#FFCCBC', pch = 16)
points(mean(post_spat_total_20$b_spat), 6, col = '#E64A19')
lines(PI(post_spat_total_20$b_spat), rep(6, 2), col = '#E64A19')
points(mean(post_foraging_total_20$b_spat), 7, col = '#FFCCBC', pch = 16)
points(mean(post_foraging_total_20$b_spat), 7, col = '#E64A19')
lines(PI(post_foraging_total_20$b_spat), rep(7, 2), col = '#E64A19')
abline(h = 8)
axis(2, c(6, 7), labels = c('total', 'direct'), las = 2)  
## relatedness
points(mean(post_gen_total_20$b_gen), 9, col = '#D7CCC8', pch = 16)
points(mean(post_gen_total_20$b_gen), 9, col = '#5D4037')
lines(PI(post_gen_total_20$b_gen), rep(9, 2), col = '#5D4037')
points(mean(post_gen_direct_20$b_gen), 10, col = '#D7CCC8', pch = 16)
points(mean(post_gen_direct_20$b_gen), 10, col = '#5D4037')
lines(PI(post_gen_direct_20$b_gen), rep(10, 2), col = '#5D4037')
axis(2, c(9, 10), labels = c('total', 'direct'), las = 2)  
## scatter plots
## tolerance
plot(clean_dat_foraging_direct_20$tol_edge_weight, clean_dat_foraging_direct_20$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#C5CAE9', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('edge weight', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('tolerance', 3, 0.25, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_20$tol_edge_weight), 
           max(clean_dat_foraging_direct_20$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_20$a_bar[i] + post_foraging_direct_20$b_tol[i] * tol, numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]], col = '#303F9F', lwd = 2)
## mate
plot(clean_dat_mate_total_20$same_mate, clean_dat_mate_total_20$acc_dist, 
     col = '#B2DFDB', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(0.75, 2.25))
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_mate_total_20$a_bar[i] + post_mate_total_20$z_mate[i,m] * post_mate_total_20$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]], col = '#00796B', lwd = 2)
axis(1, c(1, 2), c('yes', 'no'))
axis(2, c(-2, 0, 2))
mtext('same mate', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('mate', 3, 0.25, cex = 0.75)
## spat
plot(clean_dat_spat_total_20$spat_edge_weight, clean_dat_spat_total_20$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#FFCCBC', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
spats = seq(min(clean_dat_spat_total_20$spat_edge_weight), max(clean_dat_spat_total_20$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_spat_total_20$a_bar[i] + post_spat_total_20$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]], col = '#E64A19', lwd = 2)
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('norm dist', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('spatial', 3, 0.25, cex = 0.75)
## relatedness
plot(clean_dat_gen_total_20$gen_edge_weight, clean_dat_gen_total_20$acc_dist, 
     col = '#D7CCC8', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(-0.1, 1.1))
gens = seq(min(clean_dat_gen_total_20$gen_edge_weight), max(clean_dat_gen_total_20$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_total_20$a_bar[i] + post_gen_total_20$b_gen[i] * gen, numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]], col = '#5D4037', lwd = 2)
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('edge weight', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('relatedness', 3, 0.25, cex = 0.75)
## 2021
## effects
## tolerance
plot(mean(post_foraging_direct_21$b_tol), 1, col = '#C5CAE9', pch = 16,
     xlim = c(-1.5, 2.5), ylim = c(10.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
axis(1, c(-1, 0, 1, 2))
points(mean(post_foraging_direct_21$b_tol), 1, col = '#303F9F')
mtext('2021', 3, 1)
lines(PI(post_foraging_direct_21$b_tol), rep(1, 2), col = '#303F9F',)
abline(v = 0, lty = 2)  
abline(h = 2)
axis(2, 1, labels = 'total', las = 2)  
## mate
points(mean(post_mate_total_21$cont_mate), 3, col = '#B2DFDB', pch = 16)
points(mean(post_mate_total_21$cont_mate), 3, col = '#00796B')
lines(PI(post_mate_total_21$cont_mate), rep(3, 2), col = '#00796B')
points(mean(post_foraging_direct_21$cont_mate), 4, col = '#B2DFDB', pch = 16)
points(mean(post_foraging_direct_21$cont_mate), 4, col = '#00796B')
lines(PI(post_foraging_direct_21$cont_mate), rep(4, 2), col = '#00796B')
abline(h = 5)
axis(2, c(3, 4), labels = c('total', 'direct'), las = 2)  
## spat
points(mean(post_spat_total_21$b_spat), 6, col = '#FFCCBC', pch = 16)
points(mean(post_spat_total_21$b_spat), 6, col = '#E64A19')
lines(PI(post_spat_total_21$b_spat), rep(6, 2), col = '#E64A19')
points(mean(post_foraging_total_21$b_spat), 7, col = '#FFCCBC', pch = 16)
points(mean(post_foraging_total_21$b_spat), 7, col = '#E64A19')
lines(PI(post_foraging_total_21$b_spat), rep(7, 2), col = '#E64A19')
abline(h = 8)
axis(2, c(6, 7), labels = c('total', 'direct'), las = 2)  
## relatedness
points(mean(post_gen_total_21$b_gen), 9, col = '#D7CCC8', pch = 16)
points(mean(post_gen_total_21$b_gen), 9, col = '#5D4037')
lines(PI(post_gen_total_21$b_gen), rep(9, 2), col = '#5D4037')
points(mean(post_gen_direct_21$b_gen), 10, col = '#D7CCC8', pch = 16)
points(mean(post_gen_direct_21$b_gen), 10, col = '#5D4037')
lines(PI(post_gen_direct_21$b_gen), rep(10, 2), col = '#5D4037')
axis(2, c(9, 10), labels = c('total', 'direct'), las = 2)  
## scatter plots
## tolerance
plot(clean_dat_foraging_direct_21$tol_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#C5CAE9', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('edge weight', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('tolerance', 3, 0.25, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_21$tol_edge_weight), 
           max(clean_dat_foraging_direct_21$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_tol[i] * tol, numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]], col = '#303F9F', lwd = 2)
## mate
plot(clean_dat_mate_total_21$same_mate, clean_dat_mate_total_21$acc_dist, 
     col = '#B2DFDB', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(0.75, 2.25))
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_mate_total_21$a_bar[i] + post_mate_total_21$z_mate[i,m] * post_mate_total_21$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]], col = '#00796B', lwd = 2)
axis(1, c(1, 2), c('yes', 'no'))
axis(2, c(-2, 0, 2))
mtext('same mate', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('mate', 3, 0.25, cex = 0.75)
## spat
plot(clean_dat_spat_total_21$spat_edge_weight, clean_dat_spat_total_21$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#FFCCBC', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
spats = seq(min(clean_dat_spat_total_21$spat_edge_weight), max(clean_dat_spat_total_21$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_spat_total_21$a_bar[i] + post_spat_total_21$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]], col = '#E64A19', lwd = 2)
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('norm dist', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('spatial', 3, 0.25, cex = 0.75)
## relatedness
plot(clean_dat_gen_total_21$gen_edge_weight, clean_dat_gen_total_21$acc_dist, 
     col = '#D7CCC8', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xlim = c(-0.1, 1.1))
gens = seq(min(clean_dat_gen_total_21$gen_edge_weight), max(clean_dat_gen_total_21$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_total_21$a_bar[i] + post_gen_total_21$b_gen[i] * gen, numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]], col = '#5D4037', lwd = 2)
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('edge weight', 1, 2.1, cex = 0.75)
mtext('acoustic dist', 2, 2.1, cex = 0.75)
mtext('relatedness', 3, 0.25, cex = 0.75)
dev.off()

## Reduced main figure ## ----

# Plot
pdf(path_pdf_similarity_models_20, 16, 5)
par(mfrow = c(2, 7), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## foraging
plot(mean(post_foraging_total_20$b_for), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('foraging', 3, 1)
lines(PI(post_foraging_total_20$b_for), rep(1, 2))
points(mean(post_foraging_direct_20$b_for), 2)
lines(PI(post_foraging_direct_20$b_for), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect')) 
## mate
plot(mean(post_mate_total_20$cont_mate), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_mate_total_20$cont_mate), rep(1, 2))
points(mean(post_foraging_direct_20$cont_mate), 2)
lines(PI(post_foraging_direct_20$cont_mate), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('mate', 3, 1)
## spat
plot(mean(post_spat_total_20$b_spat), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_spat_total_20$b_spat), rep(1, 2))
points(mean(post_foraging_total_20$b_spat), 2)
lines(PI(post_foraging_total_20$b_spat), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('spatial network', 3, 1)
## cluster
plot(mean(post_gen_direct_20$cont_clust), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_direct_20$cont_clust), rep(1, 2))
points(mean(post_foraging_direct_20$cont_clust), 2)
lines(PI(post_foraging_direct_20$cont_clust), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('same cluster', 3, 1)
## aggressive
plot(mean(post_ag_20$b_ag), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('aggressive', 3, 1)
lines(PI(post_ag_20$b_ag), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## tolerance
plot(mean(post_foraging_direct_20$b_tol), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('tolerance', 3, 1)
lines(PI(post_foraging_direct_20$b_tol), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## relatedness
plot(mean(post_gen_total_20$b_gen), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_total_20$b_gen), rep(1, 2))
points(mean(post_gen_direct_20$b_gen), 2)
lines(PI(post_gen_direct_20$b_gen), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('relatedness', 3, 1)
# ## location
# plot(mean(post_loc_total_20$cont_loc), 1,
#      xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
# lines(PI(post_loc_total_20$cont_loc), rep(1, 2))
# points(mean(post_loc_direct_20$cont_loc), 2)
# lines(PI(post_loc_direct_20$cont_loc), rep(2, 2))
# abline(v = 0, lty = 2)  
# axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
# mtext('same location', 3, 1)
## relatedness
## scatter plots
## foraging
plot(clean_dat_foraging_direct_20$for_edge_weight, clean_dat_foraging_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('standardised foraging edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
fors = seq(min(clean_dat_foraging_direct_20$for_edge_weight), 
           max(clean_dat_foraging_direct_20$for_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(fors, function(f) post_foraging_direct_20$a_bar[i] + post_foraging_direct_20$b_for[i] * f, 
         numeric(1)))
for(i in 1:16) lines(fors, pred_dists[[i]])
## mate
plot(clean_dat_foraging_direct_20$same_mate, clean_dat_foraging_direct_20$acc_dist, xlab = '', ylab = '', 
     xlim = c(1, 2))
mtext('same mate', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_20$a_bar[i] + 
      post_foraging_direct_20$z_mate[i,m] * post_foraging_direct_20$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## spat
plot(clean_dat_foraging_total_20$spat_edge_weight, clean_dat_foraging_total_20$acc_dist, xlab = '', ylab = '')
mtext('standardised spatial edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
spats = seq(min(clean_dat_foraging_total_20$spat_edge_weight), 
            max(clean_dat_foraging_total_20$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_foraging_total_20$a_bar[i] + 
           post_foraging_total_20$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]])
## clust
plot(clean_dat_foraging_direct_20$same_clust, clean_dat_foraging_direct_20$acc_dist, xlab = '', ylab = '')
mtext('same cluster', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_20$a_bar[i] + 
      post_foraging_direct_20$z_clust[i,m] * post_foraging_direct_20$sigma_clust[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## aggressive
plot(clean_dat_ag_20$ag_edge_weight, clean_dat_ag_20$acc_dist, xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
ags = seq(min(clean_dat_ag_20$ag_edge_weight), max(clean_dat_ag_20$ag_edge_weight), 
          length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(ags, function(ag) 
    post_ag_20$a_bar[i] + post_ag_20$b_ag[i] * ag, 
    numeric(1)))
for(i in 1:16) lines(ags, pred_dists[[i]])
## tolerance
plot(clean_dat_foraging_direct_20$tol_edge_weight, clean_dat_foraging_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_20$tol_edge_weight), 
           max(clean_dat_foraging_direct_20$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_20$a_bar[i] + post_foraging_direct_20$b_tol[i] * tol, 
    numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]])
## relatedness
plot(clean_dat_gen_direct_20$gen_edge_weight, clean_dat_gen_direct_20$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
gens = seq(min(clean_dat_gen_direct_20$gen_edge_weight), 
           max(clean_dat_gen_direct_20$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_direct_20$a_bar[i] + post_gen_direct_20$b_gen[i] * gen, 
    numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]])
dev.off()

## 2021 ## ----

# Plot
pdf(path_pdf_similarity_models_21, 16, 5)
par(mfrow = c(2, 7), oma = c(3, 2, 3, 1), mar = c(3, 3, 1, 1))
## model estimates
## foraging
plot(mean(post_foraging_total_21$b_for), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
mtext('foraging', 3, 1)
lines(PI(post_foraging_total_21$b_for), rep(1, 2))
points(mean(post_foraging_direct_21$b_for), 2)
lines(PI(post_foraging_direct_21$b_for), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect')) 
## mate
plot(mean(post_mate_total_21$cont_mate), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_mate_total_21$cont_mate), rep(1, 2))
points(mean(post_foraging_direct_21$cont_mate), 2)
lines(PI(post_foraging_direct_21$cont_mate), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('mate', 3, 1)
## spat
plot(mean(post_spat_total_21$b_spat), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_spat_total_21$b_spat), rep(1, 2))
points(mean(post_foraging_total_21$b_spat), 2)
lines(PI(post_foraging_total_21$b_spat), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('spatial network', 3, 1)
## cluster
plot(mean(post_gen_direct_21$cont_clust), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_direct_21$cont_clust), rep(1, 2))
points(mean(post_foraging_direct_21$cont_clust), 2)
lines(PI(post_foraging_direct_21$cont_clust), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('same cluster', 3, 1)
## aggressive
plot(mean(post_ag_21$b_ag), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('aggressive', 3, 1)
lines(PI(post_ag_21$b_ag), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## tolerance
plot(mean(post_foraging_direct_21$b_tol), 1,
     xlim = c(-1, 2), ylim = c(0.5, 1.5), xlab = '', yaxt = 'n', ylab = '')
mtext('tolerance', 3, 1)
lines(PI(post_foraging_direct_21$b_tol), rep(1, 2))
abline(v = 0, lty = 2)  
axis(2, c(1), labels = c('effect')) 
## relatedness
plot(mean(post_gen_total_21$b_gen), 1,
     xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
lines(PI(post_gen_total_21$b_gen), rep(1, 2))
points(mean(post_gen_direct_21$b_gen), 2)
lines(PI(post_gen_direct_21$b_gen), rep(2, 2))
abline(v = 0, lty = 2)  
axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
mtext('relatedness', 3, 1)
# ## location
# plot(mean(post_loc_total_21$cont_loc), 1,
#      xlim = c(-1, 2), ylim = c(0.5, 2.5), xlab = '', yaxt = 'n', ylab = '')
# lines(PI(post_loc_total_21$cont_loc), rep(1, 2))
# points(mean(post_loc_direct_21$cont_loc), 2)
# lines(PI(post_loc_direct_21$cont_loc), rep(2, 2))
# abline(v = 0, lty = 2)  
# axis(2, c(1, 2), labels = c('total effect', 'direct effect'))  
# mtext('same location', 3, 1)
## relatedness
## scatter plots
## foraging
plot(clean_dat_foraging_direct_21$for_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('standardised foraging edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
fors = seq(min(clean_dat_foraging_direct_21$for_edge_weight), 
           max(clean_dat_foraging_direct_21$for_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(fors, function(f) post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_for[i] * f, 
         numeric(1)))
for(i in 1:16) lines(fors, pred_dists[[i]])
## mate
plot(clean_dat_foraging_direct_21$same_mate, clean_dat_foraging_direct_21$acc_dist, xlab = '', ylab = '', 
     xlim = c(1, 2))
mtext('same mate', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_21$a_bar[i] + 
      post_foraging_direct_21$z_mate[i,m] * post_foraging_direct_21$sigma_mate[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## spat
plot(clean_dat_foraging_total_21$spat_edge_weight, clean_dat_foraging_total_21$acc_dist, xlab = '', ylab = '')
mtext('standardised spatial edge weight', 1, 3, cex = 0.75)
mtext('acoustic distance', 2, 3, cex = 0.75)
spats = seq(min(clean_dat_foraging_total_21$spat_edge_weight), 
            max(clean_dat_foraging_total_21$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_foraging_total_21$a_bar[i] + 
           post_foraging_total_21$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]])
## clust
plot(clean_dat_foraging_direct_21$same_clust, clean_dat_foraging_direct_21$acc_dist, xlab = '', ylab = '')
mtext('same cluster', 1, 3, cex = 0.75)
pred_dists = lapply(1:16, function(i)  
  vapply(1:2, function(m) 
    post_foraging_direct_21$a_bar[i] + 
      post_foraging_direct_21$z_clust[i,m] * post_foraging_direct_21$sigma_clust[i], 
    numeric(1)))
for(i in 1:16) lines(1:2, pred_dists[[i]])
## aggressive
plot(clean_dat_ag_21$ag_edge_weight, clean_dat_ag_21$acc_dist, xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
ags = seq(min(clean_dat_ag_21$ag_edge_weight), max(clean_dat_ag_21$ag_edge_weight), 
          length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(ags, function(ag) 
    post_ag_21$a_bar[i] + post_ag_21$b_ag[i] * ag, 
    numeric(1)))
for(i in 1:16) lines(ags, pred_dists[[i]])
## tolerance
plot(clean_dat_foraging_direct_21$tol_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_21$tol_edge_weight), 
           max(clean_dat_foraging_direct_21$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_tol[i] * tol, 
    numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]])
## relatedness
plot(clean_dat_gen_direct_21$gen_edge_weight, clean_dat_gen_direct_21$acc_dist, 
     xlab = '', ylab = '')
mtext('edge weight', 1, 3, cex = 0.75)
gens = seq(min(clean_dat_gen_direct_21$gen_edge_weight), 
           max(clean_dat_gen_direct_21$gen_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(gens, function(gen) 
    post_gen_direct_21$a_bar[i] + post_gen_direct_21$b_gen[i] * gen, 
    numeric(1)))
for(i in 1:16) lines(gens, pred_dists[[i]])
dev.off()

## Combined ## ----

pdf(path_pdf_similarity_main_figure_reduced, 5, 1.5)
par(mfrow = c(1, 3), mar = c(3.5, 3, 0.5, 0.5), oma = c(0, 0.5, 0, 0))
## effect plot spatial 2020
plot(NULL, xlim = c(-1.5, 2.5), ylim = c(4.5, 0.5), xlab = '', xaxt = 'n', yaxt = 'n', ylab = '')
abline(v = 0, lty = 2)  
axis(1, c(-1, 0, 1, 2))
points(mean(post_spat_total_20$b_spat), 1, col = '#FFCCBC', pch = 16)
points(mean(post_spat_total_20$b_spat), 1, col = '#E64A19')
lines(PI(post_spat_total_20$b_spat), rep(1, 2), col = '#E64A19')
points(mean(post_foraging_total_20$b_spat), 2, col = '#FFCCBC', pch = 16)
points(mean(post_foraging_total_20$b_spat), 2, col = '#E64A19')
lines(PI(post_foraging_total_20$b_spat), rep(2, 2), col = '#E64A19')
axis(2, c(1, 2), labels = c('total', 'direct'), las = 2)  
abline(h = 3)
text(2.15, 0.9, 'a)', font = 2)
## effect plot tolerance 2021
points(mean(post_foraging_direct_21$b_tol), 4, col = '#C5CAE9', pch = 16)
points(mean(post_foraging_direct_21$b_tol), 4, col = '#303F9F')
lines(PI(post_foraging_direct_21$b_tol), rep(4, 2), col = '#303F9F',)
axis(2, 4, labels = 'total', las = 2)  
mtext(expression(beta), 1, 2.1, cex = 0.75)
# mtext(expression(beta ~ 'estimates'), 3, 0.25, cex = 0.75)
## scatter spatial 2020
plot(clean_dat_spat_total_20$spat_edge_weight, clean_dat_spat_total_20$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#FFCCBC', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
spats = seq(min(clean_dat_spat_total_20$spat_edge_weight), max(clean_dat_spat_total_20$spat_edge_weight), 
            length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(spats, function(s) post_spat_total_20$a_bar[i] + post_spat_total_20$b_spat[i] * s, numeric(1)))
for(i in 1:16) lines(spats, pred_dists[[i]], col = '#E64A19', lwd = 2)
axis(1, c(0, 0.5, 1))
axis(2, c(-2, 0, 2))
mtext('normalised distance', 1, 2.1, cex = 0.75)
mtext('acoustic distance', 2, 2.1, cex = 0.75)
# mtext('spatial 2020', 3, 0.25, cex = 0.75)
text(1, 2.55, 'b)', font = 2)
## scatter tolerance 2021
plot(clean_dat_foraging_direct_21$tol_edge_weight, clean_dat_foraging_direct_21$acc_dist, 
     xlim = c(-0.1, 1.1), col = '#C5CAE9', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(1, c(0, 0.5, 1))
axis(2, c(-3, 0, 3))
mtext('edge weight', 1, 2.1, cex = 0.75)
mtext('acoustic distance', 2, 2.1, cex = 0.75)
# mtext('tolerance 2021', 3, 0.25, cex = 0.75)
tols = seq(min(clean_dat_foraging_direct_21$tol_edge_weight), 
           max(clean_dat_foraging_direct_21$tol_edge_weight), 
           length.out = 100)
pred_dists = lapply(1:16, function(i)  
  vapply(tols, function(tol) 
    post_foraging_direct_21$a_bar[i] + post_foraging_direct_21$b_tol[i] * tol, numeric(1)))
for(i in 1:16) lines(tols, pred_dists[[i]], col = '#303F9F', lwd = 2)
text(1, 3.61, 'c)', font = 2)
dev.off()

message('All plots done.')