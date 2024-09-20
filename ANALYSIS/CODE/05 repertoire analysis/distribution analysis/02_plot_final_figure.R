# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: complexity paper
# Date started: 23-06-2023
# Date last modified: 23-06-2023
# Author: Simeon Q. Smeele
# Description: Plotting the results of the distribution analysis. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Start ----

# Loading libraries
libraries = c('rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list=ls()) 

# Paths
source('ANALYSIS/CODE/paths.R')

# Set seed
set.seed(1)

# Define own function to make simplex
make.simplex = function(v) v/sum(v)

# Import functions
.functions = sapply(list.files(path_functions, pattern = '*R', full.names = T), source)

# Load data
load('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/model_results.RData')

# 2020 ----

# Open PDF
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/final figure distributions - 2020.pdf',
    7, 5)
par(mfrow = c(2, 4), mar = c(5, 1, 1, 1), oma = c(1, 6, 3, 1))

# Plot results age
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_age, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_age, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(2, 1:dat_age_direct_2020$N_cat, rownames(dat_age_direct_2020$X), las = 2)
mtext('age', 3, 1, font = 2)

# Plot results versatility
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_net_pos, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_net_pos, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('versatility', 3, 1, font = 2)

# Plot results entry size
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_entry, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_entry, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('chamber size', 3, 1, font = 2)

# Plot results entry size
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_sex_total_2020$cont_sex, 2, mean), seq_len(dat_sex_total_2020$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2020$N_cat)) 
  lines(apply(post_sex_total_2020$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('sex', 3, 1, font = 2)

# Scatter age
p_post = 
  vapply(unique(sort(dat_age_direct_2020$age)), function(age)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$age, dat_age_direct_2020$X[7,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled age', col = 'grey')
mtext('proportion repertoire', 2, 3, cex = 0.66)
lines(unique(sort(dat_age_direct_2020$age)), p_post[7,], lwd = 2)

# Scatter versatility
p_post = 
  vapply(unique(sort(dat_age_direct_2020$net_pos)), function(np)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$net_pos, dat_age_direct_2020$X[1,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled versatility', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2020$net_pos)), p_post[1,], lwd = 2)

# Scatter entry size
p_post = 
  vapply(unique(sort(dat_age_direct_2020$entry_size)), function(es)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$entry_size, dat_age_direct_2020$X[1,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled chamber size', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2020$entry_size)), p_post[1,], lwd = 2)

# Scatter sex
plot(dat_sex_total_2020$sex + rnorm(dat_sex_total_2020$N_ind, 0, 0.05), 
     dat_sex_total_2020$X[1,]/colSums(dat_sex_total_2020$X), yaxt = 'n', col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
axis(1, 1:2, c('female', 'male'))
points(apply(post_sex_total_2020$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2020$A_bar[,j,1]), lwd = 2)

# Close PDF
dev.off()

# 2021 ----

# Open PDF
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/final figure distributions - 2021.pdf',
    7, 5)
par(mfrow = c(2, 4), mar = c(5, 1, 1, 1), oma = c(1, 6, 3, 1))

# Plot results age
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_age, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_age, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(2, 1:dat_age_direct_2021$N_cat, rownames(dat_age_direct_2021$X), las = 2)
mtext('age', 3, 1, font = 2)

# Plot results versatility
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_net_pos, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_net_pos, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('versatility', 3, 1, font = 2)

# Plot results entry size
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_entry, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_entry, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('chamber size', 3, 1, font = 2)

# Plot results entry size
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_sex_total_2021$cont_sex, 2, mean), seq_len(dat_sex_total_2021$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2021$N_cat)) 
  lines(apply(post_sex_total_2021$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('sex', 3, 1, font = 2)

# Scatter age
p_post = 
  vapply(unique(sort(dat_age_direct_2021$age)), function(age)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$age, dat_age_direct_2021$X[5,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled age', col = 'grey')
mtext('proportion repertoire', 2, 3, cex = 0.66)
lines(unique(sort(dat_age_direct_2021$age)), p_post[5,], lwd = 2)

# Scatter versatility
p_post = 
  vapply(unique(sort(dat_age_direct_2021$net_pos)), function(np)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$net_pos, dat_age_direct_2021$X[1,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled versatility', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2021$net_pos)), p_post[1,], lwd = 2)

# Scatter entry size
p_post = 
  vapply(unique(sort(dat_age_direct_2021$entry_size)), function(es)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$entry_size, dat_age_direct_2021$X[1,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled chamber size', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2021$entry_size)), p_post[1,], lwd = 2)

# Scatter sex
plot(dat_sex_total_2021$sex + rnorm(dat_sex_total_2021$N_ind, 0, 0.05), 
     dat_sex_total_2021$X[1,]/colSums(dat_sex_total_2021$X), yaxt = 'n', col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
axis(1, 1:2, c('female', 'male'))
points(apply(post_sex_total_2021$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2021$A_bar[,j,1]), lwd = 2)

# Close PDF
dev.off()


# Combined ----

# 2020

# Open PDF
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/final figure distributions - combined.pdf',
    7, 10)
par(mfrow = c(4, 4), mar = c(5, 1, 1, 1), oma = c(1, 6, 3, 1))

# Plot results age
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_age, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_age, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(2, 1:dat_age_direct_2020$N_cat, rownames(dat_age_direct_2020$X), las = 2)
mtext('age', 3, 1, font = 2)
text(min(dat_age_direct_2020$age) - 1.5, 10.5, '2020', xpd = NA, font = 2, cex = 2)

# Plot results versatility
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_net_pos, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_net_pos, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('versatility', 3, 1, font = 2)

# Plot results entry size
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2020$B_entry, 2, mean), seq_len(dat_age_direct_2020$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2020$N_cat)) 
  lines(apply(post_age_direct_2020$B_entry, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('chamber size', 3, 1, font = 2)

# Plot results sex
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2020$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_sex_total_2020$cont_sex, 2, mean), seq_len(dat_sex_total_2020$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2020$N_cat)) 
  lines(apply(post_sex_total_2020$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
mtext('sex', 3, 1, font = 2)

# Scatter age
p_post = 
  vapply(unique(sort(dat_age_direct_2020$age)), function(age)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$age, dat_age_direct_2020$X[7,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled age', col = 'grey')
mtext('proportion repertoire', 2, 3, cex = 0.66)
lines(unique(sort(dat_age_direct_2020$age)), p_post[7,], lwd = 2)

# Scatter versatility
p_post = 
  vapply(unique(sort(dat_age_direct_2020$net_pos)), function(np)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$net_pos, dat_age_direct_2020$X[1,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled versatility', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2020$net_pos)), p_post[1,], lwd = 2)

# Scatter entry size
p_post = 
  vapply(unique(sort(dat_age_direct_2020$entry_size)), function(es)
    vapply(seq_len(dat_age_direct_2020$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2020$lp__)), function(j)
        exp(log(mean(post_age_direct_2020$A_bar[j,,i])) + post_age_direct_2020$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_age_direct_2020$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2020$N_cat))
plot(dat_age_direct_2020$entry_size, dat_age_direct_2020$X[1,]/colSums(dat_age_direct_2020$X), ylim = c(0, 1),
     xlab = 'scaled chamber size', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2020$entry_size)), p_post[1,], lwd = 2)

# Scatter sex
plot(dat_sex_total_2020$sex + rnorm(dat_sex_total_2020$N_ind, 0, 0.05), 
     dat_sex_total_2020$X[1,]/colSums(dat_sex_total_2020$X), yaxt = 'n', col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
axis(1, 1:2, c('female', 'male'))
points(apply(post_sex_total_2020$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2020$A_bar[,j,1]), lwd = 2)

# 2021

# Plot results age
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_age, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_age, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(2, 1:dat_age_direct_2021$N_cat, rownames(dat_age_direct_2021$X), las = 2)
text(min(dat_age_direct_2020$age) - 1.5, 10.5, '2021', xpd = NA, font = 2, cex = 2)

# Plot results versatility
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_net_pos, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_net_pos, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)

# Plot results entry size
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(1, dat_age_direct_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_age_direct_2021$B_entry, 2, mean), seq_len(dat_age_direct_2021$N_cat), col = 1)
for(i in seq_len(dat_age_direct_2021$N_cat)) 
  lines(apply(post_age_direct_2021$B_entry, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)

# Plot results sex
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2021$N_cat), pch = 16, yaxt = 'n',
     main = '', ylab = '', xlab = 'effect')
points(apply(post_sex_total_2021$cont_sex, 2, mean), seq_len(dat_sex_total_2021$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2021$N_cat)) 
  lines(apply(post_sex_total_2021$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)

# Scatter age
p_post = 
  vapply(unique(sort(dat_age_direct_2021$age)), function(age)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_age[j,i] * age), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$age, dat_age_direct_2021$X[5,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled age', col = 'grey')
mtext('proportion repertoire', 2, 3, cex = 0.66)
lines(unique(sort(dat_age_direct_2021$age)), p_post[5,], lwd = 2)

# Scatter versatility
p_post = 
  vapply(unique(sort(dat_age_direct_2021$net_pos)), function(np)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_net_pos[j,i] * np), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$net_pos, dat_age_direct_2021$X[1,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled versatility', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2021$net_pos)), p_post[1,], lwd = 2)

# Scatter entry size
p_post = 
  vapply(unique(sort(dat_age_direct_2021$entry_size)), function(es)
    vapply(seq_len(dat_age_direct_2021$N_cat), function(i) 
      vapply(seq_len(length(post_age_direct_2021$lp__)), function(j)
        exp(log(mean(post_age_direct_2021$A_bar[j,,i])) + post_age_direct_2021$B_entry[j,i] * es), 
        numeric(1)),
      numeric(length(post_age_direct_2021$lp__))) |> apply(1, make.simplex) |> apply(1, mean), 
    numeric(dat_age_direct_2021$N_cat))
plot(dat_age_direct_2021$entry_size, dat_age_direct_2021$X[1,]/colSums(dat_age_direct_2021$X), ylim = c(0, 1),
     xlab = 'scaled chamber size', yaxt = 'n', col = 'grey')
lines(unique(sort(dat_age_direct_2021$entry_size)), p_post[1,], lwd = 2)

# Scatter sex
plot(dat_sex_total_2021$sex + rnorm(dat_sex_total_2021$N_ind, 0, 0.05), 
     dat_sex_total_2021$X[1,]/colSums(dat_sex_total_2021$X), yaxt = 'n', col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', xlab = '', ylab = 'proportion')
axis(1, 1:2, c('female', 'male'))
points(apply(post_sex_total_2021$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2021$A_bar[,j,1]), lwd = 2)

# Close PDF
dev.off()

# Reduced ----

# Open PDF
pdf('ANALYSIS/RESULTS/06 repertoire analysis/distribution analysis/final figure distributions - reduced.pdf',
    4.5, 4.5)
par(mfrow = c(2, 2), mar = c(3, 2.5, 1, 1), oma = c(1, 4, 0, 1))

# Plot results sex 2020
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2020$N_cat), pch = 16, xaxt = 'n', yaxt = 'n',
     main = '', ylab = '', xlab = '')
points(apply(post_sex_total_2020$cont_sex, 2, mean), seq_len(dat_sex_total_2020$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2020$N_cat)) 
  lines(apply(post_sex_total_2020$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(1, c(-0.3, 0, 0.3))
axis(2, 1:dat_age_direct_2020$N_cat, rownames(dat_age_direct_2020$X), las = 2)
mtext('contrast', 1, 2.2, cex = 0.8)
text(0.25, 8.5, 'a)', font = 2)

# Scatter sex
plot(dat_sex_total_2020$sex + rnorm(dat_sex_total_2020$N_ind, 0, 0.05), 
     dat_sex_total_2020$X[1,]/colSums(dat_sex_total_2020$X), col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
points(apply(post_sex_total_2020$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2020$A_bar[,j,1]), lwd = 2)
axis(1, 1:2, c('female', 'male'))
axis(2, c(0, 0.3, 0.6, 0.9))
mtext('sex', 1, 2.2, cex = 0.8)
text(2.32, 0.93, 'b)', font = 2)

# Plot results sex 2021
plot(NULL, xlim = c(-0.3, 0.3), ylim = c(1, dat_sex_total_2021$N_cat), pch = 16, xaxt = 'n', yaxt = 'n',
     main = '', ylab = '', xlab = '')
points(apply(post_sex_total_2021$cont_sex, 2, mean), seq_len(dat_sex_total_2021$N_cat), col = 1)
for(i in seq_len(dat_sex_total_2021$N_cat)) 
  lines(apply(post_sex_total_2021$cont_sex, 2, PI)[,i], rep(i, 2), col = 1)
abline(v = 0, lty = 2)
axis(2, 1:dat_age_direct_2021$N_cat, rownames(dat_age_direct_2021$X), las = 2)
axis(1, c(-0.3, 0, 0.3))
axis(2, 1:dat_age_direct_2020$N_cat, rownames(dat_age_direct_2020$X), las = 2)
mtext('contrast', 1, 2.2, cex = 0.8)
text(0.25, 8.5, 'c)', font = 2)

# Scatter sex
plot(dat_sex_total_2021$sex + rnorm(dat_sex_total_2021$N_ind, 0, 0.05), 
     dat_sex_total_2021$X[1,]/colSums(dat_sex_total_2021$X), col = 'grey',
     ylim = c(0, 1), xlim = c(0.5, 2.5), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
points(apply(post_sex_total_2021$A_bar[,,1], 2, mean), cex = 1.5, pch = 16)
for(j in 1:2) lines(rep(j, 2), PI(post_sex_total_2021$A_bar[,j,1]), lwd = 2)
axis(1, 1:2, c('female', 'male'))
axis(2, c(0, 0.3, 0.6, 0.9))
mtext('sex', 1, 2.2, cex = 0.8)
text(2.32, 0.93, 'd)', font = 2)

# Close PDF
dev.off()