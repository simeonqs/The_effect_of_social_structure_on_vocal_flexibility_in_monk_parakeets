// Project: complexity paper
// Date started: 04-11-2022
// Date last modified: 03-03-2023
// Author: Simeon Q. Smeele
// Description: Model to measure similarity for each dyad of individuals. 
// This version removes the within rec comparisons. 
// This version has all controls again. 
// This version has only ind pair again. 

data{
  int N_obs; 
  // int N_call;
  int N_rec_pair;
  int N_ind_pair;
  vector[N_obs] acc_dist;
  // int call_i[N_obs];
  // int call_j[N_obs];
  int rec_pair[N_obs];
  // int same_rec[N_obs];
  int ind_pair[N_obs];
}
parameters{
  real<lower=0> sigma_ind_pair;
  // real<lower=0> sigma_same_rec;
  real<lower=0> sigma_rec_pair;
  // real<lower=0> sigma_call;
  real<lower=0> sigma;
  real a_bar;
  vector[N_ind_pair] z_ind_pair;
  // vector[2] z_same_rec;
  vector[N_rec_pair] z_rec_pair;
  // vector[N_call] z_call;
}
model{
  vector[N_obs] mu;
  sigma_ind_pair ~ exponential(2);
  // sigma_same_rec ~ exponential(3);
  sigma_rec_pair ~ exponential(3);
  // sigma_call ~ exponential(3);
  sigma ~ exponential(2);
  a_bar ~ normal(0, 0.5);
  z_ind_pair ~ normal(0, 1);
  // z_same_rec ~ normal(0, 1);
  z_rec_pair ~ normal(0, 1);
  // z_call ~ normal(0, 1);
  acc_dist ~ normal(a_bar + 
    z_ind_pair[ind_pair] * sigma_ind_pair + 
    z_rec_pair[rec_pair] * sigma_rec_pair, 
    sigma);
}
  