// Project: complexity paper
// Date started: 13-02-2023
// Date last modified: 04-04-2023
// Author: Simeon Q. Smeele
// Description: Model for the total effect of foraging network on similarity. 

data{
    int N_obs; 
    real acc_dist[N_obs];
    real acc_dist_se[N_obs];
    vector[N_obs] spat_edge_weight;
    vector[N_obs] gen_edge_weight;
    int same_mate[N_obs];
    int same_cluster[N_obs];
}
parameters{
    real<lower=0> sigma;
    real a_bar;
    vector[2] z_mate;
    real<lower=0> sigma_mate;
    vector[2] z_cluster;
    real<lower=0> sigma_cluster;
    real b_spat;
    real b_gen;
    vector[N_obs] true_acc_dist;
}
model{
    sigma ~ exponential(2);
    a_bar ~ normal(0, 0.25);
    z_mate ~ normal(0, 1);
    sigma_mate ~ exponential(2);
    z_cluster ~ normal(0, 1);
    sigma_cluster ~ exponential(2);
    b_spat ~ normal(0, 1);
    b_gen ~ normal(0, 1);
    true_acc_dist ~ normal(a_bar + 
      z_mate[same_mate] * sigma_mate + 
      z_cluster[same_cluster] * sigma_cluster + 
      b_spat * spat_edge_weight +
      b_gen * gen_edge_weight, 
    sigma);
    acc_dist ~ normal(true_acc_dist, acc_dist_se);
}
