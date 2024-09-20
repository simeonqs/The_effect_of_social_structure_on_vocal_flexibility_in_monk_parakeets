// Project: complexity paper
// Date started: 13-02-2023
// Date last modified: 17-09-2024
// Author: Simeon Q. Smeele
// Description: Model for the total and direct effect of the aggressive 
// network on similarity. 

data{
  int N_obs; 
  real acc_dist[N_obs];
  real acc_dist_se[N_obs];
  vector[N_obs] ag_edge_weight;
  int same_mate[N_obs];
  int same_cluster[N_obs];
  vector[N_obs] gen_edge_weight;
}
parameters{
  real<lower=0> sigma;
  real a_bar;
  vector[2] z_mate;
  real<lower=0> sigma_mate;
  vector[2] z_clust;
  real<lower=0> sigma_clust;
  vector[N_obs] true_acc_dist;
  real b_gen;
  real b_ag;
}
model{
  sigma ~ exponential(2);
  a_bar ~ normal(0, 0.25);
  z_mate ~ normal(0, 1);
  sigma_mate ~ exponential(2);
  z_clust ~ normal(0, 1);
  sigma_clust ~ exponential(2);
  b_gen ~ normal(0, 1);
  b_ag ~ normal(0, 1);
  true_acc_dist ~ normal(a_bar + 
    z_mate[same_mate] * sigma_mate +
    z_clust[same_cluster] * sigma_clust +
    b_gen * gen_edge_weight + 
    b_ag * ag_edge_weight,
    sigma);
  acc_dist ~ normal(true_acc_dist, acc_dist_se);
}
generated quantities{
  real cont_mate;
  cont_mate = z_mate[1] * sigma_mate - z_mate[2] * sigma_mate;
}
