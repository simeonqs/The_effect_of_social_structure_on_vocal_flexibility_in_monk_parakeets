// Project: complexity paper
// Date started: 04-04-2023
// Date last modified: 04-04-2023
// Author: Simeon Q. Smeele
// Description: Model for the total effect of relatedness network on similarity. 

data{
  int N_obs; 
  real acc_dist[N_obs];
  real acc_dist_se[N_obs];
  vector[N_obs] gen_edge_weight;
}
parameters{
  real<lower=0> sigma;
  real a_bar;
  vector[N_obs] true_acc_dist;
  real b_gen;
}
model{
  sigma ~ exponential(2);
  a_bar ~ normal(0, 0.25);
  b_gen ~ normal(0, 1);
  true_acc_dist ~ normal(a_bar + 
    b_gen * gen_edge_weight,
    sigma);
  acc_dist ~ normal(true_acc_dist, acc_dist_se);
}
