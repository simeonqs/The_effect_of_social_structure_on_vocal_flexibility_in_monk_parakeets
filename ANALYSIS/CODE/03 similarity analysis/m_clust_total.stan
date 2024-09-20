// Project: complexity paper
// Date started: 13-02-2023
// Date last modified: 13-02-2023
// Author: Simeon Q. Smeele
// Description: Model for the total effect of same cluster on similarity. 

data{
    int N_obs; 
    real acc_dist[N_obs];
    real acc_dist_se[N_obs];
    int same_cluster[N_obs];
}
parameters{
    real<lower=0> sigma;
    real a_bar;
    vector[2] z_clust;
    real<lower=0> sigma_clust;
    vector[N_obs] true_acc_dist;
}
model{
    sigma ~ exponential(2);
    a_bar ~ normal(0, 0.25);
    z_clust ~ normal(0, 1);
    sigma_clust ~ exponential(2);
    true_acc_dist ~ normal(a_bar + 
      z_clust[same_cluster] * sigma_clust,
      sigma);
    acc_dist ~ normal(true_acc_dist, acc_dist_se);
}
generated quantities{
  real cont_clust;
  cont_clust = z_clust[1] * sigma_clust - z_clust[2] * sigma_clust;
}
