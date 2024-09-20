// Project: complexity paper
// Date started: 24-02-2023
// Date last modified: 24-02-2023
// Author: Simeon Q. Smeele
// Description: Model explaining information content of contact calls by total effect of tree size. 

data{
    int N_obs; 
    int N_ind;
    int n[N_obs];
    vector[N_obs] tree_size;
    vector[N_obs] degree;
    vector[N_obs] age;
    int ind_ID[N_obs];
    int sex[N_obs];
    vector[N_obs] entry_size;
}
parameters{
    real a_bar;
    vector[N_ind] z_ind_ID;
    real<lower=0> sigma_ind_ID;
    vector[2] z_sex;
    real<lower=0> sigma_sex;
    real b_tree;
    real b_degree;
    real b_age;
    real b_entry;
}
model{
    real lambda[N_obs];
    a_bar ~ normal(1.5, 0.5);
    z_ind_ID ~ normal(0, 1);
    sigma_ind_ID ~ exponential(1);
    z_sex ~ normal(0, 1);
    sigma_sex ~ exponential(1);
    b_tree ~ normal(0, 1);
    b_age ~ normal(0, 1);
    b_degree ~ normal(0, 1);
    b_entry ~ normal(0, 1);
    for(i in 1:N_obs){
      lambda[i] = a_bar + 
      z_ind_ID[ind_ID[i]] * sigma_ind_ID + 
      z_sex[sex[i]] * sigma_sex + 
      b_tree * tree_size[i] +
      b_degree * degree[i] +
      b_age * age[i] +
      b_entry * entry_size[i]; 
      lambda[i] = exp(lambda[i]);
    }
    n ~ poisson(lambda);
}
