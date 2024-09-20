// Project: complexity paper
// Date started: 28-02-2023
// Date last modified: 28-02-2023
// Author: Simeon Q. Smeele
// Description: Model explaining repertoire entropy by direct effect of age. 

data{
    int N_obs; 
    vector[N_obs] entropy;
    vector[N_obs] age;
    int sex[N_obs];
    vector[N_obs] netpos;
    vector[N_obs] entry_size;
    vector[N_obs] tree_size;
}
parameters{
    real a_bar;
    vector[2] z_sex;
    real<lower=0> sigma_sex;
    real b_age;
    real b_netpos;
    real b_entry;
    real b_tree;
    real<lower=0> sigma;
}
model{
    a_bar ~ normal(1, 1);
    z_sex ~ normal(0, 1);
    sigma_sex ~ exponential(1);
    b_age ~ normal(0, 0.5);
    b_netpos ~ normal(0, 0.5);
    b_entry ~ normal(0, 0.5);
    b_tree ~ normal(0, 0.5);
    sigma ~ exponential(1);
    entropy ~ normal(a_bar + 
      z_sex[sex] * sigma_sex + 
      b_age * age +
      b_netpos * netpos +
      b_entry * entry_size +
      b_tree * tree_size, sigma);
}
generated quantities{
    vector[2] a_sex;
    real cont_sex;
    a_sex = z_sex * sigma_sex;
    cont_sex = a_sex[1] - a_sex[2];
}
