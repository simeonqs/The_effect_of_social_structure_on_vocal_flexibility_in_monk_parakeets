// Project: complexity paper
// Date started: 28-02-2023
// Date last modified: 28-02-2023
// Author: Simeon Q. Smeele
// Description: Model explaining repertoire entropy by total effect of entry size. 

data{
    int N_obs; 
    vector[N_obs] entropy;
    vector[N_obs] age;
    int sex[N_obs];
    vector[N_obs] entry_size;
}
parameters{
    real a_bar;
    vector[2] z_sex;
    real<lower=0> sigma_sex;
    real b_age;
    real b_entry;
    real<lower=0> sigma;
}
model{
    a_bar ~ normal(1, 1);
    z_sex ~ normal(0, 1);
    sigma_sex ~ exponential(1);
    b_age ~ normal(0, 0.5);
    b_entry ~ normal(0, 0.5);
    sigma ~ exponential(1);
    entropy ~ normal(a_bar + 
      z_sex[sex] * sigma_sex + 
      b_age * age +
      b_entry * entry_size, sigma);
}
