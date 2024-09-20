// Project: complexity paper
// Date started: 27-02-2023
// Date last modified: 27-02-2023
// Author: Simeon Q. Smeele
// Description: Model explaining repertoire entropy by total effect of age. 

data{
    int N_obs; 
    vector[N_obs] entropy;
    vector[N_obs] age;
}
parameters{
    real a_bar;
    real b_age;
    real<lower=0> sigma;
}
model{
    a_bar ~ normal(1, 1);
    b_age ~ normal(0, 0.5);
    sigma ~ exponential(1);
    entropy ~ normal(a_bar + b_age * age, sigma);
}
