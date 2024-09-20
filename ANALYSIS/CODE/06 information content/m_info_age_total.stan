// Project: complexity paper
// Date started: 24-02-2023
// Date last modified: 03-03-2023
// Author: Simeon Q. Smeele
// Description: Model explaining information content of contact calls by total effect of age. 

data{
    int N_obs; 
    int N_ind;
    int n[N_obs];
    vector[N_obs] age;
    int ind_ID[N_obs];
}
parameters{
    real a_bar;
    vector[N_ind] z_ind_ID;
    real<lower=0> sigma_ind_ID;
    real b_age;
}
model{
    real lambda[N_obs];
    a_bar ~ normal(1.5, 0.5);
    z_ind_ID ~ normal(0, 1);
    sigma_ind_ID ~ exponential(1);
    b_age ~ normal(0, 1);
    for(i in 1:N_obs){
      lambda[i] = a_bar + 
      z_ind_ID[ind_ID[i]] * sigma_ind_ID + 
      b_age * age[i]; 
      lambda[i] = exp(lambda[i]);
    }
    n ~ poisson(lambda);
}
