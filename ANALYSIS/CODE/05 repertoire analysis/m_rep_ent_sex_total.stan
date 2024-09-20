// Project: complexity paper
// Date started: 01-03-2023
// Date last modified: 01-03-2023
// Author: Simeon Q. Smeele
// Description: Model explaining repertoire entropy by total effect of sex. 

data{
    int N_obs; 
    vector[N_obs] entropy;
    int sex[N_obs];
}
parameters{
    real a_bar;
    vector[2] z_sex;
    real<lower=0> sigma_sex;
    real<lower=0> sigma;
}
model{
    a_bar ~ normal(1, 1);
    z_sex ~ normal(0, 1);
    sigma_sex ~ exponential(1);
    sigma ~ exponential(1);
    entropy ~ normal(a_bar + 
      z_sex[sex] * sigma_sex, sigma);
}
generated quantities{
    vector[2] a_sex;
    real cont_sex;
    a_sex = z_sex * sigma_sex;
    cont_sex = a_sex[1] - a_sex[2];
}
