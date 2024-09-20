// Project: complexity paper
// Date started: 06-02-2023
// Date last modified: 23-02-2023
// Author: Simeon Q. Smeele
// Description: Model for the direct effect of entry size on diversity. 
data{
  int N_obs; 
  real average_div[N_obs];
  real<lower=0> se_div[N_obs];
  vector[N_obs] age;
  int sex[N_obs];
  vector[N_obs] entry_size;
}
parameters{
  vector[N_obs] real_div;
  real<lower=0> sigma;
  real a_bar;
  vector[2] z_sex;
  real<lower=0> sigma_sex;
  real b_age;
  real b_entry;
}
model{
  sigma ~ exponential(1);
  sigma_sex ~ exponential(2);
  a_bar ~ normal(0, 0.5);
  z_sex ~ normal(0, 1);
  b_age ~ normal(0, 0.5);
  b_entry ~ normal(0, 0.5);
  real_div ~ normal(a_bar + z_sex[sex] * sigma_sex + 
    b_age * age + 
    b_entry * entry_size, se_div);
  average_div ~ normal(real_div, sigma);
}
