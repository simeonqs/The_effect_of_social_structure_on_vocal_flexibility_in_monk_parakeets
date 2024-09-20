// Project: complexity paper
// Date started: 04-02-2023
// Date last modified: 23-02-2023
// Author: Simeon Q. Smeele
// Description: Model for the direct effect of age on diversity. 
data{
  int N_obs; 
  real average_div[N_obs];
  real<lower=0> se_div[N_obs];
  vector[N_obs] age;
  int sex[N_obs];
  vector[N_obs] entry_size;
  vector[N_obs] degree;
}
parameters{
  vector[N_obs] real_div;
  real<lower=0> sigma;
  real a_bar;
  vector[2] z_sex;
  real<lower=0> sigma_sex;
  real b_age;
  real b_entry;
  real b_degree;
}
model{
  sigma ~ exponential(1);
  sigma_sex ~ exponential(2);
  a_bar ~ normal(0, 0.5);
  z_sex ~ normal(0, 1);
  b_age ~ normal(0, 0.5);
  b_entry ~ normal(0, 0.5);
  b_degree ~ normal(0, 0.5);
  real_div ~ normal(a_bar + z_sex[sex] * sigma_sex + 
    b_age * age + 
    b_entry * entry_size +
    b_degree * degree, se_div);
  average_div ~ normal(real_div, sigma);
}
generated quantities{
  vector[2] a_sex;
  a_sex = z_sex * sigma_sex;
}
