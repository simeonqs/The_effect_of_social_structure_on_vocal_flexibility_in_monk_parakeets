// Project: complexity paper
// Date started: 05-02-2023
// Date last modified: 23-02-2023
// Author: Simeon Q. Smeele
// Description: Model for the total effect of sex on diversity including uncertainty on diversity. 
data{
  int N_obs; 
  real average_div[N_obs];
  real<lower=0> se_div[N_obs];
  int sex[N_obs];
}
parameters{
  vector[N_obs] real_div;
  real<lower=0> sigma;
  real a_bar;
  vector[2] z_sex;
  real<lower=0> sigma_sex;
}
model{
  sigma ~ exponential(1);
  a_bar ~ normal(0, 0.5);
  z_sex ~ normal(0, 1);
  sigma_sex ~ exponential(2);
  real_div ~ normal(a_bar + z_sex[sex] * sigma_sex, se_div);
  average_div ~ normal(real_div, sigma);
}
generated quantities{
  vector[2] a_sex;
  a_sex = z_sex * sigma_sex;
}
