// Project: complexity paper
// Date started: 21-06-2023
// Date last modified: 21-06-2023
// Author: Simeon Q. Smeele
// Description: Simple model for distribution as function of age. 

data{
  int N_ind; 
  int N_cat;
  int X[N_cat, N_ind];
  real age[N_ind];
  vector[N_cat] alpha;
}
parameters{
  simplex[N_cat] A_bar;
  real<lower=0> theta;
  vector[N_cat] B_age;
  simplex[N_cat] p[N_ind];
}
model{
  vector[N_cat] s;
  vector[N_cat] pii;
  theta ~ exponential(0.2);
  A_bar ~ dirichlet(alpha);
  B_age ~ normal(0, 0.5);
  for(n in 1:N_ind){
    for(c in 1:N_cat){
      s[c] = exp(log(A_bar[c]) + B_age[c] * age[n]);
    } 
    pii = s/sum(s);
    p[n] ~ dirichlet(pii * theta); 
    X[,n] ~ multinomial(p[n]);
  }
}
