// Project: complexity paper
// Date started: 22-06-2023
// Date last modified: 22-06-2023
// Author: Simeon Q. Smeele
// Description: Model for the direct effect of age on distribution. 

data{
  int N_ind; 
  int N_cat;
  int X[N_cat, N_ind];
  real age[N_ind];
  real net_pos[N_ind];
  real tree_size[N_ind];
  real entry_size[N_ind];
  int sex[N_ind];
  vector[N_cat] alpha;
}
parameters{
  simplex[N_cat] A_bar[2];
  real<lower=0> theta;
  vector[N_cat] B_age;
  vector[N_cat] B_net_pos;
  vector[N_cat] B_tree;
  vector[N_cat] B_entry;
  simplex[N_cat] p[N_ind];
}
model{
  vector[N_cat] s;
  vector[N_cat] pii;
  theta ~ exponential(0.2);
  for(i in 1:2) A_bar[i] ~ dirichlet(alpha);
  B_age ~ normal(0, 0.5);
  B_net_pos ~ normal(0, 0.5);
  B_tree ~ normal(0, 0.5);
  B_entry ~ normal(0, 0.5);
  for(n in 1:N_ind){
    for(c in 1:N_cat){
      s[c] = exp(log(A_bar[sex[n],c]) + 
        B_age[c] * age[n] +
        B_net_pos[c] * net_pos[n] +
        B_tree[c] * tree_size[n] +
        B_entry[c] * entry_size[n]);
    } 
    pii = s/sum(s);
    p[n] ~ dirichlet(pii * theta); 
    X[,n] ~ multinomial(p[n]);
  }
}
