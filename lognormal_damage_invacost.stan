  data {
    int N; // number of reported costs
    real y[N];// observed costs
  }
parameters {
  real<lower=0> mu; 
  real<lower=0> sigma;
}
model {  
  target += -log(sigma)+lognormal_lpdf(y|mu, sigma);
}
generated quantities {
  real log_lik;
  log_lik = lognormal_lpdf(y|mu, sigma);
}

