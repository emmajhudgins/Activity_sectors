  data {
    int N; // number of observed costs
    real y[N]; //cost observations

  }
parameters {
  real<lower=0> shape; 
  real<lower=0> scale;
}
model { 
  target += -log(scale)+ gamma_lpdf(y | shape, scale);
}
generated quantities {
  real log_lik;
  log_lik =  gamma_lpdf(y | shape, scale);
}
