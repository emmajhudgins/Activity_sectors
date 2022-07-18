  data {
    real LT; //lower damage threshold
    int N;// number of reported observations 
    real y[N]; //observed costs

  }
parameters {
  real<lower=0, upper=LT> ymin; 
  real<lower=0> alpha;
}
model {  
   target += -log(alpha)+pareto_lpdf(y | ymin, alpha);
}
generated quantities {
  real log_lik;
  log_lik = pareto_lpdf(y | ymin, alpha);
}
