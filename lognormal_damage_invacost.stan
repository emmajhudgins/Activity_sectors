  data {
    real LT; //lower damage threshold
    real MT; //middle damage threshold
    real HT; // high damage threshold

    int L_spp; // the number of low observations
    int M_spp; // the number of medium observations
    int H_spp; // the number of high observations
  
    int N; // number of reported costs
    real y[N];// costs
  }
parameters {
  real<lower=0> mu; 
  real<lower=0> sigma;
}
model {  
  target += -log(sigma)+(L_spp)*lognormal_lcdf(LT | mu, sigma)+(M_spp)*log(lognormal_cdf(MT , mu, sigma)-lognormal_cdf(LT , mu, sigma))+(H_spp)*lognormal_lccdf(MT | mu, sigma)+lognormal_lpdf(y|mu, sigma);
}
generated quantities {
  real log_lik;
  log_lik = (L_spp)*lognormal_lcdf(LT | mu, sigma)+(M_spp)*log(lognormal_cdf(MT , mu, sigma)-lognormal_cdf(LT , mu, sigma))+(H_spp)*lognormal_lccdf(MT | mu, sigma)+lognormal_lpdf(y|mu, sigma);
}
