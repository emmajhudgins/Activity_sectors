  data {
    real LT; //lower damage threshold
    real MT; //middle damage threshold
    real HT; // high damage threshold
    
    int L_spp; // the number of missing low species
    int M_spp; // the number of missing medium species
    int H_spp; // the number of missing high species
    
    int N; // number of costs
    real y[N]; //cost observations

  }
parameters {
  real<lower=0> shape; 
  real<lower=0> scale;
}
model { 
  target += -log(scale)+ (L_spp)*gamma_lcdf(LT | shape, scale)+ (M_spp)*log(gamma_cdf(MT , shape, scale)-gamma_cdf(LT , shape, scale))+ (H_spp)*gamma_lccdf(MT| shape, scale)+ gamma_lpdf(y | shape, scale);
}
generated quantities {
  real log_lik;
  log_lik = (L_spp)*gamma_lcdf(LT | shape, scale)+ (M_spp)*log(gamma_cdf(MT , shape, scale)-gamma_cdf(LT , shape, scale))+ (H_spp)*gamma_lccdf(MT| shape, scale)+ gamma_lpdf(y | shape, scale);
}
