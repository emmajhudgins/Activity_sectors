  data {
    real LT; //lower damage threshold
    real MT; //middle damage threshold
    real HT; // high damage threshold

    int L_spp; // the number of low species without observations
    int M_spp; // the number of medium species without observations
    int H_spp; // the number of high species without observations
    
    int N; // number of costs
    real y[N]; // cost observations

  }
parameters {
  real<lower=0> shape; 
  real<lower=0> scale;
}
model { 
  target += -log(scale)+ (L_spp)*weibull_lcdf(LT | shape, scale)+ (M_spp)*log(weibull_cdf(MT , shape, scale)-weibull_cdf(LT , shape, scale))+ (H_spp)*weibull_lccdf(MT| shape, scale)+ weibull_lpdf(y | shape, scale);
}
generated quantities {
  real log_lik;
  log_lik =  (L_spp)*weibull_lcdf(LT | shape, scale)+ (M_spp)*log(weibull_cdf(MT , shape, scale)-weibull_cdf(LT , shape, scale))+ (H_spp)*weibull_lccdf(MT| shape, scale)+ weibull_lpdf(y | shape, scale);
  
}
