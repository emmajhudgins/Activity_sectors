  data {
    real LT; //lower damage threshold
    real MT; //middle damage threshold
    real HT; // high damage threshold

    int L_spp; // the number of low observations
    int M_spp; // the number of medium observations
    int H_spp; // the number of high observations
    
    int N;// number of reported observations in invacost
    real y[N]; //costs

  }
parameters {
  real<lower=0, upper=LT> ymin; 
  real<lower=0> alpha;
}
model {  
   target += -log(alpha)+(L_spp)*pareto_lcdf(LT | ymin, alpha)+(M_spp)*log(pareto_cdf(MT , ymin, alpha)-pareto_cdf(LT , ymin, alpha))+(H_spp)*pareto_lccdf(MT | ymin, alpha)+pareto_lpdf(y | ymin, alpha);
}
generated quantities {
  real log_lik;
  log_lik = (L_spp)*pareto_lcdf(LT | ymin, alpha)+(M_spp)*log(pareto_cdf(MT , ymin, alpha)-pareto_cdf(LT , ymin, alpha))+(H_spp)*pareto_lccdf(MT | ymin, alpha)+pareto_lpdf(y | ymin, alpha);
}
