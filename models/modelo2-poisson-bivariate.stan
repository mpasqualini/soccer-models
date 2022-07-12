data {
  int<lower=1> G;
  int<lower=1> T;
  int<lower=0, upper=T> h[G];
  int<lower=0, upper=T> a[G];
  int<lower=0> y1[G];
  int<lower=0> y2[G];
  int<lower=0, upper=1> gamma1; // Dummy para mudar os modelos  
  int<lower=0, upper=1> gamma2; // Dummy para mudar os modelos
}

parameters {
  real mu;
  real home;
  real alpha;
  vector[T] alpha_home;
  vector[T] alpha_away;
  vector[T-1] att_raw; 
  vector[T-1] def_raw; 
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
}

transformed parameters {
  vector[T] att;
  vector[T] def;
  
  for (t in 1:(T-1)) {
    att[t] = att_raw[t];
    def[t] = def_raw[t];
  }
  
  att[T] = 0;
  def[T] = 0;
}

model {
    vector[G] lambda1;
    vector[G] lambda2;
    vector[G] lambda3;
    
    att_raw ~ normal(0, sigma_att);
    def_raw ~ normal(0, sigma_def);
    home ~ normal(0, 10);
    sigma_att ~ cauchy(0, 2.5);
    sigma_def ~ cauchy(0, 2.5);
    mu ~ normal(0, 10);
    alpha ~ normal(0, 1);
    alpha_home ~ normal(0, 1);
    alpha_away ~ normal(0, 1);
    
    for (g in 1:G) {
      lambda1[g] = exp(mu + home + att[h[g]] + def[a[g]]);
      lambda2[g] = exp(mu + att[a[g]] + def[h[g]]);
      lambda3[g] = exp(alpha + gamma1 * alpha_home[h[g]] + gamma2 * alpha_away[a[g]]);
    }
    
    y1 ~ poisson(lambda1 + lambda3);
    y2 ~ poisson(lambda2 + lambda3);
}

generated quantities {
  vector[G] lambda1;
  vector[G] lambda2;
  vector[G] lambda3;
  vector[G] y1_tilde;
  vector[G] y2_tilde;
  vector[G] log_lik;
  
  for (g in 1:G) {
    lambda1[g] = exp(mu + home + att[h[g]] + def[a[g]]);
    lambda2[g] = exp(mu + att[a[g]] + def[h[g]]);
    lambda3[g] = exp(alpha + gamma1 * alpha_home[h[g]] + gamma2 * alpha_away[a[g]]);
    
    y1_tilde[g] = poisson_rng(lambda1[g] + lambda3[g]);
    y2_tilde[g] = poisson_rng(lambda2[g] + lambda3[g]);
    
    log_lik[g] = poisson_log_lpmf(y1[g] | mu + home + att[h[g]] + def[a[g]] + alpha + gamma1 * alpha_home[h[g]] + gamma2 * alpha_away[a[g]]) + poisson_log_lpmf(y2[g] | mu + att[a[g]] + def[h[g]] + alpha + gamma1 * alpha_home[h[g]] + gamma2 * alpha_away[a[g]]);
  }
}
