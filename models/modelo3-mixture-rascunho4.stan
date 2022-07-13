data {
  int<lower=1> G; // Numero total de jogos
  int<lower=1> T; // Numero de total times
  int<lower=1> C; // Numero de componentes
  int<lower=0, upper=T> h[G]; // Indice do time que joga em casa no G-esimo jogo
  int<lower=0, upper=T> a[G]; // Indice do time que joga como visitante no G-esimo jogo
  int<lower=0> y1[G]; // Numero de gols marcados no G-esimo jogo pelo time que joga em casa
  int<lower=0> y2[G]; // Numero de gols marcados no G-esimo jogo pelo time que joga como visitante
}

parameters {
  real home; 
  real<lower=-3,upper=0> mu_att_raw1;
  real mu_att_raw2;
  real<lower=0, upper=3> mu_att_raw3;
  real<lower=0, upper=3> mu_def_raw1;
  real mu_def_raw2;
  real<lower=-3,upper=0> mu_def_raw3;

  real<lower=0> sigma_att[C];
  real<lower=0> sigma_def[C];
  vector[T] att_raw;
  vector[T] def_raw;
  simplex[3] pi_att[T];
  simplex[3] pi_def[T];
}

transformed parameters {
  vector[T] att;
  vector[T] def;
  vector[C] mu_att;
  vector[C] mu_def;
  
  for (t in 1:(T-1)) {
    att[t] = att_raw[t];
    def[t] = def_raw[t];
  }
  att[T] = 0;
  def[T] = 0;
  
  mu_att[1] = mu_att_raw1;
  mu_att[2] = mu_att_raw2;
  mu_att[3] = mu_att_raw3;
  mu_def[1] = mu_def_raw1;
  mu_def[2] = mu_def_raw2;
  mu_def[3] = mu_def_raw3;
}

model {
  real m_att[C];
  real m_def[C];
  vector[G] lambda1;
  vector[G] lambda2;
  vector[G] m_y1;
  vector[G] m_y2;
  
  home ~ normal(0, 10);
  mu_att_raw1 ~ cauchy(0, 2.5) T[-3,0] ;
  mu_att_raw2 ~ normal(0, 0.01);
  mu_att_raw3 ~ cauchy(0, 2.5)T[0,3] ;
  mu_def_raw1 ~ cauchy(0, 2.5)T[0,3] ;
  mu_def_raw2 ~ normal(0, 0.01);
  mu_def_raw3 ~ cauchy(0, 2.5)T[-3,0] ;
  sigma_att ~ cauchy(0, 2.5);
  sigma_def ~ cauchy(0, 2.5);

  for (t in 1:T) {
    pi_att[t] ~ dirichlet(rep_vector(1, C));
    pi_def[t] ~ dirichlet(rep_vector(1, C));
    
    for(c in 1:C) {
      m_att[c] = log(pi_att[t, c]) + student_t_lpdf(att[t] | 4, mu_att[c], sigma_att[c]);
      m_def[c] = log(pi_def[t, c]) + student_t_lpdf(def[t] | 4, mu_def[c], sigma_def[c]);
    }
    
    target += log_sum_exp(m_att) + log_sum_exp(m_def);
  }
  
  for (g in 1:G) {
    
    lambda1[g] = home + att[h[g]] + def[a[g]]; 
    lambda2[g] = att[a[g]] + def[h[g]];
    
    m_y1[g] = poisson_log_lpmf(y1[g] | lambda1[g]); 
    m_y2[g] = poisson_log_lpmf(y2[g] | lambda2[g]); 
    
    target += m_y1[g] + m_y2[g];
  }
}

generated quantities {
  vector[G] y1_tilde;
  vector[G] y2_tilde;
  vector[G] log_lik;
  
  for (g in 1:G) {
    y1_tilde[g] = poisson_log_rng(home + att[h[g]] + def[a[g]]);
    y2_tilde[g] = poisson_log_rng(att[a[g]] + def[h[g]]);
    log_lik[g] = poisson_log_lpmf(y1[g] | home + att[h[g]] + def[a[g]]) + poisson_log_lpmf(y2[g] | att[a[g]] + def[h[g]]);
  }
}
