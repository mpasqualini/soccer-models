data {
  int<lower=1> G; // Numero total de jogos
  int<lower=1> T; // Numero de total times
  int<lower=0, upper=T> h[G]; // Indice do time que joga em casa no G-esimo jogo
  int<lower=0, upper=T> a[G]; // Indice do time que joga como visitante no G-esimo jogo
  int<lower=0> y1[G]; // Numero de gols marcados no G-esimo jogo pelo time que joga em casa
  int<lower=0> y2[G]; // Numero de gols marcados no G-esimo jogo pelo time que joga como visitante
}

parameters {
  real home; 
  real mu_att;
  real mu_def;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  vector[T] att_raw;
  vector[T] def_raw;
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
  for (g in 1:G) {
    y1[g] ~ poisson_log(home + att[h[g]] + def[a[g]]); 
    y2[g] ~ poisson_log(att[a[g]] + def[h[g]]);
  }
  
  for (t in 1:T) {
    att_raw[t] ~ normal(mu_att, sigma_att);
    def_raw[t] ~ normal(mu_def, sigma_def);
  }
  
  home ~ normal(0, 10);
  mu_att ~ normal(0, 10);
  mu_def ~ normal(0, 10);
  sigma_att ~ cauchy(0, 2.5);
  sigma_def ~ cauchy(0, 2.5);
}

generated quantities {
  vector[G] y1_tilde;
  vector[G] y2_tilde;
  
  for (g in 1:G) {
    y1_tilde[g] = poisson_log_rng(home + att[h[g]] + def[a[g]]);
    y2_tilde[g] = poisson_log_rng(att[a[g]] + def[h[g]]);
  }
}
