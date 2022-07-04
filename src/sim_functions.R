generate_bivariate_poisson_model <- function(nt, mu, home, alpha, sigma_att, sigma_def, gamma1, gamma2) {
  aux <- cbind(rep(1:nt, each=nt), rep(1:nt, times=nt))
  aux <- aux[aux[,1]!=aux[,2],]
  
  id_h <- aux[,1]
  id_a <- aux[,2]
  
  G = length(id_h) # total de jogos
  
  ## Geracao dos efeitos aleatorios
  att <- rnorm(nt, 0, sigma_att)
  def <- rnorm(nt, 0, sigma_def)
  
  ## Restricao no ultimo time
  att[nt] <- 0
  def[nt] <- 0
  
  ## Geracao do parametro alpha_home 
  alpha_home <- rnorm(nt, 0, 1)
  
  ## Geracao do parametro alpha_away
  alpha_away <- rnorm(nt, 0, 1)
  
  ## Taxas
  lambda1 <<- exp(mu + home + att[id_h] + def[id_a])
  lambda2 <<- exp(mu + att[id_a] + def[id_h])
  lambda3 <<- exp(alpha + gamma1 * alpha_home + gamma2 * alpha_away)
  
  y_h <<- rpois(G, lambda1 + lambda3)
  y_a <<- rpois(G, lambda2 + lambda3)
  
  return(data.frame(g = 1:G, h = id_h, a = id_a, y1 = y_h, y2 = y_a))
}

generate_hierarchical_poisson_model <- function(nt, home, mu_att, mu_def,  sigma_att, sigma_def) {
  aux <- cbind(rep(1:nt, each = nt), rep(1:nt, times = nt))
  aux <- aux[aux[,1]!=aux[,2],]
  
  id_h <- aux[,1]
  id_a <- aux[,2]
  
  G <- length(id_h) # total de jogos
  
  ## Geracao dos efeitos aleatorios
  
  att <<- rnorm(nt, mu_att, sigma_att)
  def <<- rnorm(nt, mu_def, sigma_def)
  att[nt] <- 0
  def[nt] <- 0
  
  ## Taxas
  theta_h <- exp(home + att[id_h] + def[id_a])
  theta_a <- exp(att[id_a] + def[id_h])
  
  y_h <- rpois(G, theta_h)
  y_a <- rpois(G, theta_a)
  
  return(data.frame(h = id_h, a = id_a, y1 = y_h, y2 = y_a))
}

run_sim <- function(data_list, model, nchains, niter) {
  fit_stan_model <- sampling(model,
                             data = data_list,
                             chains = nchains,
                             iter = niter)
  
  return(fit_stan_model)
}

#create_histogram <- function(param, summarized_draws, true_params_list) {
#  plot <- summarized_draws |> 
#    filter(variable == param) |> 
#    ggplot(aes(x = mean)) + 
#    geom_histogram() +
#    geom_vline(xintercept = true_params_list[[param]],
#               linetype="dotted") +
#    ggtitle(param)
#  
#  return(plot)
#}