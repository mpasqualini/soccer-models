library(bayesplot)
library(data.table)
library(gridExtra)
library(posterior)
library(tidyverse)
library(rstan)

source("src/funcoes.R")

nreps <- 1
true_params_hierarchical_poisson <- list(run = 1:nreps,
                                         nt = 20,
                                         home = 0.13,
                                         mu_att = 0.05,
                                         mu_def = 0.08,
                                         sigma_att = 0.56,
                                         sigma_def = 0.52)

y <- expand.grid(true_params_hierarchical_poisson)
y[['run']] <- NULL

model1_generated_data <- pmap(y, generate_hierarchical_poisson_model)
n_teams_games <- list(G=380, T=20)
model1_generated_data <- lapply(model1_generated_data, append, n_teams_games)

model1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan",
                      model_name = "model1")
  
model_fit <- map(model1_generated_data, run_sim, model = model1, nchains = 1, niter = 5000)
saveRDS(model_fit, file = "artifacts/model1-sim.rds")

sum_draws <- map(model_fit, summarize_draws) |> rbindlist()

params_hist <- list("home", "mu_att", "mu_def", "sigma_att", "sigma_def")
p <- map(params_hist, 
         create_histogram, 
         summarized_draws = sum_draws, 
         true_params_list = true_params_hierarchical_poisson)

grid.arrange(grobs = p, ncol = 2) 
