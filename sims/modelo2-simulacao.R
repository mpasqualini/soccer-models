library(bayesplot)
library(data.table)
library(gridExtra)
library(posterior)
library(progress)
library(tidyverse)
library(rstan)

source("src/funcoes.R")

# Model 2: gamma1 = gamma2 = 0 ----

nreps <- 1
true_params_bivariate_poisson <- list(run = 1:nreps,
                                      nt = 20,
                                      home = 0.13,
                                      mu = 0.21,
                                      alpha = 0.2,
                                      sigma_att = 0.92,
                                      sigma_def = 0.89, 
                                      gamma1 = 0,
                                      gamma2 = 0)

y <- expand.grid(true_params_bivariate_poisson)
y[['run']] <- NULL

modelo2_generated_data <- pmap(y, generate_bivariate_poisson_model)
n_teams_games <- list(G=380, T=20, gamma1 = 0, gamma2 = 0)
modelo2_generated_data <- lapply(modelo2_generated_data, append, n_teams_games)

modelo2 <- stan_model(file = "models/modelo2-poisson-bivariate.stan",
                      model_name = "modelo2")

model2_fit <- map(modelo2_generated_data, run_sim, model = modelo2, nchains = 1, niter = 5000)
saveRDS(model2_fit, file = "artifacts/model2-sim.rds")

sum_draws <- map(model2_fit, summarize_draws) |> rbindlist()

params_hist <- list("home", "mu", "alpha", "sigma_att", "sigma_def")
p <- map(params_hist, 
         create_histogram, 
         summarized_draws = sum_draws, 
         true_params_list = true_params_bivariate_poisson)

grid.arrange(grobs = p, ncol = 2) 
