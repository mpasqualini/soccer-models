library(bayesplot)
library(data.table)
library(gridExtra)
library(posterior)
library(tidyverse)
library(rstan)

source("funcoes.R")

# Model 4: gamma1 = 1, gamma2 = 1 ----

nreps <- 1
true_params_bivariate_poisson <- list(run = 1:nreps,
                                      nt = 20,
                                      home = 0.13,
                                      mu = 0.21,
                                      alpha = 0.2,
                                      sigma_att = 0.92,
                                      sigma_def = 0.89, 
                                      gamma1 = 1,
                                      gamma2 = 1)

y <- expand.grid(true_params_bivariate_poisson)
y[['run']] <- NULL

model4_generated_data <- pmap(y, generate_bivariate_poisson_model)
n_teams_games <- list(G=380, T=20, gamma1 = 1, gamma2 = 1)
model4_generated_data <- lapply(model4_generated_data, append, n_teams_games)

model4 <- stan_model(file = "models/modelo2-poisson-bivariate.stan",
                     model_name = "model4")

model4_fit <- map(model4_generated_data, run_sim, model = model4, nchains = 1, niter = 5000)
saveRDS(model4_fit, file = "artifacts/model4-sim.rds")

sum_draws <- map(model4_fit, summarize_draws) |> rbindlist()

params_hist <- list("home", "mu", "alpha", "sigma_att", "sigma_def")
p <- map(params_hist, 
         create_histogram, 
         summarized_draws = sum_draws, 
         true_params_list = true_params_bivariate_poisson)

grid.arrange(grobs = p, ncol = 2) 
