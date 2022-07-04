library(data.table)
library(tidyverse)
library(rstan)

source("src/wrangling_functions.R")

brasileirao2019 <- fread("data/2019_partidas.csv")

brasileirao2019 <- reindex_teams(brasileirao2019)

brasileirao2019_list <- list(G = nrow(brasileirao2019), 
                             T = 20,
                             h = brasileirao2019$home_team_index,
                             a = brasileirao2019$away_team_index,
                             y1 = brasileirao2019$home_score,
                             y2 = brasileirao2019$away_score)

m1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan", model_name = "model1-hierarchical")
m1_fit <- sampling(object = m1, data = brasileirao2019_list)

y1_predict <- extract(m1_fit, pars = c("y1_tilde"))

