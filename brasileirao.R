library(data.table)
library(tidyverse)
library(rstan)

source("src/wrangling_functions.R")
source("src/plot_functions.R")

brasileirao2019 <- fread("data/2019_partidas.csv")

brasileirao2019 <- reindex_teams(brasileirao2019)

cols <- c("home_team", "away_team", "home_score", "away_score", 
          "home_team_index", "away_team_index")

brasileirao2019[ , ..cols] |> head() |> saveRDS(file = "dataframe_head.rds")

brasileirao2019_list <- list(G = nrow(brasileirao2019), 
                             T = 20,
                             h = brasileirao2019$home_team_index,
                             a = brasileirao2019$away_team_index,
                             y1 = brasileirao2019$home_score,
                             y2 = brasileirao2019$away_score)

m1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan", model_name = "model1-hierarchical")
m1_fit <- sampling(object = m1, data = brasileirao2019_list, chains = 1, iter = 5000)

y_predict <- extract(m1_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_df <- data.frame(y1_pred = y_predict$y1_tilde[1,], y2_pred = y_predict$y2_tilde[1,])

brasileirao2019_pred <- cbind(y_predict_df, brasileirao2019)

score_2019 <- brasileirao2019_pred |> 
  rename(y1 = "home_score", y2 = "away_score",
         h = "home_team_index", a = "away_team_index") |> 
  get_team_points_per_game()

score_2019_pred <- brasileirao2019_pred |> 
  rename(y1 = "y1_pred", y2 = "y2_pred",
         h = "home_team_index", a = "away_team_index") |> 
  get_team_points_per_game()

cbind(score_2019, score_2019_pred)

x <- merge(score_2019, score_2019_pred, by = c("game_id", "team_id"), 
           suffixes = c("_obs", "_est")) |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est))

create_cumsum_points_plot(x)
