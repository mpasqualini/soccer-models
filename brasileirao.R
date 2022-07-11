library(data.table)
library(loo)
library(tidyverse)
library(rstan)

set.seed(123)
source("src/wrangling_functions.R")
source("src/plot_functions.R")

time_ids <- fread("data/times_ids.csv") |> distinct(id, .keep_all = TRUE)
time_ids <- time_ids[ , id, nome.cartola]

brasileirao_2019 <- fread("data/2019_partidas.csv")

brasileirao_2019 <- brasileirao_2019 |> 
  left_join(time_ids, by = c("home_team" = "id")) |> 
  left_join(time_ids, by = c("away_team" = "id")) |> 
  rename(home_team_name = "nome.cartola.x",
         away_team_name = "nome.cartola.y")

# Preparing original data ----
brasileirao_2019 <- reindex_teams(brasileirao_2019)

brasileirao_2019 <- brasileirao_2019 |> 
  rename(y1 = "home_score", 
         y2 = "away_score",
         h = "home_team_index", 
         a = "away_team_index")

score_brasileirao_2019 <- 
  get_team_points_per_game(data = brasileirao_2019, 
                           y1 = quo(y1), 
                           y2 = quo(y2))

# cols <- c("home_team", "away_team", "home_score", "away_score", 
#          "home_team_index", "away_team_index")

# brasileirao_2019[ , ..cols] |> head() |> saveRDS(file = "dataframe_head.rds")

# Model 1 ----

brasileirao_2019_list <- list(G = nrow(brasileirao_2019), 
                              T = 20,
                              h = brasileirao_2019$h,
                              a = brasileirao_2019$a,
                              y1 = brasileirao_2019$y1,
                              y2 = brasileirao_2019$y2)

m1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan", 
                 model_name = "model1-hierarchical")

m1_fit <- sampling(object = m1, 
                   data = brasileirao_2019_list, 
                   chains = 1, 
                   iter = 5000)

y_predict_m1 <- extract(m1_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m1_df <- data.frame(y1_pred_m1 = y_predict_m1$y1_tilde[1,], 
                              y2_pred_m1 = y_predict_m1$y2_tilde[1,])

brasileirao_2019_with_pred <- cbind(brasileirao_2019, y_predict_m1_df)

score_brasileirao_2019_m1_pred <- 
  get_team_points_per_game(data = brasileirao_2019_with_pred, 
                           y1 = quo(y1_pred_m1), 
                           y2 = quo(y2_pred_m1))

points_per_game_obs_m1 <- merge(score_brasileirao_2019, 
                                score_brasileirao_2019_m1_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m1"))

points_per_game_obs_m1 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m1)) |> 
  create_cumsum_points_plot()

pontuacao <- points_per_game_obs_m1 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m1 = sum(points_scored_est_m1))

saveRDS(pontuacao, "artifacts/pontuacao_total_m1.rds")

## MSE ----

mean((brasileirao_2019_with_pred$y1 - brasileirao_2019_with_pred$y1_pred)^2)
mean((brasileirao_2019_with_pred$y2 - brasileirao_2019_with_pred$y2_pred)^2)

#points_championship <- merge(score_brasileirao_2019, 
#                             score_brasileirao_2019_pred, 
#                             by = c("game_id", "team_id"), 
#                             suffixes = c("_obs", "_est")) |> 
#  group_by(team_id) |> 
#  summarise(total_obs = sum(points_scored_obs),
#            total_est = sum(points_scored_est), 
#            mse = (total_obs - total_est)) |> 
#  summarise(mean(total_obs - total_est)^2)

## LOO ----

log_lik_y1_m1 <- extract_log_lik(m1_fit, parameter_name = "log_lik_y1")
ref_y1_m1 <- relative_eff(exp(log_lik_y1_m1), chain_id = rep(1, 2500))
loo_y1_m1 <- loo(log_lik_y1_m1, r_eff = ref_y1_m1)

log_lik_y2_m1 <- extract_log_lik(m1_fit, parameter_name = "log_lik_y2")
ref_y2_m1 <- relative_eff(exp(log_lik_y2_m1), chain_id = rep(1, 2500))
loo_y2_m1 <-loo(log_lik_y2_m1, r_eff = ref_y2_m1)

# Model 2 ----

brasileirao_2019_list_m2 <- list(G = nrow(brasileirao_2019), 
                                 T = 20,
                                 h = brasileirao_2019$h,
                                 a = brasileirao_2019$a,
                                 y1 = brasileirao_2019$y1,
                                 y2 = brasileirao_2019$y2,
                                 gamma1 = 0,
                                 gamma2 = 0)

m2 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model2-bivariate-gamma00")

m2_fit <- sampling(object = m2, 
                   data = brasileirao_2019_list_m2, 
                   chains = 1, 
                   iter = 5000)

y_predict_m2 <- extract(m2_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m2_df <- data.frame(y1_pred_m2 = y_predict_m2$y1_tilde[1,], 
                              y2_pred_m2 = y_predict_m2$y2_tilde[1,])

brasileirao_2019_with_pred <- cbind(brasileirao_2019_with_pred, y_predict_m2_df)

score_brasileirao_2019_m2_pred <- 
  get_team_points_per_game(data = brasileirao_2019_with_pred, 
                           y1 = quo(y1_pred_m2), 
                           y2 = quo(y2_pred_m2))

points_per_game_obs_m2 <- merge(score_brasileirao_2019, 
                             score_brasileirao_2019_m2_pred, 
                             by = c("game_id", "team_id", "team_name"), 
                             suffixes = c("_obs", "_est_m2")) 

points_per_game_obs_m2 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m2)) |> 
  create_cumsum_points_plot()

pontuacao_m2 <- points_per_game_obs_m2 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m2 = sum(points_scored_est_m2))

saveRDS(pontuacao_m2, "artifacts/pontuacao_total_m2.rds")

# Desempate rebaixamento
pontuacao_m2 |> slice_min(score_est_m2, n=4)
brasileirao_2019_with_pred |> 
  filter(h == 11 | a == 11) |> 
  summarize(sum(y1_pred_m2, y2_pred_m2))

## LOO ----

log_lik_y1_m2 <- extract_log_lik(m2_fit, parameter_name = "log_lik_y1")
ref_y1_m2 <- relative_eff(exp(log_lik_y1_m2), chain_id = rep(1, 2500))
loo_y1_m2 <- loo(log_lik_y1_m2, r_eff = ref_y1_m2)

log_lik_y2_m2 <- extract_log_lik(m2_fit, parameter_name = "log_lik_y2")
ref_y2_m2 <- relative_eff(exp(log_lik_y2_m2), chain_id = rep(1, 2500))
loo_y2_m2 <-loo(log_lik_y2_m2, r_eff = ref_y2_m2)

# Model 3 ----

brasileirao_2019_list_m3 <- list(G = nrow(brasileirao_2019), 
                                 T = 20,
                                 h = brasileirao_2019$h,
                                 a = brasileirao_2019$a,
                                 y1 = brasileirao_2019$y1,
                                 y2 = brasileirao_2019$y2,
                                 gamma1 = 1,
                                 gamma2 = 0)

m3 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma10")

m3_fit <- sampling(object = m3, 
                   data = brasileirao_2019_list_m3, 
                   chains = 1, 
                   iter = 5000)

y_predict_m3 <- extract(m3_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m3_df <- data.frame(y1_pred_m3 = y_predict_m3$y1_tilde[1,], 
                              y2_pred_m3 = y_predict_m3$y2_tilde[1,])

brasileirao_2019_with_pred <- cbind(brasileirao_2019_with_pred, y_predict_m3_df)

score_brasileirao_2019_m3_pred <- 
  get_team_points_per_game(data = brasileirao_2019_with_pred, 
                           y1 = quo(y1_pred_m3), 
                           y2 = quo(y2_pred_m3))

points_per_game_obs_m3 <- merge(score_brasileirao_2019, 
                                score_brasileirao_2019_m3_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m3")) 

points_per_game_obs_m3 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m3)) |> 
  create_cumsum_points_plot()

pontuacao_m3 <- points_per_game_obs_m3 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m3 = sum(points_scored_est_m3))

saveRDS(pontuacao_m3, "artifacts/pontuacao_total_m3.rds")

## LOO ----

log_lik_y1_m3 <- extract_log_lik(m3_fit, parameter_name = "log_lik_y1")
ref_y1_m3 <- relative_eff(exp(log_lik_y1_m3), chain_id = rep(1, 2500))
loo_y1_m3 <- loo(log_lik_y1_m3, r_eff = ref_y1_m3)

log_lik_y2_m3 <- extract_log_lik(m3_fit, parameter_name = "log_lik_y2")
ref_y2_m3 <- relative_eff(exp(log_lik_y2_m3), chain_id = rep(1, 2500))
loo_y2_m3 <-loo(log_lik_y2_m3, r_eff = ref_y2_m3)

# Model 4 ----

brasileirao_2019_list_m4 <- list(G = nrow(brasileirao_2019), 
                                 T = 20,
                                 h = brasileirao_2019$h,
                                 a = brasileirao_2019$a,
                                 y1 = brasileirao_2019$y1,
                                 y2 = brasileirao_2019$y2,
                                 gamma1 = 1,
                                 gamma2 = 1)

m4 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma11")

m4_fit <- sampling(object = m4, 
                   data = brasileirao_2019_list_m4, 
                   chains = 1, 
                   iter = 5000)

y_predict_m4 <- extract(m4_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m4_df <- data.frame(y1_pred_m4 = y_predict_m4$y1_tilde[1,], 
                              y2_pred_m4 = y_predict_m4$y2_tilde[1,])

brasileirao_2019_with_pred <- cbind(brasileirao_2019_with_pred, y_predict_m4_df)

score_brasileirao_2019_m4_pred <- 
  get_team_points_per_game(data = brasileirao_2019_with_pred, 
                           y1 = quo(y1_pred_m4), 
                           y2 = quo(y2_pred_m4))

points_per_game_obs_m4 <- merge(score_brasileirao_2019, 
                                score_brasileirao_2019_m4_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m4")) 

points_per_game_obs_m4 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m4)) |> 
  create_cumsum_points_plot()

pontuacao_m4 <- points_per_game_obs_m4 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m4 = sum(points_scored_est_m4))

saveRDS(pontuacao_m4, "artifacts/pontuacao_total_m4.rds")

## LOO ----

log_lik_y1_m4 <- extract_log_lik(m4_fit, parameter_name = "log_lik_y1")
ref_y1_m4 <- relative_eff(exp(log_lik_y1_m4), chain_id = rep(1, 2500))
loo_y1_m4 <- loo(log_lik_y1_m4, r_eff = ref_y1_m4)

log_lik_y2_m4 <- extract_log_lik(m4_fit, parameter_name = "log_lik_y2")
ref_y2_m4 <- relative_eff(exp(log_lik_y2_m4), chain_id = rep(1, 2500))
loo_y2_m4 <-loo(log_lik_y2_m4, r_eff = ref_y2_m4)

# Compare all models ----

points_per_game_obs_m1_m2 <- merge(points_per_game_obs_m1, 
                                   points_per_game_obs_m2, 
                                   by = c("game_id", 
                                          "team_id", 
                                          "team_name",
                                          "points_scored_obs"))

points_per_game_obs_m3_m4 <- merge(points_per_game_obs_m3, 
                                   points_per_game_obs_m4, 
                                   by = c("game_id", 
                                          "team_id", 
                                          "team_name",
                                          "points_scored_obs"))

points_per_game_obs_all_models <- merge(points_per_game_obs_m1_m2, 
                                        points_per_game_obs_m3_m4, 
                                        by = c("game_id", 
                                               "team_id", 
                                               "team_name",
                                               "points_scored_obs"))

points_per_game_obs_all_models |> 
  pivot_longer(cols = c(points_scored_obs, 
                        points_scored_est_m1, 
                        points_scored_est_m2,
                        points_scored_est_m3,
                        points_scored_est_m4)) |> 
  create_cumsum_points_plot()

loo_compare(loo_y1_m1, loo_y1_m2, loo_y1_m3, loo_y1_m4)
loo_compare(loo_y2_m1, loo_y2_m2, loo_y2_m3, loo_y2_m4)
