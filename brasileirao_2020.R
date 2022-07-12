library(data.table)
library(loo)
library(tidyverse)
library(rstan)

set.seed(123)
source("src/wrangling_functions.R")
source("src/plot_functions.R")

time_ids <- fread("data/times_ids.csv") |> distinct(id, .keep_all = TRUE)
time_ids <- time_ids[ , id, nome.cartola]

brasileirao_2020 <- fread("data/2020_partidas.csv") |> distinct()

brasileirao_2020 <- brasileirao_2020 |> 
  left_join(time_ids, by = c("home_team" = "id")) |> 
  left_join(time_ids, by = c("away_team" = "id")) |> 
  rename(home_team_name = "nome.cartola.x",
         away_team_name = "nome.cartola.y")

# Preparing original data ----
brasileirao_2020 <- reindex_teams(brasileirao_2020)

brasileirao_2020 <- brasileirao_2020 |> 
  rename(y1 = "home_score", 
         y2 = "away_score",
         h = "home_team_index", 
         a = "away_team_index")

score_brasileirao_2020 <- 
  get_team_points_per_game(data = brasileirao_2020, 
                           y1 = quo(y1), 
                           y2 = quo(y2))

# Model 1 ----

brasileirao_2020_list <- list(G = nrow(brasileirao_2020), 
                              T = 20,
                              h = brasileirao_2020$h,
                              a = brasileirao_2020$a,
                              y1 = brasileirao_2020$y1,
                              y2 = brasileirao_2020$y2)

m1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan", 
                 model_name = "model1-hierarchical")

m1_fit <- sampling(object = m1, 
                   data = brasileirao_2020_list, 
                   chains = 1, 
                   iter = 5000)

parameters <- c("home", "mu_att", "mu_def", "sigma_att", 
                "sigma_def", "att_raw[1]", "def_raw[1]", 
                "att[1]", "def[1]")

traceplot_m1 <- traceplot(m1_fit, parameters)
ggsave(filename = "artifacts/traceplot_m1.png", plot = traceplot_m1, 
       width = 9.46, height = 6.27, dpi = 300)

y_predict_m1 <- extract(m1_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m1_df <- data.frame(y1_pred_m1 = y_predict_m1$y1_tilde[1,], 
                              y2_pred_m1 = y_predict_m1$y2_tilde[1,])

brasileirao_2020_with_pred <- cbind(brasileirao_2020, y_predict_m1_df)

score_brasileirao_2020_m1_pred <- 
  get_team_points_per_game(data = brasileirao_2020_with_pred, 
                           y1 = quo(y1_pred_m1), 
                           y2 = quo(y2_pred_m1))

points_per_game_obs_m1 <- merge(score_brasileirao_2020, 
                                score_brasileirao_2020_m1_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m1"))

## Results ---- 
cumsum_m1 <- points_per_game_obs_m1 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m1)) |> 
  create_cumsum_points_plot(year = 2020) +
  scale_color_discrete(labels = c("Modelo 1 (Baio, 2010)", "Observado"))

ggsave(filename = "artifacts/cumsum_m1.png", plot = cumsum_m1, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao <- points_per_game_obs_m1 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m1 = sum(points_scored_est_m1))

saveRDS(pontuacao, "artifacts/pontuacao_total_m1.rds")

### MSE ----

mse_m1 <- data.frame(mse_y1 = mse(brasileirao_2020_with_pred$y1, 
                                  brasileirao_2020_with_pred$y1_pred_m1),
                     mse_y2 = mse(brasileirao_2020_with_pred$y2, 
                                  brasileirao_2020_with_pred$y2_pred_m1))

### LOO ----

log_lik_m1 <- extract_log_lik(m1_fit, parameter_name = "log_lik")
r_eff_m1 <- relative_eff(exp(log_lik_m1), chain_id = rep(1, 2500))
loo_m1 <- loo(log_lik_m1, r_eff = r_eff_m1)
waic_m1 <- waic(log_lik_m1)

# Model 2 ----

brasileirao_2020_list_m2 <- list(G = nrow(brasileirao_2020), 
                                 T = 20,
                                 h = brasileirao_2020$h,
                                 a = brasileirao_2020$a,
                                 y1 = brasileirao_2020$y1,
                                 y2 = brasileirao_2020$y2,
                                 gamma1 = 0,
                                 gamma2 = 0)

m2 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model2-bivariate-gamma00")

m2_fit <- sampling(object = m2, 
                   data = brasileirao_2020_list_m2, 
                   chains = 1, 
                   iter = 5000)

parameters_m2 <- c("mu", "home", "alpha", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m2 <- traceplot(m2_fit, parameters_m2)
ggsave(filename = "artifacts/traceplot_m2.png", plot = traceplot_m2, 
       width = 9.46, height = 6.27, dpi = 300)


y_predict_m2 <- extract(m2_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m2_df <- data.frame(y1_pred_m2 = y_predict_m2$y1_tilde[1,], 
                              y2_pred_m2 = y_predict_m2$y2_tilde[1,])

brasileirao_2020_with_pred <- cbind(brasileirao_2020_with_pred, y_predict_m2_df)

score_brasileirao_2020_m2_pred <- 
  get_team_points_per_game(data = brasileirao_2020_with_pred, 
                           y1 = quo(y1_pred_m2), 
                           y2 = quo(y2_pred_m2))

points_per_game_obs_m2 <- merge(score_brasileirao_2020, 
                                score_brasileirao_2020_m2_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m2")) 

## Results ----
cumsum_m2 <- points_per_game_obs_m2 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m2)) |> 
  create_cumsum_points_plot(year = 2020) +
  scale_color_discrete(labels = c("Modelo 2: γ1 = 0, γ2 = 0 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = "artifacts/cumsum_m2.png", plot = cumsum_m2, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m2 <- points_per_game_obs_m2 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m2 = sum(points_scored_est_m2))

saveRDS(pontuacao_m2, "artifacts/pontuacao_total_m2.rds")

# Desempate rebaixamento
# pontuacao_m2 |> slice_min(score_est_m2, n=4)
# brasileirao_2020_with_pred |> 
#   filter(h == 11 | a == 11) |> 
#   summarize(sum(y1_pred_m2, y2_pred_m2))

### MSE ----
mse_m2 <- data.frame(mse_y1 = mse(brasileirao_2020_with_pred$y1, 
                                  brasileirao_2020_with_pred$y1_pred_m2),
                     mse_y2 = mse(brasileirao_2020_with_pred$y2, 
                                  brasileirao_2020_with_pred$y2_pred_m2))

### LOO and WAIC ----

log_lik_m2 <- extract_log_lik(m2_fit, parameter_name = "log_lik")
r_eff_m2 <- relative_eff(exp(log_lik_m2), chain_id = rep(1, 2500))
loo_m2 <- loo(log_lik_m2, r_eff = r_eff_m2)
waic_m2 <- waic(log_lik_m2)

# Model 3 ----

brasileirao_2020_list_m3 <- list(G = nrow(brasileirao_2020), 
                                 T = 20,
                                 h = brasileirao_2020$h,
                                 a = brasileirao_2020$a,
                                 y1 = brasileirao_2020$y1,
                                 y2 = brasileirao_2020$y2,
                                 gamma1 = 1,
                                 gamma2 = 0)

m3 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma10")

m3_fit <- sampling(object = m3, 
                   data = brasileirao_2020_list_m3, 
                   chains = 1, 
                   iter = 5000)

parameters_m3 <- c("mu", "home", "alpha", "alpha_home[1]", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m3 <- traceplot(m3_fit, parameters_m3, nrow = 5, ncol = 2)
ggsave(filename = "artifacts/traceplot_m3.png", plot = traceplot_m3, 
       width = 9.46, height = 6.27, dpi = 300)

y_predict_m3 <- extract(m3_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m3_df <- data.frame(y1_pred_m3 = y_predict_m3$y1_tilde[1,], 
                              y2_pred_m3 = y_predict_m3$y2_tilde[1,])

brasileirao_2020_with_pred <- cbind(brasileirao_2020_with_pred, y_predict_m3_df)

score_brasileirao_2020_m3_pred <- 
  get_team_points_per_game(data = brasileirao_2020_with_pred, 
                           y1 = quo(y1_pred_m3), 
                           y2 = quo(y2_pred_m3))

points_per_game_obs_m3 <- merge(score_brasileirao_2020, 
                                score_brasileirao_2020_m3_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m3")) 

## Results ----

cumsum_m3 <- points_per_game_obs_m3 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m3)) |> 
  create_cumsum_points_plot(year = 2020) +
  scale_color_discrete(labels = c("Modelo 3: γ1 = 1, γ2 = 0 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = "artifacts/cumsum_m3.png", plot = cumsum_m3, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m3 <- points_per_game_obs_m3 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m3 = sum(points_scored_est_m3))

saveRDS(pontuacao_m3, "artifacts/pontuacao_total_m3.rds")

### MSE ----

mse_m3 <- data.frame(mse_y1 = mse(brasileirao_2020_with_pred$y1, 
                                  brasileirao_2020_with_pred$y1_pred_m3),
                     mse_y2 = mse(brasileirao_2020_with_pred$y2, 
                                  brasileirao_2020_with_pred$y2_pred_m3))

### LOO and WAIC ----

log_lik_m3 <- extract_log_lik(m3_fit, parameter_name = "log_lik")
r_eff_m3 <- relative_eff(exp(log_lik_m3), chain_id = rep(1, 2500))
loo_m3 <- loo(log_lik_m3, r_eff = r_eff_m3)
waic_m3 <- waic(log_lik_m3)

# Model 4 ----

brasileirao_2020_list_m4 <- list(G = nrow(brasileirao_2020), 
                                 T = 20,
                                 h = brasileirao_2020$h,
                                 a = brasileirao_2020$a,
                                 y1 = brasileirao_2020$y1,
                                 y2 = brasileirao_2020$y2,
                                 gamma1 = 1,
                                 gamma2 = 1)

m4 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma11")

m4_fit <- sampling(object = m4, 
                   data = brasileirao_2020_list_m4, 
                   chains = 1, 
                   iter = 5000)

parameters_m4 <- c("mu", "home", "alpha", "alpha_home[1]", 
                   "alpha_away[1]", "sigma_att", "sigma_def", 
                   "att_raw[1]", "def_raw[1]", "att[1]", "def[1]")

traceplot_m4 <- traceplot(m4_fit, parameters_m4)
ggsave(filename = "artifacts/traceplot_m4.png", plot = traceplot_m4, 
       width = 9.46, height = 6.27, dpi = 300)

y_predict_m4 <- extract(m4_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m4_df <- data.frame(y1_pred_m4 = y_predict_m4$y1_tilde[1,], 
                              y2_pred_m4 = y_predict_m4$y2_tilde[1,])

brasileirao_2020_with_pred <- cbind(brasileirao_2020_with_pred, y_predict_m4_df)

score_brasileirao_2020_m4_pred <- 
  get_team_points_per_game(data = brasileirao_2020_with_pred, 
                           y1 = quo(y1_pred_m4), 
                           y2 = quo(y2_pred_m4))

points_per_game_obs_m4 <- merge(score_brasileirao_2020, 
                                score_brasileirao_2020_m4_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m4")) 

## Results ----

cumsum_m4 <- points_per_game_obs_m4 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m4)) |> 
  create_cumsum_points_plot(year = 2020) +
  scale_color_discrete(labels = c("Modelo 4: γ1 = 1, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = "artifacts/cumsum_m4.png", plot = cumsum_m4, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m4 <- points_per_game_obs_m4 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m4 = sum(points_scored_est_m4))

saveRDS(pontuacao_m4, "artifacts/pontuacao_total_m4.rds")

### MSE ----

mse_m4 <- data.frame(mse_y1 = mse(brasileirao_2020_with_pred$y1, 
                                  brasileirao_2020_with_pred$y1_pred_m4),
                     mse_y2 = mse(brasileirao_2020_with_pred$y2, 
                                  brasileirao_2020_with_pred$y2_pred_m4))

### LOO and WAIC ----

log_lik_m4 <- extract_log_lik(m4_fit, parameter_name = "log_lik")
r_eff_m4 <- relative_eff(exp(log_lik_m4), chain_id = rep(1, 2500))
loo_m4 <- loo(log_lik_m4, r_eff = r_eff_m4)
waic_m4 <- loo(log_lik_m4)

# Model 5 ----

brasileirao_2020_list_m5 <- list(G = nrow(brasileirao_2020), 
                                 T = 20,
                                 h = brasileirao_2020$h,
                                 a = brasileirao_2020$a,
                                 y1 = brasileirao_2020$y1,
                                 y2 = brasileirao_2020$y2,
                                 gamma1 = 0,
                                 gamma2 = 1)

m5 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma11")

m5_fit <- sampling(object = m5, 
                   data = brasileirao_2020_list_m5, 
                   chains = 1, 
                   iter = 5000)

parameters_m5 <- c("mu", "home", "alpha", "alpha_away[1]", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m5 <- traceplot(m5_fit, parameters_m5, nrow = 5, ncol = 2)

ggsave(filename = "artifacts/traceplot_m5.png", plot = traceplot_m5, 
       width = 9.46, height = 6.27, dpi = 300)

y_predict_m5 <- extract(m5_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m5_df <- data.frame(y1_pred_m5 = y_predict_m5$y1_tilde[1,], 
                              y2_pred_m5 = y_predict_m5$y2_tilde[1,])

brasileirao_2020_with_pred <- cbind(brasileirao_2020_with_pred, y_predict_m5_df)

score_brasileirao_2020_m5_pred <- 
  get_team_points_per_game(data = brasileirao_2020_with_pred, 
                           y1 = quo(y1_pred_m5), 
                           y2 = quo(y2_pred_m5))

points_per_game_obs_m5 <- merge(score_brasileirao_2020, 
                                score_brasileirao_2020_m5_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m5")) 

## Results ----
cumsum_m5 <- points_per_game_obs_m5 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m5)) |> 
  create_cumsum_points_plot(year = 2020) +
  scale_color_discrete(labels = c("Modelo 5: γ1 = 0, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = "artifacts/cumsum_m5.png", plot = cumsum_m5, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m5 <- points_per_game_obs_m5 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m5 = sum(points_scored_est_m5))

saveRDS(pontuacao_m5, "artifacts/pontuacao_total_m5.rds")

### MSE ----

mse_m5 <- data.frame(mse_y1 = mse(brasileirao_2020_with_pred$y1, 
                                  brasileirao_2020_with_pred$y1_pred_m5),
                     mse_y2 = mse(brasileirao_2020_with_pred$y2, 
                                  brasileirao_2020_with_pred$y2_pred_m5))

### LOO and WAIC ----

log_lik_m5 <- extract_log_lik(m5_fit, parameter_name = "log_lik")
r_eff_m5 <- relative_eff(exp(log_lik_m5), chain_id = rep(1, 2500))
loo_m5 <- loo(log_lik_m5, r_eff = r_eff_m5)
waic_m5 <- waic(log_lik_m5)

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

points_per_game_obs_m3_m4 <- merge(points_per_game_obs_m3_m4,
                                   points_per_game_obs_m5,
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

pontuacao_all <- points_per_game_obs_all_models |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m1 = sum(points_scored_est_m1),
            score_est_m2 = sum(points_scored_est_m2),
            score_est_m3 = sum(points_scored_est_m3),
            score_est_m4 = sum(points_scored_est_m4),
            score_est_m5 = sum(points_scored_est_m5))

saveRDS(pontuacao_all, "artifacts/pontuacao_all.rds")

cumsum_all <- points_per_game_obs_all_models |> 
  pivot_longer(cols = c(points_scored_obs, 
                        points_scored_est_m1, 
                        points_scored_est_m2,
                        points_scored_est_m3,
                        points_scored_est_m4,
                        points_scored_est_m5,)) |> 
  create_cumsum_points_plot(linewidth = 0.8, year = 2020) +
  scale_color_discrete(labels = c("Modelo 1 (Baio, 2010)", 
                                  "Modelo 2: γ1 = 0, γ2 = 0 (Karlis, 2003)",
                                  "Modelo 3: γ1 = 1, γ2 = 0 (Karlis, 2003)",
                                  "Modelo 4: γ1 = 1, γ2 = 1 (Karlis, 2003)",
                                  "Modelo 5: γ1 = 0, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = "artifacts/cumsum_all.png", plot = cumsum_all, 
       width = 9.76, height = 7.37, dpi = 300)

mse_models <- rbind(mse_m1, mse_m2, mse_m3, mse_m4, mse_m5)

loo_compare_models <- loo_compare(loo_m1, loo_m2, 
                                  loo_m3, loo_m4, loo_m5)
