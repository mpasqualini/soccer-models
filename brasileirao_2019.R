library(data.table)
library(loo)
library(tidyverse)
library(rstan)

set.seed(123)
source("src/wrangling_functions.R")
source("src/plot_functions.R")
path_to_save <- "artifacts/2019/"

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

parameters <- c("home", "mu_att", "mu_def", "sigma_att", 
                "sigma_def", "att_raw[1]", "def_raw[1]", 
                "att[1]", "def[1]")

traceplot_m1 <- traceplot(m1_fit, parameters)
ggsave(filename = paste0(path_to_save, "traceplot_m1.png"), plot = traceplot_m1, 
       width = 9.46, height = 6.27, dpi = 300)

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

## Results ---- 
cumsum_m1 <- points_per_game_obs_m1 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m1)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 1 (Baio, 2010)", "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_m1.png"), plot = cumsum_m1, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao <- points_per_game_obs_m1 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m1 = sum(points_scored_est_m1))

saveRDS(pontuacao, paste0(path_to_save, "pontuacao_total_m1.rds"))

### MSE ----

mse_m1 <- mse(pontuacao$score_obs,pontuacao$score_est_m1)

### LOO ----

log_lik_m1 <- extract_log_lik(m1_fit, parameter_name = "log_lik")
r_eff_m1 <- relative_eff(exp(log_lik_m1), chain_id = rep(1, 2500))
loo_m1 <- loo(log_lik_m1, r_eff = r_eff_m1)
waic_m1 <- waic(log_lik_m1)

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

parameters_m2 <- c("mu", "home", "alpha", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m2 <- traceplot(m2_fit, parameters_m2)
ggsave(filename = paste0(path_to_save, "traceplot_m2.png"), plot = traceplot_m2, 
       width = 9.46, height = 6.27, dpi = 300)


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

## Results ----
cumsum_m2 <- points_per_game_obs_m2 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m2)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 2: γ1 = 0, γ2 = 0 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_m2.png"), plot = cumsum_m2, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m2 <- points_per_game_obs_m2 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m2 = sum(points_scored_est_m2))

saveRDS(pontuacao_m2, paste0(path_to_save, "pontuacao_total_m2.rds"))

# Desempate rebaixamento
# pontuacao_m2 |> slice_min(score_est_m2, n=4)
# brasileirao_2019_with_pred |> 
#   filter(h == 11 | a == 11) |> 
#   summarize(sum(y1_pred_m2, y2_pred_m2))

### MSE ----
mse_m2 <- mse(pontuacao_m2$score_obs, pontuacao_m2$score_est_m2)

### LOO and WAIC ----

log_lik_m2 <- extract_log_lik(m2_fit, parameter_name = "log_lik")
r_eff_m2 <- relative_eff(exp(log_lik_m2), chain_id = rep(1, 2500))
loo_m2 <- loo(log_lik_m2, r_eff = r_eff_m2)
waic_m2 <- waic(log_lik_m2)

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

parameters_m3 <- c("mu", "home", "alpha", "alpha_home[1]", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m3 <- traceplot(m3_fit, parameters_m3, nrow = 5, ncol = 2)
ggsave(filename = paste0(path_to_save, "traceplot_m3.png"), 
       plot = traceplot_m3, 
       width = 9.46, height = 6.27, dpi = 300)

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

## Results ----

cumsum_m3 <- points_per_game_obs_m3 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m3)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 3: γ1 = 1, γ2 = 0 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_m3.png"), 
       plot = cumsum_m3, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m3 <- points_per_game_obs_m3 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m3 = sum(points_scored_est_m3))

saveRDS(pontuacao_m3, paste0(path_to_save, "pontuacao_total_m3.rds"))

### MSE ----

# mse_m3 <- data.frame(mse_y1 = mse(brasileirao_2019_with_pred$y1, 
#                                   brasileirao_2019_with_pred$y1_pred_m3),
#                      mse_y2 = mse(brasileirao_2019_with_pred$y2, 
#                                      brasileirao_2019_with_pred$y2_pred_m3))

mse_m3 <- mse(pontuacao_m3$score_obs, pontuacao_m3$score_est_m3)

### LOO and WAIC ----

log_lik_m3 <- extract_log_lik(m3_fit, parameter_name = "log_lik")
r_eff_m3 <- relative_eff(exp(log_lik_m3), chain_id = rep(1, 2500))
loo_m3 <- loo(log_lik_m3, r_eff = r_eff_m3)
waic_m3 <- waic(log_lik_m3)

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

parameters_m4 <- c("mu", "home", "alpha", "alpha_home[1]", 
                   "alpha_away[1]", "sigma_att", "sigma_def", 
                   "att_raw[1]", "def_raw[1]", "att[1]", "def[1]")

traceplot_m4 <- traceplot(m4_fit, parameters_m4)
ggsave(filename = paste0(path_to_save, "traceplot_m4.png"), plot = traceplot_m4, 
       width = 9.46, height = 6.27, dpi = 300)

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

## Results ----

cumsum_m4 <- points_per_game_obs_m4 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m4)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 4: γ1 = 1, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_m4.png"), plot = cumsum_m4, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m4 <- points_per_game_obs_m4 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m4 = sum(points_scored_est_m4))

saveRDS(pontuacao_m4, paste0(path_to_save, "pontuacao_total_m4.rds"))

### MSE ----

mse_m4 <- mse(pontuacao_m4$score_obs, pontuacao_m4$score_est_m4)

### LOO and WAIC ----

log_lik_m4 <- extract_log_lik(m4_fit, parameter_name = "log_lik")
r_eff_m4 <- relative_eff(exp(log_lik_m4), chain_id = rep(1, 2500))
loo_m4 <- loo(log_lik_m4, r_eff = r_eff_m4)
waic_m4 <- waic(log_lik_m4)

# Model 5 ----

brasileirao_2019_list_m5 <- list(G = nrow(brasileirao_2019), 
                                 T = 20,
                                 h = brasileirao_2019$h,
                                 a = brasileirao_2019$a,
                                 y1 = brasileirao_2019$y1,
                                 y2 = brasileirao_2019$y2,
                                 gamma1 = 0,
                                 gamma2 = 1)

m5 <- stan_model(file = "models/modelo2-poisson-bivariate.stan", 
                 model_name = "model3-bivariate-gamma11")

m5_fit <- sampling(object = m5, 
                   data = brasileirao_2019_list_m5, 
                   chains = 1, 
                   iter = 5000)

parameters_m5 <- c("mu", "home", "alpha", "alpha_away[1]", "sigma_att", 
                   "sigma_def", "att_raw[1]", "def_raw[1]", 
                   "att[1]", "def[1]")

traceplot_m5 <- traceplot(m5_fit, parameters_m5, nrow = 5, ncol = 2)

ggsave(filename = paste0(path_to_save, "traceplot_m5.png"), plot = traceplot_m5, 
       width = 9.46, height = 6.27, dpi = 300)

y_predict_m5 <- extract(m5_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m5_df <- data.frame(y1_pred_m5 = y_predict_m5$y1_tilde[1,], 
                              y2_pred_m5 = y_predict_m5$y2_tilde[1,])

brasileirao_2019_with_pred <- cbind(brasileirao_2019_with_pred, y_predict_m5_df)

score_brasileirao_2019_m5_pred <- 
  get_team_points_per_game(data = brasileirao_2019_with_pred, 
                           y1 = quo(y1_pred_m5), 
                           y2 = quo(y2_pred_m5))

points_per_game_obs_m5 <- merge(score_brasileirao_2019, 
                                score_brasileirao_2019_m5_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m5")) 

## Results ----
cumsum_m5 <- points_per_game_obs_m5 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m5)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 5: γ1 = 0, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_m5.png"), plot = cumsum_m5, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao_m5 <- points_per_game_obs_m5 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m5 = sum(points_scored_est_m5))

saveRDS(pontuacao_m5, paste0(path_to_save, "pontuacao_total_m5.rds"))

### MSE ----

mse_m5 <- mse(pontuacao_m5$score_obs, pontuacao_m5$score_est_m5)

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

pontuacao_all |> 
  select(contains("est")) |> 
  map_df(mse, actual = pontuacao_all$score_obs)

saveRDS(pontuacao_all, paste0(path_to_save, "pontuacao_all.rds"))

cumsum_all <- points_per_game_obs_all_models |> 
  pivot_longer(cols = c(points_scored_obs, 
                        points_scored_est_m1, 
                        points_scored_est_m2,
                        points_scored_est_m3,
                        points_scored_est_m4,
                        points_scored_est_m5,)) |> 
  create_cumsum_points_plot(linewidth = 0.8, year = 2019) +
  scale_color_discrete(labels = c("Modelo 1 (Baio, 2010)", 
                                  "Modelo 2: γ1 = 0, γ2 = 0 (Karlis, 2003)",
                                  "Modelo 3: γ1 = 1, γ2 = 0 (Karlis, 2003)",
                                  "Modelo 4: γ1 = 1, γ2 = 1 (Karlis, 2003)",
                                  "Modelo 5: γ1 = 0, γ2 = 1 (Karlis, 2003)", 
                                  "Observado"))

ggsave(filename = paste0(path_to_save, "cumsum_all.png"), plot = cumsum_all, 
       width = 9.76, height = 7.37, dpi = 300)

mse_models <- rbind(mse_m1, mse_m2, mse_m3, mse_m4, mse_m5)

loo_compare_models <- loo_compare(loo_m1, loo_m2, 
                                  loo_m3, loo_m4, loo_m5)

# Mixture ----

brasileirao_2019_list_m6 <- list(
  G = nrow(brasileirao_2019),
  T = 20,
  C = 3,
  h = brasileirao_2019$h,
  a = brasileirao_2019$a,
  y1 = brasileirao_2019$y1,
  y2 = brasileirao_2019$y2
)

m6 <- stan_model(file = "models/modelo3-mixture-rascunho4.stan", 
                 model_name = "model6-mixture")

m6_fit <- sampling(object = m6, 
                   data = brasileirao_2019_list_m6, 
                   chains = 2, 
                   thin = 5,
                   iter = 10000)

shinystan::launch_shinystan(m6_fit)

parameters <- c("home", "mu_att", "mu_def", "sigma_att", 
                "sigma_def", "att_raw[1]", "def_raw[3]", 
                "att[1]", "def[3]", "pi_att[1, 1]", "pi_def[3, 1]")

traceplot_m6 <- traceplot(m6_fit, parameters)

y_predict_m6 <- extract(m6_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m6_df <- data.frame(y1_pred_m6 = y_predict_m6$y1_tilde[1,], 
                              y2_pred_m6 = y_predict_m6$y2_tilde[1,])

brasileirao_2019_m6 <- cbind(brasileirao_2019, y_predict_m6_df)

score_brasileirao_2019_m6_pred <- 
  get_team_points_per_game(data = brasileirao_2019_m6, 
                           y1 = quo(y1_pred_m6), 
                           y2 = quo(y2_pred_m6))

points_per_game_obs_m6 <- merge(score_brasileirao_2019, 
                                score_brasileirao_2019_m6_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m6")) 

points_per_game_obs_m6 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m6)) |> 
  create_cumsum_points_plot(year = 2019) +
  scale_color_discrete(labels = c("Modelo 6: Mistura (Baio, 2010)", 
                                  "Observado"))

pontuacao_m6 <- points_per_game_obs_m6 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m6 = sum(points_scored_est_m6))

mse_m6 <- mse(pontuacao_m6$score_obs, pontuacao_m6$score_est_m6)

log_lik_m6 <- extract_log_lik(m6_fit, parameter_name = "log_lik")
r_eff_m6 <- relative_eff(exp(log_lik_m6), chain_id = rep(1, 2500))
loo_m6 <- loo(log_lik_m6, r_eff = r_eff_m6)
waic_m5 <- waic(log_lik_m5)

points_per_game_obs_m6 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m6 = sum(points_scored_est_m6))


