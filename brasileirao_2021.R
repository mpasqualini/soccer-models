library(bayesplot)
library(data.table)
library(loo)
library(tidyverse)
library(rstan)

set.seed(123)
source("src/wrangling_functions.R")
source("src/plot_functions.R")

brasileirao_2021_raw <- fread("/home/mariana/Documents/pessoal/monografia/jogos2021.csv")
cols <- c("rodada", "equipe_id", "equipe_id_1",
          "data_realizacao", "placar_oficial_mandante", 
          "placar_oficial_visitante")
brasileirao_2021 = brasileirao_2021_raw[ , ..cols, ]
brasileirao_2021 <- brasileirao_2021 |> 
  rename(date = data_realizacao,
         home_team = equipe_id, 
         away_team = equipe_id_1, 
         y1 = placar_oficial_mandante,
         y2 = placar_oficial_visitante)

time_ids <- fread("data/times_ids.csv") |> distinct(id, .keep_all = TRUE)
time_ids <- time_ids[ , id, nome.cartola]

brasileirao_2021 <- brasileirao_2021 |> 
  left_join(time_ids, by = c("home_team" = "id")) |> 
  left_join(time_ids, by = c("away_team" = "id")) |> 
  rename(home_team_name = "nome.cartola.x",
         away_team_name = "nome.cartola.y",
         round = "rodada")

brasileirao_2021 <- reindex_teams(brasileirao_2021)

brasileirao_2021 <- brasileirao_2021 |> 
  rename(h = "home_team_index", 
         a = "away_team_index")

score_brasileirao_2021 <- 
  get_team_points_per_game(data = brasileirao_2021, 
                           y1 = quo(y1), 
                           y2 = quo(y2))

# Model 1 ----

brasileirao_2021_list <- list(G = nrow(brasileirao_2021), 
                              T = 20,
                              h = brasileirao_2021$h,
                              a = brasileirao_2021$a,
                              y1 = brasileirao_2021$y1,
                              y2 = brasileirao_2021$y2)

m1 <- stan_model(file = "models/modelo1-poisson-hierarchical.stan", 
                 model_name = "model1-hierarchical")

m1_fit <- sampling(object = m1, 
                   data = brasileirao_2021_list, 
                   chains = 1, 
                   iter = 5000)

y_predict_m1 <- extract(m1_fit, pars = c("y1_tilde", "y2_tilde"))
y_predict_m1_df <- data.frame(y1_pred_m1 = y_predict_m1$y1_tilde[1,], 
                              y2_pred_m1 = y_predict_m1$y2_tilde[1,])

brasileirao_2021_with_pred <- cbind(brasileirao_2021, y_predict_m1_df)

score_brasileirao_2021_m1_pred <- 
  get_team_points_per_game(data = brasileirao_2021_with_pred, 
                           y1 = quo(y1_pred_m1), 
                           y2 = quo(y2_pred_m1))

points_per_game_obs_m1 <- merge(score_brasileirao_2021, 
                                score_brasileirao_2021_m1_pred, 
                                by = c("game_id", "team_id", "team_name"), 
                                suffixes = c("_obs", "_est_m1"))

## Results ---- 
cumsum_m1 <- points_per_game_obs_m1 |> 
  pivot_longer(cols = c(points_scored_obs, points_scored_est_m1)) |> 
  create_cumsum_points_plot(year = 2021) +
  scale_color_discrete(labels = c("Modelo 1 (Baio, 2010)", "Observado"))

ggsave(filename = "artifacts/cumsum_m1_2021.png", plot = cumsum_m1, 
       width = 9.76, height = 7.37, dpi = 300)

pontuacao <- points_per_game_obs_m1 |> 
  group_by(team_name) |> 
  summarize(score_obs = sum(points_scored_obs), 
            score_est_m1 = sum(points_scored_est_m1))

saveRDS(pontuacao, "artifacts/pontuacao_total_m1_2021.rds")

### MSE ----

mse_m1 <- data.frame(mse_y1 = mse(brasileirao_2021_with_pred$y1, 
                                  brasileirao_2021_with_pred$y1_pred_m1),
                     mse_y2 = mse(brasileirao_2021_with_pred$y2, 
                                  brasileirao_2021_with_pred$y2_pred_m1))

### LOO ----

log_lik_m1 <- extract_log_lik(m1_fit, parameter_name = "log_lik")
r_eff_m1 <- relative_eff(exp(log_lik_m1), chain_id = rep(1, 2500))
loo_m1 <- loo(log_lik_m1, r_eff = r_eff_m1)
waic_m1 <- waic(log_lik_m1)