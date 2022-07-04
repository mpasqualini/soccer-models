library(bayesplot)
library(data.table)
library(gridExtra)
library(posterior)
library(tidyverse)
library(rstan)

gera_dados_poisson_hierarquico <- function(nt, home, mu_att, mu_def,  sigma_att, sigma_def) {
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

nt <- 20
home <- 0.21
mu_att <- 0.42
mu_def <- -0.33
sigma_att <- 0.92
sigma_def <- 0.89

dados <- gera_dados_poisson_hierarquico(nt, home, mu_att, mu_def, sigma_att, sigma_def)

game_points <- dados |> 
  mutate(home_team_points = case_when(y1 > y2 ~ 3,
                                      y1 < y2 ~ 0,
                                      y1 == y2 ~ 1), 
         away_team_points = case_when(y2 > y1 ~ 3, 
                                      y2 < y1 ~ 0,
                                      y2 == y1 ~ 1
         ))

home <- game_points |> 
  select(h, home_team_points) |> 
  rename(team_id = h, points_scored = home_team_points)

away <- game_points |> 
  select(a, away_team_points) |> 
  rename(team_id = a, points_scored = away_team_points)

teams_points <- rbind(home, away) |> 
  group_by(team_id) |> 
  mutate(game_id = row_number())

teams_points |> 
  group_by(team_id) |> 
  mutate(cumsum = cumsum(points_scored)) |> 
  ggplot(aes(x = game_id, y = cumsum)) +
  geom_line() +
  facet_wrap(~team_id)
