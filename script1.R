library(bayesplot)
library(data.table)
library(tidyverse)
library(rstan)

data_2019_raw <- fread("2019_partidas.csv")

ngames <- nrow(data_2019_raw)
nteams <- data_2019_raw[ , uniqueN(home_team)]

team_ids <- data.frame("team_original_id" = data_2019_raw[ , sort(unique(home_team))],
                       "team_index" = 1:20)

data_2019_long <- melt(data_2019_raw, 
                       measure.vars = c("home_team", "away_team"), 
                       value.name = "team_index")

data_2019 = data_2019_raw
data_2019[ , `:=` (home_team_index = match(data_2019_raw$home_team, 
                                                 team_ids$team_original_id), 
                     away_team_index = match(data_2019_raw$away_team, 
                                             team_ids$team_original_id))
]


ngames = nrow(dados)

data_2019_list <- list(G = ngames,
                       T = 20,
                       h = dados[,2],
                       a = dados[,3],
                       y1 = dados[,4],
                       y2 = dados[,5],
                       C=3
                       )

# "One short chain for debug"
#fit_modelo = stan(file = "poisson_hierarchical_proposto.stan", 
#                  data = data_2019_list,
#                  chains = 1,
#                  iter = 2000
#                  )

fit_modelo = stan(file = "models/modelo3-mixture-rascunho4.stan", 
                  data = data_2019_list,
                  chains = 2, thin = 5,
                  iter = 10000
)
summary(fit_modelo)

fit_modelo %>%
  rhat() %>%
  mcmc_rhat() +
  yaxis_text()

traceplot(fit_modelo)
shinystan::launch_shinystan(fit_modelo)
