library(bayesplot)
library(data.table)
library(gridExtra)
library(posterior)
library(tidyverse)
library(rstan)

source("src/sim_functions.R")
source("src/wrangling_functions.R")
source("src/plot_functions.R")

nt <- 20
home <- 0.21
mu_att <- 0.42
mu_def <- -0.33
sigma_att <- 0.92
sigma_def <- 0.89

simulated_data <- generate_hierarchical_poisson_model(nt, home, mu_att, mu_def, sigma_att, sigma_def)
points_per_team <- get_team_points_per_game(simulated_data)
create_cumsum_points_plot(points_per_team)
