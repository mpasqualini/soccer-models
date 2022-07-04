reindex_teams <- function(data) {
  data_long <- melt(data, 
                    measure.vars = c("home_team", "away_team"), 
                    value.name = "team_index")
  
  unique_teams_original_id <- data_long[ , sort(unique(team_index))]
  
  teams_dict <- data.frame(team_original_id = unique_teams_original_id, team_id = 1:20)
  
  data[ , `:=` (home_team_index = match(data$home_team, 
                                        teams_dict$team_original_id), 
                away_team_index = match(data$away_team, 
                                        teams_dict$team_original_id))
  ]
  
  return(data)
}

get_team_points_per_game <- function(data) {
  game_points <- data |> 
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
  
  return(teams_points)
}