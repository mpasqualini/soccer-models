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