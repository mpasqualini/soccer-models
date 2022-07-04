create_cumsum_points_plot <- function(data) {
  plot <- data |> 
    group_by(team_id) |> 
    mutate(cumsum = cumsum(points_scored)) |> 
    ggplot(aes(x = game_id, y = cumsum)) +
    geom_line() +
    facet_wrap(~team_id)
  
  return(plot)
}