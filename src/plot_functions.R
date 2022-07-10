create_cumsum_points_plot <- function(data) {
  plot <- data |> 
    group_by(team_id, team_name, name) |>
    arrange(game_id) |> 
    mutate(cumsum = cumsum(value)) |> 
    ggplot(aes(x = game_id, y = cumsum, color = name)) +
    geom_line() +
    facet_wrap(~team_name) +
    labs(x="", y="") +
    theme(legend.title=element_blank())
  
  return(plot)
}
