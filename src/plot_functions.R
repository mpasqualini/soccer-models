create_cumsum_points_plot <- function(data, linewidth=1, year) {

  plot <- data |> 
    group_by(team_id, team_name, name) |>
    arrange(game_id) |> 
    mutate(cumsum = cumsum(value)) |> 
    ggplot(aes(x = game_id, y = cumsum, color = name)) +
    geom_line(aes(linetype = ifelse(name == "points_scored_obs", "4", "1")), size = linewidth) +
    facet_wrap(~team_name) +
    labs(x="", y="", title = paste0("Campeonato Brasileiro ", year)) +
    theme(legend.title=element_blank()) +
    guides(linetype="none")
  
  return(plot)
}
