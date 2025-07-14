# Paired bar chart by subgroup and year
plot_year_subgroup_bars <- function(
    data, 
    main_color, 
    ymin = NULL, 
    ymax = NULL, 
    legend = TRUE, 
    title = NULL
    ) {
  p <- ggplot(data, aes(x = subgroup, y = hhsize, fill = factor(year))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8),
             width = 0.8, color = "black") +
    geom_text(aes(label = round(hhsize, 2), group = year),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +
    scale_fill_manual(
      values = c("2000" = alpha(main_color, 0.4), "2019" = alpha(main_color, 0.8)),
      name = ""
    ) +
    labs(y = "Average Household Size") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = if (legend) "bottom" else "none",
      legend.box = "horizontal",
      axis.title.x = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
    )
  
  if (!is.null(title)) {
    p <- p + labs(title = title)
  }
  
  if (!is.null(ymin) || !is.null(ymax)) {
    p <- p + coord_cartesian(ylim = c(ymin %||% -Inf, ymax %||% Inf))
  }
  
  return(p)
}
