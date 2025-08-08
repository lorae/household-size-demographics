# Paired bar chart by subgroup and year
plot_year_subgroup_bars <- function(
    data,
    yvar,
    bar_fills,   # named list: per1 (2000), per2 (2019)
    ymin = NULL,
    ymax = NULL,
    ytitle = "Average Household Size",
    legend = TRUE,
    title = NULL
) {
  # Extract year styling
  fill_colors <- c("2000" = bar_fills$per1$color, "2019" = bar_fills$per2$color)
  border_colors <- fill_colors
  line_types <- c("2000" = bar_fills$per1$line_type, "2019" = bar_fills$per2$line_type)
  alphas <- c("2000" = bar_fills$per1$alpha, "2019" = bar_fills$per2$alpha)
  
  # Add year as a factor for consistent ordering
  data <- data |>
    mutate(year_label = factor(as.character(year), levels = c("2000", "2019")))
  
  p <- ggplot(data, aes(x = subgroup, y = {{ yvar }})) +
    geom_bar(
      aes(fill = year_label, color = year_label, linetype = year_label),
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.8,
      size = 0.4,
      alpha = NA  # set below with scale_fill_manual for per-year alpha
    ) +
    geom_text(
      aes(x = subgroup, label = round({{ yvar }}, 2), group = year),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3
    ) +
    scale_fill_manual(
      values = c(
        "2000" = alpha(bar_fills$per1$color, bar_fills$per1$alpha),
        "2019" = alpha(bar_fills$per2$color, bar_fills$per2$alpha)
      ),
      guide = if (legend) "legend" else "none"
    ) +
    scale_color_manual(
      values = border_colors,
      guide = if (legend) "legend" else "none"
    ) +
    scale_linetype_manual(
      values = line_types,
      guide = if (legend) "legend" else "none"
    ) +
    labs(
      y = ytitle,
      title = title,
      fill = NULL, color = NULL, linetype = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = if (legend) "bottom" else "none",
      legend.box = "horizontal",
      axis.title.x = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
      plot.title = element_text(hjust = 0.5)
    )
  
  if (!is.null(ymin) || !is.null(ymax)) {
    p <- p + coord_cartesian(ylim = c(ymin %||% -Inf, ymax %||% Inf))
  }
  
  return(p)
}


# USed for fig06 and fg-fig03
# Observed vs counterfactual bars
# row argument must be a df with one row containing
# name (will become plot title), observed_2000, observed_2019, expected_2019
make_observed_cf_barplot <- function(row, yaxis_override = NULL, label_format = "%.3f") {
  
  # Validate yaxis_override if provided
  if (!is.null(yaxis_override)) {
    if (!is.numeric(yaxis_override) || length(yaxis_override) != 2) {
      stop("yaxis_override must be a numeric vector of length 2.")
    }
    if (yaxis_override[1] >= yaxis_override[2]) {
      stop("The first element of yaxis_override must be less than the second.")
    }
    ylim_vals <- yaxis_override
  } else {
    ylim_vals <- c(row$ymin, row$ymax)
  }
  
  # Build plot data
  fig_data <- tibble::tibble(
    Category = factor(
      c("2000\nObserved", "2019\nObserved", "2019\nExpected"),
      levels = c("2000\nObserved", "2019\nObserved", "2019\nExpected")
    ),
    Household_Size = c(row$observed_2000, row$observed_2019, row$expected_2019),
    Type = c("Observed", "Observed", "Expected")
  )
  
  # Create and return the plot
  ggplot(fig_data, aes(x = Category, y = Household_Size, fill = Type, linetype = Type)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.2, width = 0.6) +
    geom_text(aes(label = sprintf(label_format, Household_Size)), vjust = 1.5, color = "white", size = 4) +
    scale_fill_manual(values = c(
      "Observed" = "steelblue", 
      "Expected" = scales::alpha("steelblue", 0.5)
    )) +
    scale_linetype_manual(values = c(
      "Observed" = "solid", 
      "Expected" = "dotted"
    )) +
    labs(
      title = row$name,
      y = NULL, x = NULL
    ) +
    coord_cartesian(ylim = ylim_vals) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 11, margin = margin(t = 5)),
      plot.title = element_text(size = 13, margin = margin(b = 10))
    )
}