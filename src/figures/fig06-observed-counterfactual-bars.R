# #src/figures/fig06-observed-counterfactual-bars.R
# The purpose of this script is to define functions that produce bar charts showing
# 2000 observed, 2019 observed, and 2019 and expected (counterfactual) outcomes 
# of number of people in a household, number bedrooms in a household, and bedroom 
# crowding. Also an appendix version that does the same set of 3 bars by room.
#
# Input: various global vars for kob decomposition, such as kob_ppbr, defined in 
#        kob/scripts/kob-control-script.R
# Output: Functions defined below, called in kob/scripts/kob-control-script.R
#
# TODO: write a unit test or example script that does this. Also, have kob-control-script
# save the kob results before producing graphs, rather than relying on global vars.

make_fig06_barplot <- function(target_name, fig06_data, yaxis_override = NULL) {
  # Get the relevant row including ymin/ymax
  row <- fig06_data |> filter(name == target_name)
  
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
    geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
    scale_fill_manual(values = c(
      "Observed" = "steelblue", 
      "Expected" = scales::alpha("steelblue", 0.5)
    )) +
    scale_linetype_manual(values = c(
      "Observed" = "solid", 
      "Expected" = "dotted"
    )) +
    labs(
      title = target_name,
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
