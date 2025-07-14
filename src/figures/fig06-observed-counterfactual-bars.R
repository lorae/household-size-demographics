# #src/figures/fig06-observed-counterfactual-bars.R
# Produce bar charts showing 2000 observed, 2019 observed, and 2019 and expected 
# (counterfactual) outcomes.
#
# Input: throughput/kob_output.rds
# Output: output/figures/fig06-observed-counterfactual-bars.png, 
#         output/figures/fig06-appendix-observed-counterfactual-bars.png
#
# TODO: write unit tests for functions

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")

# ----- Step 1: Define functions -----
# Plotting function
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

# Rounding helpers
round_down_to <- function(x, base) base * floor(x / base)
round_up_to <- function(x, base) base * ceiling(x / base)

# ----- Step 2: Read in data, define parameters ----- #
kob_output <- readRDS("throughput/kob_output.rds")
aggregates <- readRDS("throughput/aggregates.rds")

# TODO: add e_se, c_se, and u_se (non-urgent unless needed; would be nice)
# TODO: move this into kob-build-output
kob_aggregates <- imap_dfr(kob_output, function(df, abbrev_variable) {
  df |>
    summarise(
      e = sum(e, na.rm = TRUE),
      c = sum(c, na.rm = TRUE),
      u = sum(u, na.rm = TRUE)
    ) |>
    mutate(abbrev_variable = abbrev_variable)
}) |>
  select(abbrev_variable, e, c, u)

# Rounding granularity for axis limits
increment <- 1  

# Create an observed, expected table used to produce these figures. Observed values
# in 2000 are already in `aggregates` - we rename those cols and add on a third 
# col using kob results
fig06_data <- aggregates |>
  rename(
    observed_2000 = mean_2000,
    observed_2019 = mean_2019
  ) |>
  left_join(kob_aggregates, by = "abbrev_variable") |>
  #relocate(name, .before = variable) |>
  rowwise() |>
  mutate(
    expected_2019 = observed_2000 + e,
    min_val = min(observed_2000, observed_2019, expected_2019, na.rm = TRUE),
    max_val = max(observed_2000, observed_2019, expected_2019, na.rm = TRUE),
    ymin = round_down_to(min_val, increment),
    ymax = round_up_to(max_val, increment)
  ) |>
  ungroup() |>
  relocate(expected_2019, .after = observed_2019) |>
  select(-min_val, -max_val)



# ----- Step 3: Make plots ----- #
p <- make_fig06_barplot("Number of People", fig06_data, yaxis_override = c(3, 3.5))
b <- make_fig06_barplot("Number of Bedrooms", fig06_data, yaxis_override = c(2, 3.5))
ppbr <- make_fig06_barplot("Persons per Bedroom", fig06_data, yaxis_override = c(1, 1.5))
r <- make_fig06_barplot("Number of Rooms", fig06_data, yaxis_override = c(5.5, 6.5))
ppr <- make_fig06_barplot("Persons per Room", fig06_data, yaxis_override = c(0, 1))

# Figure 6 shows # Persons, # Bedrooms, Persons per Bedroom
fig06 <- (p + b + ppbr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# Figure 6A (Appendix version) shows # Persons, # Rooms, Persons per Room
fig06a <- (p + r + ppr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/fig06-observed-counterfactual-bars.png", 
  plot = fig06, 
  width = 3000, height = 2400, units = "px", dpi = 300
)
ggsave(
  "output/figures/fig06-appendix-observed-counterfactual-bars.png", 
  plot = fig06a, 
  width = 3000, height = 2400, units = "px", dpi = 300
)

