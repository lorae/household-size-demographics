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
source("src/utils/plotting-tools.R") # defines `make_observed_cf_barplot()`

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
p <- make_observed_cf_barplot("Number of Persons", fig06_data, yaxis_override = c(3, 3.5))
b <- make_observed_cf_barplot("Number of Bedrooms", fig06_data, yaxis_override = c(2, 3.5))
ppbr <- make_observed_cf_barplot("Persons per Bedroom", fig06_data, yaxis_override = c(1, 1.5))
r <- make_observed_cf_barplot("Number of Rooms", fig06_data, yaxis_override = c(5.5, 6.5))
ppr <- make_observed_cf_barplot("Persons per Room", fig06_data, yaxis_override = c(0, 1))

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

