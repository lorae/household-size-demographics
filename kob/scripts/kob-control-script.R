# kob/scripts/kob-control-script.R
# The purpose of this script is to run a full kob analysis by running two subtasks:
# 1. create an object suitable for input in the kob function using the 
#    kob-prepare-data.R script
# 2. put the output object from that script into the kob function, defined in
#    kob-function.R
# 3. Create graphs and tables of the results using a kob-graphs-tables.R
# TODO: functionalize the kob-prepare-data.R script
# TODO: functionalize the kob-graphs-tables.R script
# Both of these to-dos are essential to create a robust pipeline. The current 
# version (a work in progress) is fragile because it depends on environmental
# variables that are not well-documented in pass-offs from script-to-script.

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")
library("oaxaca")
library("tibble")
library(ggplot2)
library(dplyr)
library(tidyr)

# ----- Step 1: Load in data ----- #
source("kob/scripts/kob-prepare-data.R") # only needs to run if kob_input.rds doesn't already exist
kob_input <- readRDS("throughput/kob_input.rds")

# This is for validation (kob_output_validate)
aggregates <- readRDS("throughput/aggregates.rds")

# This is for tidying (kob_tidy_output)
varnames_dict <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

# ----- Step 2: Run the kob analysis ----- #
# Each section covers a separate outcome
source("kob/scripts/kob-function.R") # defines the `kob` function and `kob-output-validate()`

# --- bedroom outcome ---
# Produce the results
kob_bedroom <- kob(kob_input$bedroom) |>
  # Append `variable` and `value` columns
  kob_tidy_output()

# Validate against aggregates
kob_output_validate(
  kob_bedroom,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)

# Consolidate for graphing
# First pass at aggregating up estimates
kob_collapse_variable <- function(kob_output, variable) {
  kob_output |>
    filter(.data$variable == !!variable) |>
    summarise(
      across(starts_with("coef_"), ~ NA_real_),
      across(c("prop_2000", "prop_2019", "u", "e", "c"), ~ sum(.x, na.rm = TRUE)),
      across(c("prop_2000_se", "prop_2019_se", "u_se", "e_se", "c_se"), ~ sqrt(sum(.x^2, na.rm = TRUE)))
    )
}

# Creates a single row which summarizes across cpumas
kob_bedroom_cpuma_summary <- kob_collapse_variable(kob_bedroom, "cpuma")

# Run the function on all vars in varnames_dict and bind rows together
kob_collapsed_all <- map_dfr(varnames_dict, function(var) {
  kob_collapse_variable(kob_bedroom, var) |>
    mutate(variable = var, .before = 1)
}) |>
  select(variable, u, e, c, u_se, e_se, c_se)

# Validate against aggregates (again)
kob_output_validate(
  kob_collapsed_all,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)

# Prepare long-format data with bars
plot_data <- kob_collapsed_all |>
  select(variable, e, e_se, c, c_se) |>
  pivot_longer(cols = c(e, c), names_to = "component", values_to = "estimate") |>
  mutate(
    se = if_else(component == "e", e_se, c_se),
    component = recode(component, e = "Endowments", c = "Coefficients")
  ) |>
  select(-e_se, -c_se)

# To use bars, we need an explicit "width" for error bars
ggplot(plot_data, aes(x = estimate, y = variable, fill = component)) +
  geom_col(width = 0.6, position = "identity") +
  geom_errorbarh(
    aes(xmin = estimate - se, xmax = estimate + se),
    height = 0.2
  ) +
  facet_wrap(~component, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(
    x = "Contribution to Outcome Gap",
    y = NULL,
    title = "KOB Decomposition: # Bedrooms"
  ) +
  scale_fill_manual(values = c("Endowments" = "#56B4E9", "Coefficients" = "#E69F00")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ----- Step 3: Save and graph ----- #
# For now, I'm not running this script since I don't want to save these results
# anywhere while I test and refactor on benchmark data

# source("kob/scripts/kob-graphs-tables.R")
