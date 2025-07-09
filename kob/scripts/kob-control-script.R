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

source("src/utils/regression-tools.R") # add_intercept function

# ----- Step 1: Load in data ----- #
# Only run this if kob_input.rds doesn't already exist
if (!file.exists("throughput/kob_input.rds")) {
  source("kob/scripts/kob-prepare-data.R")
}

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
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

# Validate against aggregates
kob_output_validate(
  kob_bedroom,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)

# ----- Step 3: Graph ----- #
# Define a function to plot the decomp
plot_kob_decomposition <- function(kob_output, varnames,
                                   title = "KOB Decomposition: Bedrooms with Intercept",
                                   show_total = TRUE) {
  # Collapse into one row per variable
  collapsed <- kob_output |>
    filter(variable %in% varnames) |> 
    group_by(variable) |>
    summarise(
      across(starts_with("coef_"), ~ NA_real_),
      across(c("prop_2000", "prop_2019", "u", "e", "c"), ~ sum(.x, na.rm = TRUE)),
      across(c("prop_2000_se", "prop_2019_se", "u_se", "e_se", "c_se"), ~ sqrt(sum(.x^2, na.rm = TRUE))),
      .groups = "drop"
    ) |>
    select(variable, e, e_se, c, c_se) |>
    pivot_longer(cols = c(e, c), names_to = "component", values_to = "estimate") |>
    mutate(
      se = if_else(component == "e", e_se, c_se),
      component = recode(component, e = "Endowments", c = "Coefficients"),
      component = factor(component, levels = c("Coefficients", "Endowments"))
    ) |>
    select(-e_se, -c_se)
  
  # Add intercept row if present
  if ("(Intercept)" %in% kob_output$term) {
    intercept_row <- kob_output |>
      filter(term == "(Intercept)") |>
      summarise(
        variable = "Intercept",
        component = "Intercept",
        estimate = sum(u, na.rm = TRUE),
        se = sqrt(sum(u_se^2, na.rm = TRUE)),
        .groups = "drop"
      )
    collapsed <- bind_rows(collapsed, intercept_row)
  }
  
  # Add total row if requested
  if (show_total) {
    total_row <- collapsed |>
      summarise(
        variable = "Total",
        component = "Total",
        estimate = sum(estimate, na.rm = TRUE),
        se = NA_real_,  # Total uncertainty is often not shown
        .groups = "drop"
      )
    collapsed <- bind_rows(collapsed, total_row)
  }
  
  # Rescale intercept and total bars for readability
  intercept_scale <- 0.2 / max(abs(collapsed$estimate), na.rm = TRUE)
  plot_data <- collapsed |>
    mutate(
      estimate_rescaled = case_when(
        component == "Intercept" ~ estimate * intercept_scale,
        component == "Total" ~ estimate * intercept_scale,
        TRUE ~ estimate
      ),
      se_rescaled = case_when(
        component == "Intercept" ~ se * intercept_scale,
        component == "Total" ~ se * intercept_scale,
        TRUE ~ se
      ),
      variable = factor(variable, levels = rev(unique(variable)))
    )
  
  # Plot
  ggplot(plot_data, aes(x = estimate_rescaled, y = variable, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_errorbarh(
      aes(xmin = estimate_rescaled - se_rescaled,
          xmax = estimate_rescaled + se_rescaled),
      height = 0.25, color = "black", na.rm = TRUE
    ) +
    facet_grid(rows = vars(component), scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_manual(values = c(
      "Endowments" = "#56B4E9",
      "Coefficients" = "#E69F00",
      "Intercept" = "gray50",
      "Total" = "black"
    )) +
    theme_minimal(base_size = 13) +
    labs(
      title = title,
      x = "Contribution to Outcome Gap",
      y = NULL
    ) +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0),
      panel.spacing.y = unit(1, "lines"),
      legend.position = "none"
    )
}


plot_kob_decomposition(kob_bedroom_updated, varnames = varnames_dict, show_total = TRUE)

