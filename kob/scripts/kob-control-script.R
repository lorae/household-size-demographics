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

# TODO: refactor teh aggregates df so the variable names match the aliases in kob_input
# --- Bedroom ---
kob_bedroom <- kob(kob_input$bedroom) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_bedroom,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)

# --- Number of People ---
kob_numprec <- kob(kob_input$numprec) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_numprec,
  mean_2000 = aggregates |> filter(variable == "NUMPREC") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "NUMPREC") |> pull(mean_2019)
)

# --- Rooms ---
kob_room <- kob(kob_input$room) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_room,
  mean_2000 = aggregates |> filter(variable == "room") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "room") |> pull(mean_2019)
)

# --- Persons per Room (PPR) ---
kob_ppr <- kob(kob_input$ppr) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppr,
  mean_2000 = aggregates |> filter(variable == "persons_per_room") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "persons_per_room") |> pull(mean_2019)
)

# --- Persons per Bedroom (PPBR) ---
kob_ppbr <- kob(kob_input$ppbr) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppbr,
  mean_2000 = aggregates |> filter(variable == "persons_per_bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "persons_per_bedroom") |> pull(mean_2019)
)

# ----- Step 3: Graphs ----- #
# General: Create named list of KOB outputs and plot titles
kob_outputs <- list(
  "Bedrooms" = kob_bedroom,
  "Number of People" = kob_numprec,
  "Rooms" = kob_room,
  "Persons per Room" = kob_ppr,
  "Persons per Bedroom" = kob_ppbr
)

# --- 3.7: Figure 7 -  KOB decomposition bar charts
source("src/figures/fig07-kob-decomp-bars.R") # Defines functions needed for this plot
pretty_labels <- c(
  us_born = "U.S. born",
  tenure = "Tenure",
  RACE_ETH_bucket = "Race / Ethnicity",
  INCTOT_cpiu_2010_bucket = "Income",
  gender = "Sex",
  EDUC_bucket = "Education",
  cpuma = "CPUMA",
  AGE_bucket = "Age",
  Intercept = "Intercept",
  Total = "Total"
)

# Generate and print all five plots
plots <- imap(kob_outputs, ~{
  plot_data <- prepare_kob_plot_data(.x, varnames = varnames_dict, pretty_labels = pretty_labels)
  plot_kob_decomposition(plot_data, title = .y, show_total = TRUE)
})

# Optional: Display plots in RStudio
for (p in plots) print(p)



