# src/scripts/kob-build-output.R
# 
# Ingest KOB input; apply KOB function, validate output, and save clean, reusable 
# throughput containing KOB results for downstream graphing, tabulation, and 
# description.
# 
# Input: throughput/kob_input.rds
# Output: throughput/kob_output.rds
#

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("oaxaca")
library("tibble")

source("src/utils/regression-tools.R") # add_intercept & split_term_column functions

# ----- Step 1: Load in data ----- #
# Only run this if kob_input.rds doesn't already exist
# TODO: eventually handle this with targets or document more clearly or use
# a large control script that runs the project from beginning to end
if (!file.exists("throughput/kob_input.rds")) {
  source("kob/scripts/kob-prepare-data.R")
}

kob_input <- readRDS("throughput/kob_input.rds")

# For validation (kob_output_validate)
aggregates <- readRDS("throughput/aggregates.rds")

# For tidying (split_term_column)
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
# Defines `kob()` and `kob-output-validate()`
source("kob/scripts/kob-function.R")

# --- Number of Persons (p) ---
kob_p <- kob(kob_input$numprec) |>
  split_term_column() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_p,
  mean_2000 = aggregates |> filter(abbrev_variable == "p") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "p") |> pull(mean_2019)
)


# --- Number of Bedrooms (b) ---
kob_b <- kob(kob_input$bedroom) |>
  split_term_column() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_b,
  mean_2000 = aggregates |> filter(abbrev_variable == "b") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "b") |> pull(mean_2019)
)

# --- Number of Rooms (r) ---
kob_r <- kob(kob_input$room) |>
  split_term_column() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_r,
  mean_2000 = aggregates |> filter(abbrev_variable == "r") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "r") |> pull(mean_2019)
)

# --- Persons per Room (ppr) ---
kob_ppr <- kob(kob_input$ppr) |>
  split_term_column() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppr,
  mean_2000 = aggregates |> filter(abbrev_variable == "ppr") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "ppr") |> pull(mean_2019)
)

# --- Persons per Bedroom (ppbr) ---
kob_ppbr <- kob(kob_input$ppbr) |>
  split_term_column() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppbr,
  mean_2000 = aggregates |> filter(abbrev_variable == "ppbr") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "ppbr") |> pull(mean_2019)
)

# ----- Step 3: Output data ----- #
# General: Create named list of KOB outputs and plot titles
kob_output <- list(
  p = kob_p, 
  b = kob_b, 
  r = kob_r, 
  ppr = kob_ppr, 
  ppbr = kob_ppbr
  )

saveRDS(kob_output, "throughput/kob_output.rds")

