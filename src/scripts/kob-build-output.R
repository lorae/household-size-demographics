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

# --- Number of Persons (p) ---
kob_p <- kob(kob_input$numprec) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_p,
  mean_2000 = aggregates |> filter(abbrev_variable == "p") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "p") |> pull(mean_2019)
)


# --- Number of Bedrooms (b) ---
kob_b <- kob(kob_input$bedroom) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_b,
  mean_2000 = aggregates |> filter(abbrev_variable == "b") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "b") |> pull(mean_2019)
)

# --- Number of Rooms (r) ---
kob_r <- kob(kob_input$room) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_r,
  mean_2000 = aggregates |> filter(abbrev_variable == "r") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "r") |> pull(mean_2019)
)

# --- Persons per Room (ppr) ---
kob_ppr <- kob(kob_input$ppr) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppr,
  mean_2000 = aggregates |> filter(abbrev_variable == "ppr") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(abbrev_variable == "ppr") |> pull(mean_2019)
)

# --- Persons per Bedroom (ppbr) ---
kob_ppbr <- kob(kob_input$ppbr) |>
  kob_tidy_output() |>
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

# TODO: separate reg output into a subfolder in throughput to make these main
# throughput files easier to find
saveRDS(kob_output, "throughput/kob_output.rds")

