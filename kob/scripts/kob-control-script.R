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

# ----- Step 1: Load in data from kob-prepare-data.R ----- #
source("kob/scripts/kob-prepare-data.R") # coef is the key output here

# ----- Step 2: Run the kob analysis ----- #
source("kob/scripts/kob-function.R") # defines the `kob` function

# This is for validation. The difference in mean hhsize from 2000 to 2019 should
# exactly equal the sum of the u, c, and e components.
aggregates <- readRDS("throughput/aggregates.rds")

# Apply the kob function
kob_output <- kob(coef)
kob_output

# Validate whether the sum of the outputs matches the validation diff
output_diff <- kob_output$components |>
  unlist() |>
  sum()

# No output means the test passed (i.e. kob worked!)
testthat::expect_equal(output_diff, as.numeric(validation_diff), tolerance = 1e-8)


# ----- Step 3: Save and graph ----- #
# For now, I'm not running this script since I don't want to save these results
# anywhere while I test and refactor on benchmark data

# source("kob/scripts/kob-graphs-tables.R")
