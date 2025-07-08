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

# ----- Step 1: Load in data ----- #
source("kob/scripts/kob-prepare-data.R") # only needs to run if kob_input.rds doesn't already exist
kob_input <- readRDS("throughput/kob_input.rds")

# This is for validation.
aggregates <- readRDS("throughput/aggregates.rds")

# ----- Step 2: Run the kob analysis ----- #
source("kob/scripts/kob-function.R") # defines the `kob` function and `kob-output-validate()`

# kob_input <- readRDS("throughput/kob_input.rds")
# 
# kob_output <- kob(kob_input$bedroom)
# 
# # Time to validate
# aggregates <- readRDS("throughput/aggregates.rds")

# kob_output_validate(
#   kob_output = kob_output,
#   mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
#   mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
# )

# ----- Step 3: Save and graph ----- #
# For now, I'm not running this script since I don't want to save these results
# anywhere while I test and refactor on benchmark data

# source("kob/scripts/kob-graphs-tables.R")
