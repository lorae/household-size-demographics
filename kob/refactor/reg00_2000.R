# kob/refactor/reg00_2000
# This script runs the props for regression 00 in 2000.

# TODO: explore whether I can generate a survey design without pulling data into
# memory. Example code below.
# TODO: add a test comparing outputs and computation time from database and non-database
# to see if it works / is better.
# design_db <- svydesign(
#   ids    = ~CLUSTER,
#   strata = ~STRATA,
#   weights= ~PERWT,
#   data   = "ipums_processed",
#   dbtype = "DuckDB",
#   dbname = "data/db/ipums.duckdb",
#   nest   = TRUE
# ) %>%
#   subset(GQ %in% c(0,1,2))
# # documentation mentioning database driver:
# # https://www.rdocumentation.org/packages/survey/versions/4.4-2/topics/svydesign

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Read in the pre-subsetted survey
tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("kob/throughput/design_2000_survey.rds")
toc()