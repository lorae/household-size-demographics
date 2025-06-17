# kob/refactor/reg00_2000
# This script runs the props for regression 00 in 2000.

# TODO: explore whether I can generate a survey design without pulling data into
# memory. Example code below.
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

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Collect data into memory
ipums_2000_tb <- ipums_db |>
  # Don't need REPWT series of variables for the 2000 data: that's a 2019 survey
  # design variable used to calculate standard errors using successive differences
  # replication. This cuts out 80 columns, which will probably improve speed by 
  # about 10% according to my benchmarking.
  select(-starts_with("REPWT")) |>
  filter(YEAR == 2000) |> 
  collect()

# Design the survey
tic("Design the 2000 survey")
design_2000_survey <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  nest = TRUE
) |>
  subset(GQ %in% c(0, 1, 2))
toc()