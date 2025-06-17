# kob/refactor/design-surveys.R
# 
# The purpose of this script is to design the surveys and save that output so
# it doesn't have to be re-run each time a new regression is run.
# By my latest estimate, designing a survye takes about 3 minutes in 2000 and 
# TODO: fill in for 2019

# This script must be run before the kob/refactor/reg_*.R series of scripts, because
# they load in the .rds file that is the output of this script.

# Input
# TODO
# Output
# TODO

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

# Design the 2000 survey
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

# Save the 2000 survey
tic("Save 2000 survey design as RDS")
saveRDS(design_2000_survey, file = "kob/throughput/design_2000_survey.rds")
toc()

tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("kob/throughput/design_2000_survey.rds")
toc()