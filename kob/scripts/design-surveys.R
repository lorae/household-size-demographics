# kob/refactor/design-surveys.R
# 
# Update: This script also saves 2019 data in a tibble so that
# multiple scripts can read data and perform analysis simultaneously, which is
# not possible in a duckDB.
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

# Note: This script takes about 1.5 hours to run. Each survey design takes about
# 3 minutes; each save takes about 10 minutes.

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Collect 2000 data into memory
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

# Collect 2019 data into memory
ipums_2019_tb <- ipums_db |>
  filter(YEAR == 2019) |>
  collect()

# Design the 2019 survey
tic("Design the 2019 survey")
design_2019_survey <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",  # regex pattern to match columns
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_2019_tb
)
design_2019_survey <- subset(design_2019_survey, GQ %in% c(0, 1, 2))
toc()

# Save the 2019 survey
# Note: this took 47 minutes last time
tic("Save 2019 survey design as RDS")
saveRDS(design_2019_survey, file = "kob/throughput/design_2019_survey.rds")
toc()

# Save the 2019 raw tibble
# Note: This took 25 minutes last time
tic("Save the 2019 raw tibble, unfiltered")
saveRDS(ipums_2019_tb, file = "kob/throughput/ipums_2019_tb.rds")
toc()

