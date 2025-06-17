# kob/refactor/benchmark/parallelize.R
#
# The purpose of this file is to test the extent to which parallelizing using furrr
# can speed up computation
#

# ----- STEP 0: Config ----- #

library(srvyr)
library(survey)
library(tictoc)
library(testthat)
library(duckdb)
library(dplyr)
library(ggplot2)
library(furrr)

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Pseudorandom seed
set.seed(123)

# ----- BENCHMARK 4: parallelize ----- #
# --- Step 4a: sample the data
# I randomly select 20 strata. 

strata_summary <- ipums_db |> 
  filter(YEAR == 2000) |>
  select(STRATA, CLUSTER) |>
  distinct() |> 
  collect()

sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |>  # Ensure stratum has at least 2 PSUs (they always will)
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 20) # Predictable sample due to pseudorandom seed, set in config

sampled_strata_vec <- sampled_strata$STRATA

# Now filter
ipums_2000_tb <- ipums_db |>
  filter(YEAR == 2000, STRATA %in% sampled_strata_vec) |>
  collect()

# --- Step 4b: design the survey
design_2000_survey_large <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  nest = TRUE
) |>
  subset(GQ %in% c(0, 1, 2))

# --- Step 4c: Run non-parallelized operations
tic("Non-parallelized")

toc()
