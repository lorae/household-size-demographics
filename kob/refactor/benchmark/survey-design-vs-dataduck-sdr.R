# kob/refactor/benchmark/survey-design-db-vs-tb.R
# The purpose of this script is to test whether the output from my `bootstrap_replicates`
# and `se_from_bootstrap` functions match the SEs from the widely accepted 
# `survey` package
#
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(duckdb)
library(glue)

devtools::load_all("../dataduck")

# Initialize a subset of data to write to a database connection\
# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Pseudorandom seed
set.seed(123)

# --- STEP 1: Select a sample of data and save as tb and db
# This section creates two variables:
# - `ipums_2000_sample_tb`: an in-memory tibble with a 1-million row sample of data
# - `ipums_2000_sample_db`: a duckdb lazy table with the identical 1-million row sample
#
# TODO: migrate this entire process into process-ipums.R, so that the benchmark
# dataset is made at the same time (and uploaded to a separate duckdb connection)
# at the same time as the main db.
# TODO: Then refactor all the benchmark tests to read from this db (and convert into
# a tibble as needed, within the test).
# TODO: rename the table in this benchmark db to _2000, and create a _2019 table
# in the same db file so that code can be benchmarked on both surveys, which have
# different design variables (and thus different methods for calculating SEs)

# Select a sample including 5 strata from the underlying dataset
strata_summary <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |>
  select(STRATA, CLUSTER) |>
  distinct() |> 
  collect()

sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |>  # Ensure stratum has at least 2 PSUs (they always will)
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 5) # Predictable sample due to pseudorandom seed, set in config

# Collect the sample into memory
ipums_2000_sample_tb <- ipums_db |> 
  filter(YEAR == 2000, STRATA %in% !!sampled_strata$STRATA) |>
  collect()

# Write benchmark db to separate connection
# TODO: I do this by just loading from the in-memory sample db; this perhaps
# should be refactored to directly querying from the db when I transfer this process
# to process-ipums. Perhaps it's a different table or view in the same connection.
# For now, however, this does the trick.
copy_to(benchmark_con, ipums_2000_sample_tb, "ipums_sample", overwrite = TRUE)
ipums_2000_sample_db <- tbl(benchmark_con, "ipums_sample")

# Sanity_check: `ipums_2000_sample_tb` contains same data as the `ipums_2000_sample_db`
# TODO: remove this check (we know it works) or also add into process-ipums.R once
# code is migrated
ipums_2000_sample_db_check <- ipums_2000_sample_db |> collect()
all.equal( # must sort by `pers_id` before comparing, otherwise row order differs
  ipums_2000_sample_tb |> arrange(pers_id),
  ipums_2000_sample_db_check |> arrange(pers_id)
)

# ----- Step 2: Create survey designs for the tb and db ----- #
tic("Create survey design object from db sample")
drv <- survey::DuckDB() # Define the driver object

design_db <- svydesign(
  ids    = ~CLUSTER,
  strata = ~STRATA,
  weights= ~PERWT,
  data   = NULL,                    # must be NULL for a DB‐backed design
  dbname =  "data/db/benchmark.duckdb",
  dbtype =  drv,                    # pass the driver object
  table  = "ipums_sample",       
  nest   = TRUE
) |>
  subset(GQ %in% c(0,1,2))
toc()

tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("kob/throughput/design_2000_survey.rds")
toc()

# Calculate proportions
prop_vars <- c(
  "RACE_ETH_bucket", 
  "AGE_bucket", 
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket", 
  "us_born", 
  "tenure", 
  "gender",
  "cpuma"
)

# Convert to one-sided formulas: ~varname
formulas <- lapply(prop_vars, function(var) as.formula(paste0("~", var)))

# Set up parallel plan (this will use available cores)
options(future.globals.maxSize = 10 * 1024^3)
plan(multicore)

tic("Parallelized population proportions (forked)")
props00_2000 <- future_map(formulas, ~svymean(.x, design_2000_survey))
toc()

names(props00_2000) <- prop_vars

# Save results in throughput
tic("Save props00_2000 to kob/throughput")
saveRDS(props00_2000, file = "kob/throughput/props00_2000.rds")
toc()