# kob/refactor/benchmark/survey-design-db-vs-tb.R
# The purpose of this script is to test whether the output from my `bootstrap_replicates`
# and `se_from_bootstrap` functions match the SEs from the widely accepted 
# `survey` package
#
# ----- STEP 0: Setup and config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(duckdb)
library(glue)
library(dplyr)
library(purrr)

# Load the dataduck package
devtools::load_all("../dataduck")

# Initialize a subset of data to write to a database connection\
# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
benchmark_con <- dbConnect(duckdb::duckdb(), "data/db/benchmark.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Pseudorandom seed
set.seed(123)

# ----- STEP 1: Load and Prepare Benchmark Sample -----
# This section creates two variables:
# - `ipums_2019_sample_tb`: an in-memory tibble with a 1-million row sample of data
# - `ipums_2019_sample_db`: a duckdb lazy table with the identical 1-million row sample
#
# TODO: migrate this entire process into process-ipums.R, so that the benchmark
# dataset is made at the same time (and uploaded to a separate duckdb connection)
# at the same time as the main db.
# TODO: Then refactor all the benchmark tests to read from this db (and convert into
# a tibble as needed, within the test).
# TODO: rename the table in this benchmark db to _2000, and create a _2019 table
# in the same db file so that code can be benchmarked on both surveys, which have
# different design variables (and thus different methods for calculating SEs)

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
benchmark_con <- dbConnect(duckdb::duckdb(), "data/db/benchmark.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Sample 5 strata from 2019 data where GQ ∈ [0,1,2] and each stratum has ≥ 2 PSUs
strata_summary <- ipums_db |> 
  filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |> 
  distinct(STRATA, CLUSTER) |> 
  collect()

sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |> 
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 5)

# Collect the sampled data
ipums_2019_sample_tb <- ipums_db |> 
  filter(YEAR == 2019, STRATA %in% !!sampled_strata$STRATA) |> 
  collect()

# Write the benchmark data to a secondary DuckDB connection
copy_to(benchmark_con, ipums_2019_sample_tb, "ipums_sample", overwrite = TRUE)
ipums_2019_sample_db <- tbl(benchmark_con, "ipums_sample")

# Optional sanity check
ipums_2019_sample_db_check <- ipums_2019_sample_db |> collect()
stopifnot(all.equal(
  ipums_2019_sample_tb |> arrange(pers_id),
  ipums_2019_sample_db_check |> arrange(pers_id)
))

# ----- Step 2: Create Survey Design and Benchmark Output ----- #
# Build a survey design object using replicate weights
tic("Create survey design")
design_2019_expected <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_2019_sample_tb
) |> subset(GQ %in% c(0, 1, 2))
toc()

# Compute benchmark mean household size by tenure
tic("Benchmark: svyglm and svyby")
model_expected <- svyglm(NUMPREC ~ -1 + tenure, design = design_2019_expected)
benchmark_means <- svyby(~NUMPREC, ~tenure, design = design_2019_expected, svymean)
toc()

# ----- STEP 3: Run Custom Point Estimate Pipeline ----- #
# Filter for matching GQ criteria
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# Custom weighted mean function
hhsize_by_tenure <- function(data, wt_col, hhsize_col) {
  data |> group_by(tenure) |> summarize(
    weighted_mean = sum(.data[[hhsize_col]] * .data[[wt_col]]) / sum(.data[[wt_col]]),
    .groups = "drop"
  )
}

# Run custom estimate
actual_v1 <- hhsize_by_tenure(
  data = filtered_tb,
  wt_col = "PERWT",
  hhsize_col = "NUMPREC"
)

# Confirm that means match
stopifnot(all.equal(
  actual_v1$weighted_mean,
  unname(model_expected$coefficients),
  tolerance = 1e-6
))

# ----- STEP 4: Compare SEs from custom pipeline vs survey package ----- #
# This is the main performance comparison. We test whether the standard errors
# from `se_from_bootstrap()` match those from `svyglm` / `svyby`, which would
# validate our faster alternative approach.

##### --- end of refactor so far



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


# --------------


# Input data
input_data <- tibble(
  per_id = c(1, 2, 3, 4, 5),
  sex = c(1, 0, 1, 1, 0),
  hhsize = c(2, 3, 2, 1, 1),
  weight = c(10, 12, 15, 30, 20),
  repwt1 = c(11, 13, 16, 28, 22),
  repwt2 = c(8, 8, 16, 25, 22),
  repwt3 = c(2, 4, 10, 14, 13),
  repwt4 = c(18, 17, 11, 25, 15)
)

# Initialize two test functions
hhsize_by_sex <- function(
    data,
    wt_col, # string name of weight column in `data`
    hhsize_col # string name of hhsize column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      weighted_mean = sum(.data[[hhsize_col]] * .data[[wt_col]], na.rm = TRUE)/sum(.data[[wt_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(result)
}

count_by_sex <- function(
    data,
    wt_col # string name of weight column in `data`
) {
  result <- data |>
    group_by(sex) |>
    summarize(
      count = n(),
      weighted_count = sum(.data[[wt_col]]),
      .groups = "drop"
    )
  
  return(result)
}

# Produce two test bootstrap_replicates input objects
input_bootstrap_count <- bootstrap_replicates(
  data = input_data,
  f = count_by_sex,
  wt_col = "weight",
  repwt_cols = paste0("repwt", 1:4),
  id_cols = "sex"
)

input_bootstrap_hhsize <- bootstrap_replicates(
  data = input_data,
  f = hhsize_by_sex,
  wt_col = "weight",
  repwt_cols = paste0("repwt", 1:4),
  hhsize_col = "hhsize",
  id_cols = "sex"
)

# Expected results
expected_count <- tibble(
  sex = c(0, 1),
  count = c(2, 3),
  weighted_count = c(32, 55),
  se_weighted_count = c(15.42725, 29.63106)
)

expected_hhsize <- tibble(
  sex = c(0, 1),
  weighted_mean = c(1.75, 1.454545),
  se_weighted_mean = c(0.47193501, 0.09704985)
)

test_that("se_from_bootstrap produces correct results on count_by_sex", {
  output_count <- se_from_bootstrap(
    bootstrap = input_bootstrap_count,
    constant = 1,
    se_cols = c("weighted_count")
  )
  
  # Round decimals to avoid floating point mismatch
  output_count <- output_count |>
    mutate(se_weighted_count = round(se_weighted_count, 5))
  expected_count <- expected_count |>
    mutate(se_weighted_count = round(se_weighted_count, 5))
  
  expect_equal(output_count, expected_count)
})

test_that("se_from_bootstrap produces correct results on hhsize_by_sex", {
  output_hhsize <- se_from_bootstrap(
    bootstrap = input_bootstrap_hhsize,
    constant = 1,
    se_cols = c("weighted_mean")
  )
  
  # Round decimals to avoid floating point mismatch
  output_hhsize <- output_hhsize |>
    mutate(
      se_weighted_mean = round(se_weighted_mean, 4),
      weighted_mean = round(weighted_mean, 4)
    )
  expected_hhsize <- expected_hhsize |>
    mutate(
      se_weighted_mean = round(se_weighted_mean, 4),
      weighted_mean = round(weighted_mean, 4)
    )
  
  expect_equal(output_hhsize, expected_hhsize)
})