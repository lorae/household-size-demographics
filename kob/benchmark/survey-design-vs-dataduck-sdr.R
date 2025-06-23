# kob/refactor/benchmark/survey-design-db-vs-tb.R
# The purpose of this script is to test whether the output from my `bootstrap_replicates`
# and `se_from_bootstrap` functions match the SEs from the widely accepted 
# `survey` package
# Update: they do!! 🎉
# TODO: add this benchmark to dataduck itself
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
# TODO: this doesn't affect slice_sample. Why?
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
# Now that we've validated the main point estimates match, we test whether the 
# standard errors from `se_from_bootstrap()` match those from `svyglm` / `svyby`.
# If so, this could spell out large performance enhancements.

input_bootstrap <- bootstrap_replicates(
  data = ipums_2019_sample_tb |> filter(GQ %in% c(0,1,2)),
  f = hhsize_by_tenure,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  hhsize_col = "NUMPREC",
  id_cols = "tenure"
)

model_output <- se_from_bootstrap(
  bootstrap = input_bootstrap,
  constant = 4/80,
  se_cols = c("weighted_mean")
)
model_output

summary(model_expected)

# ----- Final Check: Validate against svyglm output ----- #
# Extract SEs from svyglm
expected_ses <- summary(model_expected)$coefficients[, "Std. Error"]
print(expected_ses)

# Match to your custom output
actual_ses <- model_output$se_weighted_mean
print(actual_ses)

# Confirm match within tolerance
stopifnot(all.equal(actual_ses, expected_ses, tolerance = 1e-6, check.attributes = FALSE))

message("✅ Standard errors match within tolerance.")
