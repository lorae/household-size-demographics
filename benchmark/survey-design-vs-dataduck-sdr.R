# benchmark/survey-design-vs-dataduck-sdr.R
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
library(purrr)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("src/utils/create-benchmark-data.R")

# ----- STEP 1: Load and Prepare Benchmark Sample -----
# This section creates two variables:
# - `ipums_2019_sample_tb`: an in-memory tibble with a 1-million row sample of data
# - `ipums_2019_sample_db`: a duckdb lazy table with the identical 1-million row sample
# It uses the create-benchmark-data function which is cache-aware: loads data from
# cache if available; if not, it regenerates.
#
# TODO: Refactor all the benchmark tests to use the create-benchmark-data function.
# This one is a good start.
# TODO: rename the table in this benchmark db to _2000, and create a _2019 table
# in the same db file so that code can be benchmarked on both surveys, which have
# different design variables (and thus different methods for calculating SEs)

# Custom function which is cache-aware: loads data from cache if available; if not, 
# it regenerates. File path to cache ("output_dir" arg) is the function default - 
# see source code for exact path.
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# Load the tibble. There's also a db available, but we ignore that for now.
ipums_2019_sample_tb <- readRDS("cache/benchmark_sample_2019_3/tb.rds")

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
