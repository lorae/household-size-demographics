# benchmark/matrix-vs-survey-exact.R
#
# ❌ Note: This check currently fails — and that's instructive.
# ✅ See matrix-lm-fallback-vs-survey-ses.R for the fixed version.
#
# Last updated: 2025-07-07
#
# Summary:
# This script attempts to verify that our custom matrix-based regression,
# combined with bootstrap SEs from replicate weights, exactly reproduces the
# results from `svyglm` on 2019 IPUMS data.
#
# Why it fails:
# In ~1% of cases, the matrix backend fails due to singularity or non-positive-definite
# weight matrices (e.g., when all weights for a factor level are 0). This produces NaNs
# in the output and propagates through to the standard error calculation.
#
# Guidance:
# - If you rerun this script, you may *not* reproduce the failure. It depends on the sample.
# - To find a failing example, set `force = TRUE` in Step 1 until the error appears.
# - Once a failing sample is cached, set `force = FALSE` to keep it stable.
#
# Fix:
# The `dataduck_matrix_lm_fallback()` function in regression-backends.R handles this issue
# by defaulting to fast matrix algebra, but falling back to `lm()` if NaNs or errors are detected.
#
# For a corrected pipeline that uses this fallback, see:
# → benchmark/matrix-lm-fallback-vs-survey-ses.R

cat("
This script benchmarks whether the custom dataduck matrix-based regression and 
successive differences replication on 2019 data exactly reproduces the point 
estimates and standard errors from svyglm. It assumes that the replicate weights 
may contain negative values and does NOT modify them. This tests whether the matrix 
solver is a true drop-in replacement for survey design-based inference.
\n")

# ----- Step 0: User settings
# Define tolerance level of tests (two numbers count as equal if they are within
# `tol` of each other)
tol <- 1e-10

# Define number of strata to pull from 2019 data. Total data contains 2000 strata.
# 3 is the smallest I'd recommend; 200 is the largest I'd recommend for a run that
# doesn't take longer than 30 minutes.
n_strata <- 3

# Define regression formula
formula <- BEDROOMS ~ -1 + 
  tenure

# ----- Step 1: Config ----- #

library(survey)
library(tictoc)
library(dplyr)
library(glue)
library(purrr)
library(devtools)

# dataduck internal package & helper scripts
# TODO: will someday be added to dataduck
load_all("../dataduck")
source("src/utils/create-benchmark-data.R")
source("src/utils/regression-backends.R")

# ----- step 1: Load and prepare sample ----- #
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# ----- step 2: Compute benchmark model using svyglm ----- #
tic("create survey design")
design_2019_expected <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_2019_sample_tb
) |> subset(GQ %in% c(0, 1, 2))
toc()

tic("fit svyglm")
model_expected <- svyglm(formula, design = design_2019_expected)
toc()

expected_coefs <- tibble(
  term = names(model_expected$coefficients),
  estimate = as.numeric(model_expected$coefficients)
) |> arrange(term)

expected_ses <- summary(model_expected)$coefficients[, "Std. Error"] |> unname()

# ----- step 3: Run matrix regression and compare results ----- #

model_actual <- dataduck_reg_matrix_2(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula
)

stopifnot(all.equal(model_actual$estimate, expected_coefs$estimate, tolerance = tol))
message(glue("\u2705 Coefficients match within {tol}."))

# ----- step 4: Bootstrap SEs using unmodified replicate weights ----- #

input_bootstrap <- bootstrap_replicates(
  data = filtered_tb,
  f = dataduck_reg_matrix_2,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  id_cols = "term",
  formula = formula,
  verbose = TRUE
)

model_output <- se_from_bootstrap(
  bootstrap = input_bootstrap,
  constant = 4 / 80,
  se_cols = c("estimate")
)

actual_ses <- model_output$se_estimate

all_equal_helper <- function(x, y, tol = 1e-8) {
  if (length(x) != length(y)) {
    message("Length mismatch.")
    return(FALSE)
  }
  if (any(is.na(x) | is.na(y))) {
    message("NA values found.")
    return(FALSE)
  }
  if (any(is.nan(x) | is.nan(y))) {
    message("NaN values found.")
    return(FALSE)
  }
  all(abs(sort(x) - sort(y)) < tol)
}

stopifnot(all_equal_helper(actual_ses, expected_ses, tol = tol))

# There's an error: they don't match
input_bootstrap
# Looking at this, it's clear one of the replicate estimates has NaN. This
# propagates up to mess up the entire bootstrap replicate.
# I think it has something to do with the underlying regression function that I
# use here. For now, I am abandoning this method.
# TODO: determine why one (sometimes multiple, depends on the sample) bootstrap
# replicate runs produces NaN estimates.
# TODO: add a type check in dataduck for imports that stop user if any of the 
# replicate estimates are NaN, NULL, etc.

message(glue("\u2705 Standard errors match within {tol}."))
