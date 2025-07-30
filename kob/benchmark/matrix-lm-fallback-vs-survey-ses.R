# kob/benchmark/matrix-lm-fallback-vs-survey-ses.R
#
# ✅ This script successfully reproduces `svyglm` point estimates and standard errors
# using the dataduck matrix backend *with fallback* to `lm()` when matrix inversion fails.
#
# Last updated: 2025-07-07
#
# Summary:
# This is the corrected version of `matrix-vs-survey-exact.R`. It benchmarks whether
# our matrix-based regression backend, combined with successive differences replication,
# reproduces the estimates and standard errors from `svyglm` on 2019 IPUMS data.
#
# Why this works:
# The `dataduck_matrix_lm_fallback()` function checks each regression result for
# NaNs or matrix singularity errors. When detected, it reruns the model using `lm()`,
# which is more robust but slower. This fallback ensures that all replicate models succeed,
# so that the bootstrap standard error calculation is not corrupted.
#
# Sample reproducibility:
# - The benchmark sample is cached locally in `kob/cache/` after creation.
# - Set `force = TRUE` under Step 1 to regenerate a new random sample.
#
# Performance:
# On small samples (e.g., `n_strata = 3`), the runtime is typically under 2 minutes.
# For larger samples, matrix fallback is used sparingly (~1–2%), minimizing performance loss.
#
# Related scripts:
# ❌ `matrix-vs-survey-exact.R` — shows failure when fallback is not used
# ✅ `this script` — uses matrix backend with lm() fallback


cat("
This script benchmarks whether the custom dataduck matrix-based regression and 
successive differences replication on 2019 data reproduces the point 
estimates and standard errors from svyglm. It defaults to a matrix algebra regression
function that does not modify replicate weights. In the rare event of failure, it 
falls back to the lm method by first zeroing negative replicate weights and then re
running. This should have minimal effect on the point estimate, as this fallback 
only occurs roughly 1-2% of the time. This tests whether the pipeline is a true 
drop-in replacement for survey design-based inference.
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

ipums_2019_sample_tb <- readRDS(glue("kob/cache/benchmark_sample_2019_{n_strata}/tb.rds"))
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

model_actual <- dataduck_reg_matrix(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula
)

stopifnot(all.equal(model_actual$estimate, expected_coefs$estimate, tolerance = tol))
message(glue("\u2705 Coefficients match within {tol}."))

# ----- step 4: Bootstrap SEs using unmodified replicate weights ----- #

input_bootstrap <- bootstrap_replicates(
  data = filtered_tb,
  f = dataduck_matrix_lm_fallback, 
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

# Optional: look at the input bootstrap.
#input_bootstrap

message(glue("\u2705 Standard errors match within {tol}."))
