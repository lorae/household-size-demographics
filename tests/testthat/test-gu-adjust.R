# tests/testthat/test-gu-adjust.R

# The purpose of this set of unit tests is to verify that `gu_adjust()` produces 
# consistent estimates regardless of the omitted category. 
# Also, this script confirms that the function correctly stops / produces warnings
# for faulty inputs.

# ----- Step 0: Setup -----
library(testthat)
library(dplyr)
library(glue)
library(devtools)

# Ensure working directory is project root
root <- rprojroot::find_root(rprojroot::is_rstudio_project)
setwd(root)

# Load dependencies
load_all("../dataduck")
source("src/utils/regression-backends.R")
source("src/utils/create-benchmark-data.R")
source("src/utils/regression-postprocess-tools.R")

# Load benchmark sample
n_strata <- 3
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# ----- Step 1: Run regressions with different omitted groups -----
formula <- NUMPREC ~ RACE_ETH_bucket

# AIAN omitted
data_AIAN <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "AIAN"))
reg_AIAN <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula) |> gu_adjust()

# AAPI omitted
data_AAPI <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "AAPI"))
reg_AAPI <- dataduck_reg_matrix_2(data = data_AAPI, wt_col = "PERWT", formula = formula) |> gu_adjust()

# Black omitted
data_Black <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "Black"))
reg_Black <- dataduck_reg_matrix_2(data = data_Black, wt_col = "PERWT", formula = formula) |> gu_adjust()

# ----- Step 2: Test equivalence -----
test_that("gu_adjust produces consistent estimates for each pair of baselines", {
  keep_cols <- c("term", "variable", "value", "estimate")
  
  r1 <- reg_AIAN |> select(all_of(keep_cols))
  r2 <- reg_AAPI |> select(all_of(keep_cols))
  r3 <- reg_Black |> select(all_of(keep_cols))
  
  # --- r1 vs r2 ---
  terms_r1_r2 <- intersect(r1$term, r2$term)
  r1_r2_1 <- r1 |> filter(term %in% terms_r1_r2) |> arrange(term)
  r1_r2_2 <- r2 |> filter(term %in% terms_r1_r2) |> arrange(term)
  expect_equal(r1_r2_1, r1_r2_2, tolerance = 1e-6)
  
  # --- r2 vs r3 ---
  terms_r2_r3 <- intersect(r2$term, r3$term)
  r2_r3_1 <- r2 |> filter(term %in% terms_r2_r3) |> arrange(term)
  r2_r3_2 <- r3 |> filter(term %in% terms_r2_r3) |> arrange(term)
  expect_equal(r2_r3_1, r2_r3_2, tolerance = 1e-6)
  
  # --- r1 vs r3 ---
  terms_r1_r3 <- intersect(r1$term, r3$term)
  r1_r3_1 <- r1 |> filter(term %in% terms_r1_r3) |> arrange(term)
  r1_r3_2 <- r3 |> filter(term %in% terms_r1_r3) |> arrange(term)
  expect_equal(r1_r3_1, r1_r3_2, tolerance = 1e-6)
})

test_that("gu_adjust warns if some adjust_vars are missing", {
  reg_valid <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula)
  
  expect_warning(
    gu_adjust(reg_valid, adjust_vars = c("RACE_ETH_bucket", "Peaches")),
    regexp = "Some adjust_vars were not found and will be ignored"
  )
})

test_that("gu_adjust errors if all adjust_vars are missing", {
  reg_valid <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula)
  
  expect_error(
    gu_adjust(reg_valid, adjust_vars = c("Peaches", "Apples")),
    regexp = "None of the adjust_vars were found in regression output"
  )
})

test_that("gu_adjust produces consistent estimates across multivariate baselines", {
  # Multivariable formula
  formula_multivar <- NUMPREC ~ RACE_ETH_bucket + AGE_bucket
  
  # --- Run regression with omitted vars: AAPI, 0-4
  ipums_2019_sample_tb$RACE_ETH_bucket <- 
    relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AAPI")
  ipums_2019_sample_tb$AGE_bucket <- 
    relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "0-4")
  reg_a <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_multivar) |>
    gu_adjust(adjust_vars = c("RACE_ETH_bucket", "AGE_bucket"))
  
  # --- Run regression with omitted vars: White, 45-49
  ipums_2019_sample_tb$RACE_ETH_bucket <- 
    relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "White")
  ipums_2019_sample_tb$AGE_bucket <- 
    relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "45-49")
  reg_b <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_multivar) |>
    gu_adjust(adjust_vars = c("RACE_ETH_bucket", "AGE_bucket"))
  
  # --- Run regression with omitted vars: Hispanic, 15-19
  ipums_2019_sample_tb$RACE_ETH_bucket <- 
    relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "Hispanic")
  ipums_2019_sample_tb$AGE_bucket <- 
    relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "15-19")
  reg_c <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_multivar) |>
    gu_adjust(adjust_vars = c("RACE_ETH_bucket", "AGE_bucket"))
  
  # Compare shared rows pairwise
  keep_cols <- c("term", "variable", "value", "estimate")
  a <- reg_a |> select(all_of(keep_cols))
  b <- reg_b |> select(all_of(keep_cols))
  c <- reg_c |> select(all_of(keep_cols))
  
  # a vs b
  terms_ab <- intersect(a$term, b$term)
  a_ab <- a |> filter(term %in% terms_ab) |> arrange(term)
  b_ab <- b |> filter(term %in% terms_ab) |> arrange(term)
  expect_equal(a_ab, b_ab, tolerance = 1e-6)
  
  # b vs c
  terms_bc <- intersect(b$term, c$term)
  b_bc <- b |> filter(term %in% terms_bc) |> arrange(term)
  c_bc <- c |> filter(term %in% terms_bc) |> arrange(term)
  expect_equal(b_bc, c_bc, tolerance = 1e-6)
  
  # a vs c
  terms_ac <- intersect(a$term, c$term)
  a_ac <- a |> filter(term %in% terms_ac) |> arrange(term)
  c_ac <- c |> filter(term %in% terms_ac) |> arrange(term)
  expect_equal(a_ac, c_ac, tolerance = 1e-6)
})

test_that("gu_adjust only modifies the specified coef_col", {
  # Run NUMPREC and BEDROOMS regressions
  formula_numprec  <- NUMPREC  ~ RACE_ETH_bucket
  formula_bedrooms <- bedroom ~ RACE_ETH_bucket
  
  # Use same omitted category for both
  ipums_2019_sample_tb$RACE_ETH_bucket <- 
    relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AAPI")
  
  reg_numprec <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_numprec) |>
    rename(numprec_estimate = estimate)
  
  reg_bedroom <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula_bedrooms) |>
    rename(bedroom_estimate = estimate)
  
  # Join both outputs
  merged <- full_join(reg_numprec, reg_bedroom, by = c("term"))
  
  # Save original bedroom column
  bedroom_before <- merged$bedroom_estimate
  
  # Apply GU adjustment only to the NUMPREC column
  adjusted <- gu_adjust(merged, adjust_vars = c("RACE_ETH_bucket"), coef_col = "numprec_estimate")
  
  # Check that numprec_estimate has changed
  expect_false(setequal(adjusted$numprec_estimate, merged$numprec_estimate))
  
  # Check that bedroom_estimate remains unchanged
  expect_equal(adjusted$bedroom_estimate, bedroom_before)
})

test_that("gu_adjust is idempotent", {
  # Run a basic regression
  ipums_2019_sample_tb$RACE_ETH_bucket <- 
    relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AAPI")
  
  formula <- NUMPREC ~ RACE_ETH_bucket
  gu_once <- dataduck_reg_matrix_2(data = ipums_2019_sample_tb, wt_col = "PERWT", formula = formula) |>
    gu_adjust(adjust_vars = "RACE_ETH_bucket", coef_col = "estimate")
  print(gu_once)
  
  # Apply gu_adjust a second time
  gu_twice <- gu_adjust(gu_once, adjust_vars = "RACE_ETH_bucket", coef_col = "estimate")
  print(gu_twice)
  
  # Compare outputs — should be identical
  expect_equal(gu_once, gu_twice, tolerance = 1e-10)
})

