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
reg_AIAN <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula) |>
  complete_implicit_zeros(
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) 

# AAPI omitted
data_AAPI <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "AAPI"))
reg_AAPI <- dataduck_reg_matrix_2(data = data_AAPI, wt_col = "PERWT", formula = formula)|>
  complete_implicit_zeros(
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) 

# Black omitted
data_Black <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "Black"))
reg_Black <- dataduck_reg_matrix_2(data = data_Black, wt_col = "PERWT", formula = formula) |>
  complete_implicit_zeros(
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) 

# ----- Step 2: Test equivalence -----
test_that("gu_adjust produces consistent estimates for each pair of baselines", {

  r1 <- gu_adjust(
    reg_output = reg_AIAN,
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) |> arrange(term)

  r2 <- gu_adjust(
    reg_output = reg_AAPI,
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) |> arrange(term)

  r3 <- gu_adjust(
    reg_output = reg_Black,
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
  ) |> arrange(term)

  expect_equal(r1, r2, tolerance = 1e-6)
  expect_equal(r2, r3, tolerance = 1e-6)
  expect_equal(r1, r3, tolerance = 1e-6)
})


test_that("gu_adjust produces consistent estimates across multivariate baselines", {
  formula_multivar <- NUMPREC ~ RACE_ETH_bucket + AGE_bucket
  
  adjust_by <- list(
    RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White"),
    AGE_bucket = unique(ipums_2019_sample_tb$AGE_bucket)
  )
  
  # Baseline: AAPI + 0–4
  ipums_2019_sample_tb$RACE_ETH_bucket <- relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "AAPI")
  ipums_2019_sample_tb$AGE_bucket <- relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "0-4")
  reg_a <- dataduck_reg_matrix_2(ipums_2019_sample_tb, "PERWT", formula_multivar) |>
    complete_implicit_zeros(
      adjust_by = adjust_by,
      coef_col = "estimate"
    )
  a <- gu_adjust(
    reg_output = reg_a,
    adjust_by,
    coef_col = "estimate"
  ) |> arrange(term)
  
  # Baseline: White + 45–49
  ipums_2019_sample_tb$RACE_ETH_bucket <- relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "White")
  ipums_2019_sample_tb$AGE_bucket <- relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "45-49")
  reg_b <- dataduck_reg_matrix_2(ipums_2019_sample_tb, "PERWT", formula_multivar) |>
    complete_implicit_zeros(
      adjust_by = adjust_by,
      coef_col = "estimate"
    )
  b <- gu_adjust(
    reg_output = reg_b,
    adjust_by,
    coef_col = "estimate"
  ) |> arrange(term)
  
  # Baseline: Hispanic + 15–19
  ipums_2019_sample_tb$RACE_ETH_bucket <- relevel(factor(ipums_2019_sample_tb$RACE_ETH_bucket), ref = "Hispanic")
  ipums_2019_sample_tb$AGE_bucket <- relevel(factor(ipums_2019_sample_tb$AGE_bucket), ref = "15-19")
  reg_c <- dataduck_reg_matrix_2(ipums_2019_sample_tb, "PERWT", formula_multivar) |>
    complete_implicit_zeros(
      adjust_by = adjust_by,
      coef_col = "estimate"
    )
  c <- gu_adjust(
    reg_output = reg_c,
    adjust_by,
    coef_col = "estimate"
  ) |> arrange(term)
  
  # Full-object comparisons
  expect_equal(a, b, tolerance = 1e-6)
  expect_equal(b, c, tolerance = 1e-6)
  expect_equal(a, c, tolerance = 1e-6)
})


test_that("gu_adjust warns if some adjust_vars are missing", {
  reg_valid <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula)
  
  expect_warning(
    gu_adjust(
      reg_valid, 
      adjust_by = list(
        RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White"),
        FRUIT = c("Peaches", "Grapefruit", "Raspberries")
      )),
    regexp = "Some adjust_vars were not found and will be ignored"
  )
})

test_that("gu_adjust errors if all adjust_vars are missing", {
  reg_valid <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula)
  
  expect_error(
    gu_adjust(
      reg_valid, 
      adjust_by = list(
        VEGETABLE = c("SweetPotato", "Artichoke", "Kale"),
        FRUIT = c("Peaches", "Grapefruit", "Raspberries")
      )
    ),
    regexp = "None of the adjust_vars were found in regression output"
  )
})

test_that("gu_adjust is idempotent", {
  
  gu_once <- reg_AIAN |> gu_adjust()
  
  gu_twice <- reg_AIAN |> gu_adjust() |> gu_adjust()
  
  expect_equal(gu_once, gu_twice, tolerance = 1e-10)
})

