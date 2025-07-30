# tests/testthat/test-add-intercept.R

# The purpose of this test is to verify that `add_intercept()` correctly reconstructs
# the output of a regression that originally included an intercept.

# ----- Step 0: Config -----
library(testthat)
library(dplyr)
library(glue)
library(devtools)

# Ensure working directory is project root
root <- rprojroot::find_root(rprojroot::is_rstudio_project)
setwd(root)

# Load packages and helper functions
load_all("../dataduck")
source("src/utils/regression-tools.R") # Contains the function & split_term_column
source("src/utils/create-benchmark-data.R") # Needed to grab sample raw data
source("src/utils/regression-backends.R") # Needed to get regression function
source("kob/scripts/kob-function.R") # Needed as a dependency of regression-tools.R

# ----- Step 1: Prepare benchmark regression data -----

# (1) Create benchmark sample if not already created
n_strata <- 3

create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# (2) Load and filter sample
ipums_2019_sample_tb <- readRDS(glue("cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# (3) Define formulas
formula_int <- BEDROOMS ~ RACE_ETH_bucket + tenure
formula_no_int <- BEDROOMS ~ -1 + RACE_ETH_bucket + tenure

# (4) Run regression with intercept
reg_int <- dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula_int
) |>
  split_term_column() |>
  rename(coef_2000 = estimate) |>
  mutate(
    # These extra cols are placeholders. Without them, add_intercept will refuse to run
    coef_2000_se = 0,
    coef_2019 = 0,
    coef_2019_se = 0,
    prop_2000 = 0,
    prop_2000_se = 0,
    prop_2019 = 0,
    prop_2019_se = 0,
    u = 0,
    u_se = 0,
    e = 0,
    e_se = 0,
    c = 0,
    c_se = 0
  )

# (5) Run regression without intercept
reg_no_int <- dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula_no_int
) |>
  split_term_column() |>
  rename(coef_2000 = estimate) |>
  mutate(
    # These extra cols are placeholders. Without them, add_intercept will refuse to run
    coef_2000_se = 0,
    coef_2019 = 0,
    coef_2019_se = 0,
    prop_2000 = 0,
    prop_2000_se = 0,
    prop_2019 = 0,
    prop_2019_se = 0,
    u = 0,
    u_se = 0,
    e = 0,
    e_se = 0,
    c = 0,
    c_se = 0
  )

# ----- Step 2: Test add_intercept -----

test_that("add_intercept reconstructs intercept regression correctly", {
  result <- add_intercept(
    reg_data = reg_no_int,
    variable = "RACE_ETH_bucket",
    reference_value = "AAPI"
  )
  
  # Sort both data frames for comparison
  result_sorted <- result |> arrange(term) |> select(term, value, variable, coef_2000)
  print(result_sorted)
  reg_int_sorted <- reg_int |> arrange(term) |> select(term, value, variable, coef_2000)
  print(reg_int_sorted)
  
  # Compare full outputs
  expect_equal(result_sorted, reg_int_sorted)
})


test_that("add_intercept stops if variable is not found", {
  expect_error(
    add_intercept(
      reg_data = reg_no_int,
      variable = "Peaches",             # <- not a real variable
      reference_value = "White"
    ),
    regexp = "Variable 'Peaches' not found"
  )
})

test_that("add_intercept stops if reference value is not found", {
  expect_error(
    add_intercept(
      reg_data = reg_no_int,
      variable = "RACE_ETH_bucket",
      reference_value = "Peaches"       # <- not a real value
    ),
    regexp = "Reference value 'Peaches' not found"
  )
})
