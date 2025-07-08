# tests/testthat/test-kob-function.R

# The purpose of this test is to determine whether the KOB function works as expected
# on a set of controlled inputs.

# ----- Step 0: Config -----
library(testthat)
library(tibble)
library(dplyr)
library(rprojroot)
library(purrr)
library(stringr)

# Ensure working directory is project root
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function under test
source("kob/scripts/kob-function.R")

# ----- Step 1: Create a dummy kob_input -----
kob_input <- tibble(
  term = c("foo", "bar", "baz"),
  coef_2000 = c(1.0, 2.0, 3.0),
  coef_2000_se = c(0.1, 0.2, 0.3),
  coef_2019 = c(1.5, 2.5, 2.7),
  coef_2019_se = c(0.15, 0.25, 0.35),
  prop_2000 = c(0.2, 0.3, 0.5),
  prop_2000_se = c(0.01, 0.02, 0.03),
  prop_2019 = c(0.25, 0.35, 0.4),
  prop_2019_se = c(0.015, 0.025, 0.02)
)

kob_input_intercept <- tibble(
  term = c("(Intercept)", "foo", "bar", "baz"),
  coef_2000 = c(4.0, 1.0, 2.0, 3.0),
  coef_2000_se = c(0.4, 0.1, 0.2, 0.3),
  coef_2019 = c(4.5, 1.5, 2.5, 2.7),
  coef_2019_se = c(0.5, 0.15, 0.25, 0.35),
  prop_2000 = c(NA, 0.2, 0.3, 0.5),
  prop_2000_se = c(NA, 0.01, 0.02, 0.03),
  prop_2019 = c(NA, 0.25, 0.35, 0.4),
  prop_2019_se = c(NA, 0.015, 0.025, 0.02)
)

kob_expected <- kob_input |>
  mutate(
    u = c(NA_real_, NA_real_, NA_real_),
    u_se = c(NA_real_, NA_real_, NA_real_),
    e = c(0.075, 0.125, -0.27),
    e_se = c(0.02806243, 0.081009259, 0.103450471),
    c = c(0.1, 0.15, -0.15),
    c_se = c(0.036400549, 0.09656604, 0.230664258)
  )

kob_expected_intercept <- kob_input_intercept |>
  mutate(
    u = c(0.5, NA_real_, NA_real_, NA_real_),
    u_se = c(0.6403124, NA_real_, NA_real_, NA_real_),
    e = c(NA_real_, 0.075, 0.125, -0.27),
    e_se = c(NA_real_, 0.02806243, 0.081009259, 0.103450471),
    c = c(NA_real_, 0.1, 0.15, -0.15),
    c_se = c(NA_real_, 0.036400549, 0.09656604, 0.230664258)
  )

# ----- Step 2: Test `kob` -----

test_that("kob() output matches expected values without intercept", {
  result <- kob(kob_input)
  
  expect_equal(result$u, kob_expected$u, tolerance = 1e-6)
  expect_equal(result$u_se, kob_expected$u_se, tolerance = 1e-6)
  expect_equal(result$e, kob_expected$e, tolerance = 1e-6)
  expect_equal(result$e_se, kob_expected$e_se, tolerance = 1e-6)
  expect_equal(result$c, kob_expected$c, tolerance = 1e-6)
  expect_equal(result$c_se, kob_expected$c_se, tolerance = 1e-6)
})

test_that("kob() output matches expected values with intercept", {
  result <- kob(kob_input_intercept)
  
  expect_equal(result$u, kob_expected_intercept$u, tolerance = 1e-6)
  expect_equal(result$u_se, kob_expected_intercept$u_se, tolerance = 1e-6)
  expect_equal(result$e, kob_expected_intercept$e, tolerance = 1e-6)
  expect_equal(result$e_se, kob_expected_intercept$e_se, tolerance = 1e-6)
  expect_equal(result$c, kob_expected_intercept$c, tolerance = 1e-6)
  expect_equal(result$c_se, kob_expected_intercept$c_se, tolerance = 1e-6)
})

test_that("kob() stops when required columns are missing", {
  input_missing_col <- kob_input |> select(-coef_2000)  # Remove one required column
  
  expect_error(
    kob(input_missing_col),
    regexp = "Missing required columns: coef_2000"
  )
})

test_that("kob() stops when multiple intercepts are present", {
  input_multiple_intercepts <- kob_input_intercept |>
    add_row(term = "(Intercept)", 
            coef_2000 = 3, coef_2000_se = 0.1,
            coef_2019 = 3.5, coef_2019_se = 0.1,
            prop_2000 = NA, prop_2000_se = NA,
            prop_2019 = NA, prop_2019_se = NA)
  
  expect_error(
    kob(input_multiple_intercepts),
    regexp = "Multiple '\\(Intercept\\)' rows detected"
  )
})

test_that("kob() handles zero standard errors correctly", {
  input_zero_se <- kob_input |>
    mutate(across(ends_with("_se"), ~0))
  
  result <- kob(input_zero_se)
  
  expect_true(all(result$e_se == 0))
  expect_true(all(result$c_se == 0))
})

test_that("kob() returns zero e and c terms for identical 2000 and 2019 inputs", {
  input_identical <- kob_input |>
    mutate(
      coef_2019 = coef_2000,
      coef_2019_se = coef_2000_se,
      prop_2019 = prop_2000,
      prop_2019_se = prop_2000_se
    )
  
  result <- kob(input_identical)
  
  expect_equal(result$e, rep(0, nrow(result)))
  expect_equal(result$c, rep(0, nrow(result)))
})

test_that("kob() skips e/c calculation for intercept row", {
  result <- kob(kob_input_intercept)
  intercept_row <- result |> filter(term == "(Intercept)")
  
  expect_true(is.na(intercept_row$e))
  expect_true(is.na(intercept_row$c))
  expect_true(is.na(intercept_row$e_se))
  expect_true(is.na(intercept_row$c_se))
})

# ----- Step 3: Test `kob_tidy_output` ----- 
# Varnames fed into kob_tidy_output() function, below
varnames_dict_test <- c(
  "AGE_bucket",
  "EDUC_bucket"
)

test_that("kob_tidy_output correctly splits variable and value", {
  test_input <- tibble(term = c("AGE_bucket10-14", "EDUC_bucketCollege", "(Intercept)"))
  test_output <- kob_tidy_output(test_input, varnames = varnames_dict_test)
  
  expect_equal(test_output$variable, c("AGE_bucket", "EDUC_bucket", NA))
  expect_equal(test_output$value, c("10-14", "College", "(Intercept)"))
})

test_that("kob_tidy_output warns when term doesn't match any varname", {
  test_input <- tibble(term = c("(Intercept)"))
  
  expect_warning(
    kob_tidy_output(test_input, varnames = varnames_dict_test),
    regexp = "could not be matched to a variable prefix"
  )
})
