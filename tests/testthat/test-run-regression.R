# tests/testthat/test-run-regression.R

library(testthat)
library(rprojroot)
library(dplyr)
library(glue)

# ----- Step 0: Workspace setup ----- #

# Ensure working directory is project root
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function file
source("kob/scripts/refactor/run-regression.R")

# ----- Step 1: Create test inputs ----- #

# Read synthetic data
synth_data <- readRDS("kob/synthetic-data/c-only.rds")
input <- synth_data |> filter(year == 2000)

# ----- Step 2: TESTS! ----- #
test_that("Model output coefficients match expected values", {

  expected_model <- lm(
    NUMPREC ~ HHINCOME_bucket + EDUC_bucket,
    data = input,
    weights = input$PERWT,
    contrasts = list(
      HHINCOME_bucket = "contr.treatment",
      EDUC_bucket = "contr.treatment"
    )
  )

  # Define model spec
  varnames_dict <- list(
    HHINCOME_bucket = c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
    EDUC_bucket = c("less_than_hs", "hs", "some_college", "college_4yr_plus")
  )

  # Capture printed output so it doesn't pollute test output
  output_model <- suppressMessages(run_regression(
    data = input,
    weights = "PERWT",
    varnames_dict = varnames_dict,
    outcome_var = "NUMPREC"
  ))

  # Do coefficients for identical terms match?
  expect_equal(expected_model$coefficients,
               output_model$coefficients,
               tolerance = 1e-6)
  
  # Keeping this code as a more direct way to compare coefficients if I end
  # up swapping regression functions (e.g. `glm` instead of `lm`) which output
  # slightly different formats for the coefficients.
  expect_equal(expected_model$coefficients["HHINCOME_bucketfrom_10k_to_100k"],
               output_model$coefficients["HHINCOME_bucketfrom_10k_to_100k"],
               tolerance = 1e-6)
  
  expect_equal(expected_model$coefficients["(Intercept)"],
               output_model$coefficients["(Intercept)"],
               tolerance = 1e-6)
})


test_that("run_regression outputs error when `varnames_dict` names not in `data`", {

  # Define model spec
  varnames_dict <- list(
    RACE_ETH_bucket = c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
    EDUC_bucket = c("less_than_hs", "hs", "some_college", "college_4yr_plus")
  )
  
  expect_error(
    suppressMessages(
      run_regression(
        data = input,
        weights = "PERWT",
        varnames_dict = varnames_dict,
        outcome_var = "NUMPREC"
      )
    ),
    regexp = "The following predictors in varnames_dict are not in the data: RACE_ETH_bucket"
  )
})

test_that("run_regression warns when varnames_dict includes values not in the data", {
  
  varnames_dict <- list(
    HHINCOME_bucket = c("negative", "less_than_10k", "from_10k_to_100k", "greater_than_100k"),  # 'negative' is not in data
    EDUC_bucket = c("less_than_hs", "hs", "some_college", "college_4yr_plus")
  )
  
  expect_warning(
    suppressMessages(
      run_regression(
        data = input,
        weights = "PERWT",
        varnames_dict = varnames_dict,
        outcome_var = "NUMPREC"
      )
    ),
    regexp = "The following values for 'HHINCOME_bucket' are listed in varnames_dict but not found in data: negative"
  )
})


test_that("run_regression errors when data contains values not listed in varnames_dict", {
  
  varnames_dict <- list(
    HHINCOME_bucket = c("from_10k_to_100k", "greater_than_100k"),  # missing "less_than_10k", which is present in data
    EDUC_bucket = c("less_than_hs", "hs", "some_college", "college_4yr_plus")
  )
  
  expect_error(
    suppressMessages(
      run_regression(
        data = input,
        weights = "PERWT",
        varnames_dict = varnames_dict,
        outcome_var = "NUMPREC"
      )
    ),
    regexp = "The following values for 'HHINCOME_bucket' are found in the data but not listed in varnames_dict: less_than_10k"
  )
})


