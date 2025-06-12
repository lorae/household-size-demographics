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

# ----- Step 2: Define expected model ----- #

expected_model <- lm(
  NUMPREC ~ HHINCOME_bucket + EDUC_bucket,
  data = input,
  weights = input$PERWT,
  contrasts = list(
    HHINCOME_bucket = "contr.treatment",
    EDUC_bucket = "contr.treatment"
  )
)

# ----- Step 3: Run the custom function ----- #
# This test should just work.
# Define model spec
varnames_dict <- list(
  HHINCOME_bucket = c("less_than_10k", "from_10k_to_100k", "greater_than_100k"),
  EDUC_bucket = c("less_than_hs", "hs", "some_college", "college_4yr_plus")
)

# Capture printed output so it doesn't pollute test output
suppressMessages({
  result <- capture.output({
    run_regression(
      data = input,
      weights = "PERWT",
      varnames_dict = varnames_dict,
      outcome_var = "NUMPREC"
    )
  })
})

# ----- Step 4: Placeholder test -----

test_that("Custom regression matches expected model coefficients", {
  # TODO: Replace with actual comparison
  # e.g., compare coefficients like:
  # expect_equal(coef(expected_model), coef(model_output), tolerance = 1e-6)
  
  # Dummy placeholder
  expect_true(TRUE)
})
