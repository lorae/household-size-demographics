# tests/testthat/test-kob-tidy-output.R

# ----- Step 0: Load libraries and source function -----
library(testthat)
library(tibble)
library(dplyr)
library(rprojroot)

# Ensure working directory is project root
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function
source("src/utils/regression-tools.R")

# ----- Step 1: Define test inputs -----
varnames_dict_test <- c(
  "AGE_bucket",
  "EDUC_bucket"
)

# ----- Step 2: Run unit tests -----
test_that("kob_tidy_output correctly splits variable and value", {
  test_input <- tibble(term = c("AGE_bucket10-14", "EDUC_bucketCollege", "(Intercept)"))
  test_output <- kob_tidy_output(test_input, varnames = varnames_dict_test)
  print(test_output)
  
  expect_equal(test_output$variable, c("AGE_bucket", "EDUC_bucket", "(Intercept)"))
  expect_equal(test_output$value, c("10-14", "College", "(Intercept)"))
})

test_that("kob_tidy_output warns when term doesn't match any varname", {
  test_input <- tibble(term = c("Clown Car"))
  
  expect_warning(
    kob_tidy_output(test_input, varnames = varnames_dict_test),
    regexp = "could not be matched to a variable prefix"
  )
})

