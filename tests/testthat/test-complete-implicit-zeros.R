# The purpose of this set of unit tests is to verify that `complete_implicit_zeros()`
# correctly identifies and fills in omitted reference levels with coefficient = 0.
# These reference levels are required for applying the Gardeazabal-Ugidos adjustment.
# The tests confirm:
# 1. Correct behavior when one level is missing.
# 2. Expected errors when 0 or >1 levels are missing.

# ----- Step 0: Setup -----
library(testthat)
library(dplyr)
library(glue)

# Ensure working directory is project root
root <- rprojroot::find_root(rprojroot::is_rstudio_project)
setwd(root)

# Load target function and any dependencies
source("src/utils/regression-postprocess-tools.R")  # contains complete_implicit_zeros()

# ----- Step 1: Create dummy regression output -----
reg_stub <- tibble::tibble(
  term = c("(Intercept)", "FRUITPeach", "FRUITApple", "FRUITRaspberry", "FRUITGrapefruit"),
  estimate = c(1, 2, 3, 4, 5)
)

# Provide a fake version of split_term_column() for testing only
split_term_column <- function(df) {
  df |>
    mutate(
      variable = gsub("[^A-Z_]+", "", term),
      value = gsub("^[A-Z_]+", "", term)
    )
}

varnames_dict <- c(
  "FRUIT"
)

# ----- Step 2: Unit tests -----

test_that("complete_implicit_zeros fills in missing level correctly", {
  input <- reg_stub |> filter(term != "FRUITApple")  # Remove Apple
  
  result <- complete_implicit_zeros(
    input,
    adjust_by = list(FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit")),
    coef_col = "estimate"
  )
  
  expect_true("FRUITApple" %in% result$term)
  expect_equal(result$estimate[result$term == "FRUITApple"], 0)
  
  # Final length should be one more than input
  expect_equal(nrow(result), nrow(input) + 1)
})

test_that("complete_implicit_zeros warns if no level is missing", {
  expect_warning(
    complete_implicit_zeros(
      reg_stub,
      adjust_by = list(FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit")),
      coef_col = "estimate"
    ),
    regexp = "No levels were missing for variable 'FRUIT'"
  )
})

test_that("complete_implicit_zeros errors if more than one level is missing", {
  input <- reg_stub |> filter(term != "FRUITApple" & term != "FRUITPeach")
  
  expect_error(
    complete_implicit_zeros(
      input,
      adjust_by = list(FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit")),
      coef_col = "estimate"
    ),
    regexp = "expected exactly one missing level"
  )
})

