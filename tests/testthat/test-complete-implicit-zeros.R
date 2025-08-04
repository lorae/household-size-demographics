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

reg_stub_salad <- tibble::tibble(
  term = c("(Intercept)", 
           "FRUITPeach", "FRUITApple", "FRUITRaspberry", "FRUITGrapefruit",
           "VEGETABLESweetPotato", "VEGETABLEArtichoke", "VEGETABLEKale"),
  estimate = c(1, 2, 3, 4, 5, 6, 7, 8)
)

varnames_dict <- c(
  "FRUIT",
  "VEGETABLE"
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

test_that("complete_implicit_zeros fills in one omitted row per variable", {
  input <- reg_stub_salad |> 
    filter(term != "FRUITApple" & term != "VEGETABLEKale")  # Remove 1 fruit, 1 vegetable
  
  result <- complete_implicit_zeros(
    input,
    adjust_by = list(
      FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit"),
      VEGETABLE = c("SweetPotato", "Artichoke", "Kale")
    ),
    coef_col = "estimate"
  )

  # Expect both missing rows are re-added
  expect_true("FRUITApple" %in% result$term)
  expect_true("VEGETABLEKale" %in% result$term)
  
  expect_equal(result$estimate[result$term == "FRUITApple"], 0)
  expect_equal(result$estimate[result$term == "VEGETABLEKale"], 0)
  
  # Two new rows should be added
  expect_equal(nrow(result), nrow(input) + 2)
})

test_that("complete_implicit_zeros warns for one variable and fills only the missing row", {
  input <- reg_stub_salad |> filter(term != "VEGETABLEKale")  # All fruits present, one vegetable missing
  
  expect_warning(
    result <- complete_implicit_zeros(
      input,
      adjust_by = list(
        FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit"),
        VEGETABLE = c("SweetPotato", "Artichoke", "Kale")
      ),
      coef_col = "estimate"
    ),
    regexp = "No levels were missing for variable 'FRUIT'"
  )
  
  # Only the missing vegetable should be added
  expect_true("VEGETABLEKale" %in% result$term)
  expect_false("FRUITApple" %in% (result |> filter(!(term %in% input$term)) |> pull(term)))
  expect_equal(result$estimate[result$term == "VEGETABLEKale"], 0)
  
  # Should add exactly 1 new row
  expect_equal(nrow(result), nrow(input) + 1)
})

test_that("complete_implicit_zeros errors if regression has more levels than adjust_by expects", {
  # Add an unexpected fruit (e.g., 'Mango') that isn't in adjust_by
  input <- tibble::tibble(
    term = c("(Intercept)", "FRUITPeach", "FRUITApple", "FRUITRaspberry", "FRUITGrapefruit", "FRUITMango"),
    estimate = c(1, 2, 3, 4, 5, 6)
  )
  
  expect_error(
    complete_implicit_zeros(
      input,
      adjust_by = list(FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit")),  # Mango not expected
      coef_col = "estimate"
    ),
    regexp = "unexpected levels not listed in adjust_by"
  )
})

test_that("complete_implicit_zeros assigns correct SE to added row", {
  # Remove 'FRUITApple' and add SE column
  input <- reg_stub |> 
    filter(term != "FRUITApple") |> 
    mutate(std_error = c(1, 2, 3, 4))  # arbitrary SEs for 4 remaining terms
  
  result <- complete_implicit_zeros(
    input,
    adjust_by = list(FRUIT = c("Peach", "Apple", "Raspberry", "Grapefruit")),
    coef_col = "estimate",
    se_col = "std_error"
  )
  
  # Extract added row
  added <- result |> filter(term == "FRUITApple")
  
  expect_equal(added$estimate, 0)
  expect_equal(added$std_error, sqrt(2^2 + 3^2 + 4^2), tolerance = 1e-6)
})


