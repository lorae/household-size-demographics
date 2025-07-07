# tests/testthat/test-regression-backends.R

library(testthat)
library(tibble)
library(dplyr)

# ----- Load regression backend functions ----- #
source("kob/benchmark/regression-backends.R")

# ----- Create synthetic test data ----- #
test_data <- tibble::tibble(
  NUMPREC = c(2, 3, 2, 1),
  PERWT = c(100, 200, 150, 120),
  REPWT1 = c(100, 0, 100, 0),
  tenure = factor(
    c("homeowner", "renter", "homeowner", "renter")
  )
)

# Initialize formula
formula = NUMPREC ~ tenure
# ---- Run the reg functions ----- #
dataduck_reg_lm(data = test_data, wt_col = "PERWT", formula = formula)
dataduck_reg_matrix(data = test_data, wt_col = "PERWT", formula = formula)

# ---- Run the reg functions ----- #
# This time with repwt1, where we expect reg_matrix to fail but not reg_lm
dataduck_reg_lm(data = test_data, wt_col = "REPWT1", formula = formula)
dataduck_reg_matrix(data = test_data, wt_col = "REPWT1", formula = formula)