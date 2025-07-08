# tests/testthat/test-kob-function.R

# The purpose of this test is to determine whether the KOB function works as expected
# on a set of controlled inputs.

# ----- Step 0: Config -----
library(tibble)
library(dplyr)
library(rprojroot)

# Ensure working directory is project root
root <- find_root(is_rstudio_project)
setwd(root)

# Source the function under test
source("kob/scripts/kob-function.R")

# ----- Step 1: Create a dummy kob_input -----
kob_input_test <- tibble(
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