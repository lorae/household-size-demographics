# tests/testthat/test-gu-adjust.R

# The purpose of this test is to verify that `gu_adjust()` produces consistent estimates
# regardless of the omitted category for a factor variable like RACE_ETH_bucket.

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
source("src/utils/regression-postprocess-tools.R") # gu_adjust()


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
reg_AIAN <- dataduck_reg_matrix_2(data = data_AIAN, wt_col = "PERWT", formula = formula) |> gu_adjust()

# AAPI omitted
data_AAPI <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "AAPI"))
reg_AAPI <- dataduck_reg_matrix_2(data = data_AAPI, wt_col = "PERWT", formula = formula) |> gu_adjust()

# Black omitted
data_Black <- filtered_tb |> mutate(RACE_ETH_bucket = relevel(factor(RACE_ETH_bucket), ref = "Black"))
reg_Black <- dataduck_reg_matrix_2(data = data_Black, wt_col = "PERWT", formula = formula) |> gu_adjust()

# ----- Step 2: Test equivalence -----
test_that("gu_adjust produces consistent estimates for each pair of baselines", {
  keep_cols <- c("term", "variable", "value", "estimate")
  
  r1 <- reg_AIAN |> select(all_of(keep_cols))
  r2 <- reg_AAPI |> select(all_of(keep_cols))
  r3 <- reg_Black |> select(all_of(keep_cols))
  
  # --- r1 vs r2 ---
  terms_r1_r2 <- intersect(r1$term, r2$term)
  r1_r2_1 <- r1 |> filter(term %in% terms_r1_r2) |> arrange(term)
  r1_r2_2 <- r2 |> filter(term %in% terms_r1_r2) |> arrange(term)
  expect_equal(r1_r2_1, r1_r2_2, tolerance = 1e-6)
  
  # --- r2 vs r3 ---
  terms_r2_r3 <- intersect(r2$term, r3$term)
  r2_r3_1 <- r2 |> filter(term %in% terms_r2_r3) |> arrange(term)
  r2_r3_2 <- r3 |> filter(term %in% terms_r2_r3) |> arrange(term)
  expect_equal(r2_r3_1, r2_r3_2, tolerance = 1e-6)
  
  # --- r1 vs r3 ---
  terms_r1_r3 <- intersect(r1$term, r3$term)
  r1_r3_1 <- r1 |> filter(term %in% terms_r1_r3) |> arrange(term)
  r1_r3_2 <- r3 |> filter(term %in% terms_r1_r3) |> arrange(term)
  expect_equal(r1_r3_1, r1_r3_2, tolerance = 1e-6)
})
