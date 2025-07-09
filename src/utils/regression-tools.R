# src/utils/regression-tools.R
#
# The purpose of this script is to provide testable, modular functions for transforming
# regression results
#
#

# ----- Step 0: Config ----- #

library(devtools)

# Define number of strata to use
n_strata <- 3

# Define regression formulas
formula_intercept <- BEDROOMS ~ RACE_ETH_bucket
formula_no_intercept <- BEDROOMS ~ -1 + RACE_ETH_bucket

# Load internal packages and helpers
load_all("../dataduck")
source("kob/benchmark/create-benchmark-data.R")
source("kob/benchmark/regression-backends.R")

# ----- Step 1: Load and prepare sample ----- #
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("kob/cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT", 
  formula = formula_intercept)

dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT", 
  formula = formula_no_intercept)