# tests/testthat/test-create-benchmark-data.R
#
# ----- Step 0: Config -----
library(rprojroot)

# Ensure working directory is project root
root <- find_root(is_rstudio_project)
setwd(root)

# Source the functions we're testing
source("kob/benchmark/create-benchmark-data.R")

# ----- Step 1: Run the tests! -----
test_that("create_benchmark_sample will create new dir if output_dir doesn't exist", {
  # First, delete this dir if it exists
  unlink("tests/testthat/test-create-benchmark-sample", recursive = TRUE, force = TRUE)
  
  # Run create_benchmark_sample, which should create the dir and deposit files
  create_benchmark_sample(
    year = 2019,
    n_strata = 3,
    db_path = "data/db/ipums.duckdb",
    db_table_name = "ipums_processed",
    output_dir = "tests/testthat/test-create-benchmark-sample",
    force = FALSE
  )
  
  # Confirm expected output files exist
  expect_true(all_exist(
    "tests/testthat/test-create-benchmark-sample/benchmark_sample_2019_3/db.duckdb",
    "tests/testthat/test-create-benchmark-sample/benchmark_sample_2019_3/tb.rds"
    )
  )
})

# Expect to execute due to force = TRUE arg
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = TRUE
)

# Expect following warning: 
# Warning message:
#   In create_benchmark_sample(year = 2019, n_strata = 2352, db_path = "data/db/ipums.duckdb",  :
#                                ❗ Only 2351 strata available in 2019, but n_strata = 2352.
create_benchmark_sample(
  year = 2019,
  n_strata = 2352,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = TRUE
)

# Expect following warning:
# Warning message:     
#   In create_benchmark_sample(year = 2018, n_strata = 3, db_path = "data/db/ipums.duckdb",  :
#                                ❗ The requested year 2018 does not exist in the source data.
create_benchmark_sample(
  year = 2018,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = TRUE
)

# Add unit test seeing if files can be loaded, looking at colnames and such