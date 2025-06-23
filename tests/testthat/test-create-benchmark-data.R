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

test_that("create_benchmark_sample writes expected data content to rds", {
  tb <- readRDS("tests/testthat/test-create-benchmark-sample/benchmark_sample_2019_3/tb.rds")
  
  expect_s3_class(tb, "tbl_df")
  expect_true("STRATA" %in% names(tb))
  expect_true(nrow(tb) > 0)
})

test_that("create_benchmark_sample writes expected data content to duckdb", {
  # Connect to the generated DuckDB file
  con <- DBI::dbConnect(
    duckdb::duckdb(), 
    dbdir = "tests/testthat/test-create-benchmark-sample/benchmark_sample_2019_3/db.duckdb"
  )
  
  # List available tables (should contain "ipums_sample")
  tables <- DBI::dbListTables(con)
  expect_true("ipums_sample" %in% tables)
  
  # Read in the table
  db_data <- DBI::dbReadTable(con, "ipums_sample")
  
  # Check that it has expected structure
  expect_s3_class(db_data, "data.frame")
  expect_true("STRATA" %in% names(db_data))
  expect_gt(nrow(db_data), 0)
  
  # Disconnect cleanly
  DBI::dbDisconnect(con, shutdown = TRUE)
})

test_that("create_benchmark_sample returns early when output exists and force = FALSE", {
  # Assumes the files already exist from previous run; generated in tests above
  expect_message(
    create_benchmark_sample(
      year = 2019,
      n_strata = 3,
      db_path = "data/db/ipums.duckdb",
      db_table_name = "ipums_processed",
      output_dir = "tests/testthat/test-create-benchmark-sample",
      force = FALSE
    ),
    regexp = "Benchmark files already exists and user has opted force == FALSE"
  )
})

test_that("create_benchmark_sample regenerates outputs when force = TRUE", {
  # Assumes the files already exist from previous run; generated in tests above
  # Capture message output
  messages <- capture_messages({
    create_benchmark_sample(
      year = 2019,
      n_strata = 3,
      db_path = "data/db/ipums.duckdb",
      db_table_name = "ipums_processed",
      output_dir = "tests/testthat/test-create-benchmark-sample",
      force = TRUE
    )
  })
  
  # Confirm the "early return" message was NOT included
  expect_false(any(grepl("Benchmark files already exists and user has opted force == FALSE", messages)))
  
  # Confirm the success messages *were* emitted
  expect_true(any(grepl("Saved benchmark tb", messages)))
  expect_true(any(grepl("Saved benchmark db", messages)))
})

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
