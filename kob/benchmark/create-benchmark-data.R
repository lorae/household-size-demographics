# create-benchmark-data.R
#
# The purpose of the script is to modularize the creation of a benchmark sample
# for use in tests of jobs and other functions.
#
# ----- Step 0: Config

library(glue)
library(duckdb)
library(dplyr)

#' should_run
#'
#' Determines whether a step should run based on file existence.
#' Returns TRUE if any file is missing, FALSE otherwise.
#'
#' @param ... One or more file paths
#' @return Logical scalar: TRUE if any file is missing, FALSE otherwise
should_run <- function(...) {
  !all(file.exists(...))
}

#
#
# create_benchmark_sample
#
# Lightweight cache-aware execution:
# Skips computation if output files exist and force == FALSE.
# Saves benchmark data to `kob/cache/benchmark_sample_{year}_{n_strata}`,
# with one .rds file for the tibble and one .duckdb file for the database.
# Within the DB, the table name is always "ipums_sample".
#
# Inputs:
# - year: numeric
# - n_strata: number of strata to sample from
# - db_path: path to source DuckDB database
# - db_table_name: name of source table
# - force: whether to overwrite cached results
#
# output:
# - it will save the created benchmark to a directory called
# `kob/cache/benchmark_sample_{year}_{n_strata}`
# - There will be one file in that folder with the db and one with the tb
# - within the db, the table name will be "ipums_sample"
create_benchmark_sample <- function(
    year = 2019,
    n_strata = 3,
    db_path = "data/db/ipums.duckdb", # The name of the DB with the source data
    db_table_name = "ipums_processed", # Name of the table within the DB of source data
    force = FALSE # TRUE will recalculate benchmark sample, even if it already exists in cache
) {
  # Path to the cache where the output will be stored
  output_path = glue("kob/cache/benchmark_sample_{year}_{n_strata}")
  output_tb <- glue("{output_path}/tb.rds")
  output_db <- glue("{output_path}/db.duckdb")
  
  outputs_missing <- should_run(output_db, output_tb)
  
  # Early break
  if (!outputs_missing & !force) {
    message("✅ Benchmark files already exists and user has opted force == FALSE, so no files were generated")
    return(NULL)
  }

  # Continue
  message("⚙️  Generating benchmark files...")
  
  # Connect to DB; assign alias to table
  con <- dbConnect(duckdb::duckdb(), db_path)
  ipums_db <- tbl(con, db_table_name)

  # Sample {`n_strata`} strata from {`year`} data where GQ ∈ [0,1,2]
  strata_sample <- ipums_db |> 
    filter(YEAR == year) |> 
    distinct(STRATA) |> 
    arrange(sql("RANDOM()")) |>
    head(n_strata) |>
    collect()
  
  print(strata_sample)
  
  # Save them
  
  # unit test comparing them can be external.
}

# Example usage (for dev only)
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)
