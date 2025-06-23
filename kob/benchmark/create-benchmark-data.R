# create-benchmark-data.R
#
# The purpose of the script is to modularize the creation of a benchmark sample
# for use in tests of jobs and other functions.
#
# ----- Step 0: Config

library(glue)

#
# create_benchmark_sample
# Lightweight version of a cache aware design pattern:
# Early NULL return is triggered (via the "bouncer" or "early return" design pattern) 
# if the target output files already exist and the user has indicated via the "force"
# arg that they do not wish to override them.
# That way, computation is only applied if needed.
# TODO: essentially write a should_run function that tests for existence of all
# file paths given to it. Can be used as a helper function across this codebase.
# E.g.
# should_run <- function(...) {
#   !all(file.exists(...))
# }
# or just implement targets.
#
# input:
# - args given below
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
  
  # Define output_exists as a binary variable that is TRUE if both `output_tb` and 
  # `output_db` files already exist. Note: only checks for file existence, not content.
  output_exists <- (file.exists(output_tb) && file.exists(output_db))
  print(output_exists)
  
  if (output_exists & force == FALSE) {
    message("✅ Benchmark files already exists and user has opted force == FALSE, so no files were generated")
    return(NULL)
  }

  # continue
  message("⚙️  Generating benchmark files...")
  
  # Then:
  
  # Run logic to create the tibble and db
  
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
