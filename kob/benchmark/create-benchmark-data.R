# create-benchmark-data.R
#
# The purpose of the script is to modularize the creation of a benchmark sample
# for use in tests of jobs and other functions.
#
# ----- Step 0: Config

library(glue)
library(duckdb)
library(dplyr)

#' all_exist
#'
#' Determines whether all specified file inputs in arguments exists.
#' Returns TRUE all files exist, FALSE otherwise.
#' TODO: this might be later replaced with `targets` package
#'
#' @param ... One or more file paths
#' @return Logical scalar: TRUE if all files exist, FALSE otherwise
all_exist <- function(...) {
  all(file.exists(...))
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
# - It saves benchmark to a directory called
# `kob/cache/benchmark_sample_{year}_{n_strata}`
# - There will be one file in that folder with the db and one with the tb
# - within the db, the table name will be "ipums_sample"
# - returns an invisible NULL if any of the defensive validation checks are failed
create_benchmark_sample <- function(
    year = 2019,
    n_strata = 3,
    db_path = "data/db/ipums.duckdb", # The name of the DB with the source data
    db_table_name = "ipums_processed", # Name of the table within the DB of source data
    force = FALSE # TRUE will recalculate benchmark sample, even if it already exists in cache
) {
  # Path to the cache where the output will be stored
  output_dir = glue("kob/cache/benchmark_sample_{year}_{n_strata}") |> as.character()
  output_tb_path <- glue("{output_dir}/tb.rds") |> as.character()
  output_db_path <- glue("{output_dir}/db.duckdb") |> as.character()
  
  outputs_exist <- all_exist(output_db_path, output_tb_path)
  
  # Early break
  if (outputs_exist & !force) {
    message("✅ Benchmark files already exists and user has opted force == FALSE, so no files were generated")
    return(invisible(NULL))
  }

  # Continue
  message("⚙️  Generating benchmark files...")
  
  # Ensure the output_dir already exists. If it doesn't, create it.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  # Connect to DB; assign alias to table
  con <- dbConnect(duckdb::duckdb(), db_path)
  ipums_db <- tbl(con, db_table_name)

  # Sample {`n_strata`} strata from {`year`} data
  # TODO: defensive check: validate n_strata <= number of available strata in sample year
  # TODO: defensive check: validate that {year} exists in data
  strata_sample <- ipums_db |> 
    filter(YEAR == year) |> 
    distinct(STRATA) |> 
    arrange(sql("RANDOM()")) |>
    head(n_strata) |>
    collect()
  
  # Create the sampled tb & write to `output_tb_path`
  ipums_sample_tb <- ipums_db |> 
    filter(YEAR == year, STRATA %in% !!strata_sample$STRATA) |> 
    collect()
  saveRDS(ipums_sample_tb, file = output_tb_path)
  if(all_exist(output_tb_path)) {
    message(glue("✅ Saved benchmark tb of {year} data with {n_strata} strata to {output_tb_path}."))
  } else {
    warning("❗ Benchmark tb save was unsuccessful.")
  }
  
  # Write the sampled tb to `output_db_path`
  output_db_con <- dbConnect(duckdb::duckdb(), output_db_path)
  copy_to(output_db_con, ipums_sample_tb, "ipums_sample", overwrite = TRUE)
  if(all_exist(output_db_path)) {
    message(glue("✅ Saved benchmark db of {year} data with {n_strata} strata to {output_db_path}."))
  } else {
    warning("❗ Benchmark db save was unsuccessful.")
  }
  
  # Clean up db connections
  dbDisconnect(con, shutdown = TRUE)
  dbDisconnect(output_db_con, shutdown = TRUE)
}

# Example usage (for dev only)
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# Example usage (for dev only)
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = TRUE
)
