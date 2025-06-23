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
#' Determines whether a step should run based on file existence.
#' Returns TRUE if any file is missing, FALSE otherwise.
#' TODO: this might be later replaced with `targets` package
#'
#' @param ... One or more file paths
#' @return Logical scalar: TRUE if any file is missing, FALSE otherwise
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
  output_path = glue("kob/cache/benchmark_sample_{year}_{n_strata}") |> as.character()
  output_tb <- glue("{output_path}/tb.rds") |> as.character()
  output_db <- glue("{output_path}/db.duckdb") |> as.character()
  
  outputs_exist <- all_exist(output_db, output_tb)
  
  # Early break
  if (outputs_exist & !force) {
    message("✅ Benchmark files already exists and user has opted force == FALSE, so no files were generated")
    return(NULL)
  }

  # Continue
  message("⚙️  Generating benchmark files...")
  
  # Ensure the output_dir already exists. If it doesn't, create it.
  # TODO: rename output_path to output_dir
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  
  
  # Connect to DB; assign alias to table
  con <- dbConnect(duckdb::duckdb(), db_path)
  ipums_db <- tbl(con, db_table_name)

  # Sample {`n_strata`} strata from {`year`} data
  strata_sample <- ipums_db |> 
    filter(YEAR == year) |> 
    distinct(STRATA) |> 
    arrange(sql("RANDOM()")) |>
    head(n_strata) |>
    collect()
  
  # Create the sampled tb & write to `output_tb`
  ipums_sample_tb <- ipums_db |> 
    filter(YEAR == year, STRATA %in% !!strata_sample$STRATA) |> 
    collect()
  saveRDS(ipums_sample_tb, file = output_tb)
  if(all_exist(output_tb)) {
    message(glue("Saved benchmark tb of {year} data with {n_strata} strata to {output_tb}."))
  } else {
    warning("Benchmark tb save was unsuccessful.")
  }
  
  # Write the sampled tb to `output_db`
  output_db_con <- dbConnect(duckdb::duckdb(), output_db)
  copy_to(output_db_con, ipums_sample_tb, "ipums_sample", overwrite = TRUE)
  #message(glue("Saved benchmark db of {year} data with {n_strata} strata to {output_db}."))

  # Sanity check: both output files should now exist
  should_run(output_db, output_tb) # hopefully returns FALSE
  # message(glue("Benchmark data extracts successfully saved to {output_path}."))
}

# Example usage (for dev only)
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)
