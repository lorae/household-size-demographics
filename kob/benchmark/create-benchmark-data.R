# create-benchmark-data.R
#
# The purpose of the script is to modularize the creation of a benchmark sample
# for use in tests of jobs and other functions.
#
# create_benchmark_sample
# input:
# - args given below
# output:
# - it will save the created benchmark to a directory called
# `kob/cache/benchmark_sample_{year}_{n_strata}`
# - There will be one file in that folder with the db and one with the tb
create_benchmark_sample <- function(
    year = 2019,
    n_strata = 3,
    db_path = "data/db/ipums.duckdb",
    benchmark_db_path = "data/db/benchmark.duckdb",
    db_table_name = "ipums_processed",
    benchmark_db_table_name = "ipums_sample",
    force = FALSE # TRUE will recalculate benchmark sample, even if it already exists in cache
) {
  # Create output_file variable which is just the path to the cache where the output 
  # will be stored
  # Check if both the db and dbs with expected file names exist inthe directory. Use
  # this info to assign output_dne = {t, f} where dne stands for does not exist.
  # assign true if one or both files are missing. False if everything that should be 
  # there is already there.
  # Then:
  # if file DNE:
  #   RUN CODE
  # if force:
  #   RUN CODE
  # (the other case, where FILE DNE AND FORCE are both F, means the code shouldn't 
  # run. This logic can simplify to if FILE DNE OR FORCE, run, else, don't run.)
  
  # Then run logic to create the tibble and db
  
  # Save them
  
  # unit test comparing them can be external.
  
}
