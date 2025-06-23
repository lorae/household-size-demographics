# create-benchmark-data.R
#
# The purpose of the script is to modularize the creation of a benchmark sample
# for use in tests of jobs and other functions.
create_benchmark_sample <- function(
    year = 2019,
    n_strata = 3,
    db_path = "data/db/ipums.duckdb",
    benchmark_db_path = "data/db/benchmark.duckdb",
    db_table_name = "ipums_processed",
    benchmark_db_table_name = "ipums_sample",
    force = FALSE # TRUE will recalculate benchmark sample, even if it already exists in cache
) {}
