# Config
source("kob/benchmark/create-benchmark-data.R")

# Example usages (for dev only) - move to unit tests
# Expect to not execute; return early due to cached results
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

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