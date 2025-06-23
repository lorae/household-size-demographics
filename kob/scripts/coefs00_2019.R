#kob/scripts/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Read in data ----- #
# For now we're going to use a subset of the 2019 data to test functionality
# of the script before expanding to the entire survey dataset

cache_path <- "kob/cache"
n_strata <- 2

create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  output_dir = cache_path,
  force = TRUE
)

db_path <- glue("{cache_path}/benchmark_sample_2019_{n_strata}/db.duckdb")
con <- dbConnect(duckdb::duckdb(), db_path)
ipums_db <- tbl(con, "ipums_sample")

ipums_db |> head() |> collect() |> View() # Looks good

#################### all below this line is not yet refactored

prop_vars <- c(
  "RACE_ETH_bucket", 
  "AGE_bucket", 
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket", 
  "us_born", 
  "tenure", 
  "gender",
  "cpuma"
)

tic("Run model 00")
model00_2019 <- svyglm(NUMPREC ~ -1 + 
                         RACE_ETH_bucket +
                         AGE_bucket +
                         EDUC_bucket +
                         INCTOT_cpiu_2010_bucket +
                         us_born +
                         tenure +
                         gender + 
                         cpuma, design = design_2019_survey)
toc()

# Save results in throughput
tic("Save model00_2019 to kob/throughput")
saveRDS(model00_2019, file = "kob/throughput/model00_2019.rds")
toc()