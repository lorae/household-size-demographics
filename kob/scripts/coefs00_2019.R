#kob/scripts/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(tibble)
library(purrr)
library(broom)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Initialize values ----- #
cache_path <- "kob/cache"
n_strata <- 3
year <- 2019
formula <- NUMPREC ~ -1 + tenure + gender + cpuma # for regression

# ----- STEP 2: Read in data ----- #
# For now we're going to use a subset of the 2019 data to test functionality
# of the script. This is temporary and will be replaced with the full ipums_tb
create_benchmark_sample(
  year = year,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  output_dir = cache_path,
  force = FALSE
)

# TODO: have create_benchmark_sample always output paths, so I can call them
# from a list rather than re-construct them here.
tb_path <- glue("{cache_path}/benchmark_sample_{year}_{n_strata}/tb.rds")
ipums_tb <- readRDS(tb_path)

# ----- Step 3: Define regression wrapper function ----- #
# Read in the pre-subsetted survey
# tic("Read 2019 survey design as RDS")
# design_2019_survey <- readRDS("kob/throughput/design_2019_survey.rds")
# toc()

# Since we're running on a benchmark subset, we'll DIY the survey design
tic("Design the 2019 survey")
design_2019_survey <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",  # regex pattern to match columns
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_tb
)
design_2019_survey <- subset(design_2019_survey, GQ %in% c(0, 1, 2))
toc()

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