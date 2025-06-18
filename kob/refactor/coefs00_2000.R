# TODO: rename reg00_2000 to prop00_2000.
# The purpose of this script is to estimate regression 1 (with standard errors)
# on data from 2000.

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Read in the pre-subsetted survey
tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("kob/throughput/design_2000_survey.rds")
toc()

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
model00_2000 <- svyglm(NUMPREC ~ -1 + 
                  RACE_ETH_bucket +
                  AGE_bucket +
                  EDUC_bucket +
                  INCTOT_cpiu_2010_bucket +
                  us_born +
                  tenure +
                  gender + 
                  cpuma, design = design_2000_survey)
toc()

# Save results in throughput
tic("Save model00_2000 to kob/throughput")
saveRDS(model00_2000, file = "kob/throughput/model00_2000.rds")
toc()