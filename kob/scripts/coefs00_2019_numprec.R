#kob/scripts/coefs00_2019_numprec.R
# This script runs regression 0 with NUMPREC as the outcome variable

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(tibble)
library(purrr)
library(broom)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# Initialize formula
formula <- NUMPREC ~ -1 + 
  RACE_ETH_bucket +
  AGE_bucket +
  EDUC_bucket +
  INCTOT_cpiu_2010_bucket +
  us_born +
  tenure +
  gender + 
  cpuma

# Read in the pre-subsetted survey
tic("Read survey design as RDS")
design <- readRDS("kob/throughput/design_2019_survey.rds")
toc()

tic("Subset the survey by GQ")
design <- subset(design, GQ %in% c(0, 1, 2))
toc(log = TRUE)

tic("Run model")
model <- svyglm(formula, design = design)
toc(log = TRUE)

tic("Extract the coefs and std. errors using broom")
model_summary <- broom::tidy(model)
toc()

# Save results in throughput
tic("Save model to kob/throughput")
saveRDS(model_summary, file = "kob/throughput/model00_2019_numprec_summary-v2.rds")
toc(log = TRUE)