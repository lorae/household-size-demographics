#kob/scripts/coefs00_2000_numprec.R
# TODO: rename output to p_2000 and adjust downstream kob scripts accordingly
# This script runs regression 0 with NUMPREC as the outcome variable

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(tibble)
library(purrr)
library(broom)
library(glue)

# Initialize year
year <- 2000

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

# Initialize output path
output_path <- glue("throughput/model00_{year}_numprec_summary-v3.rds")

# Read in the pre-subsetted survey
tic("Read survey design as RDS")
design <- readRDS(glue("throughput/design_{year}_survey.rds"))
toc()

tic("Run model")
model <- svyglm(formula, design = design)
toc(log = TRUE)

tic("Extract the coefs and std. errors using broom")
model_summary <- broom::tidy(model)
toc()

# Save results in throughput
tic(glue("Save model to {output_path}"))
saveRDS(model_summary, file = output_path)
toc(log = TRUE)