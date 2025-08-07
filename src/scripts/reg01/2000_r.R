# src/scripts/reg01/2000_r.R
# 
# This script runs regression 1 with `room` as the outcome variable

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

# Define regression formula
source("src/scripts/reg01/define_formula.R")
formula <- get_formula(
  outcome_var = "room", 
  predictors = reg01_predictors, 
  has_intercept = TRUE
  )

# Initialize output path
output_path <- glue("throughput/reg01/{year}_r.rds")

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