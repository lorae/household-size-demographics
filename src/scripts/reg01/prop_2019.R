# src/scripts/reg01/prop_2019.R
# 
# The purpose of this script is to calculate population proportions in 2019.

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Initialize output path
out_path <- "throughput/reg01/prop_2019.rds"

# Read in the pre-subsetted survey
tic("Read 2019 survey design as RDS")
design_2019_survey <- readRDS("throughput/design_2019_survey.rds")
toc()

# Calculate proportions: Source script that defines reg01_predictors
source("src/scripts/reg01/define_formula.R")
prop_vars <- reg01_predictors # Defined in define_formula.R, sourced above

# Convert to one-sided formulas: ~varname
formulas <- lapply(prop_vars, function(var) as.formula(paste0("~", var)))

# Set up parallel plan (this will use available cores)
options(future.globals.maxSize = 30 * 1024^3)
plan(multicore)

tic("Parallelized population proportions (forked)")
props00_2019 <- future_map(formulas, ~svymean(.x, design_2019_survey))
toc()

names(props00_2019) <- prop_vars

# Save results in throughput
message(glue("Saving prop output. Output path: {out_path}"))

tic(glue("Save results to {out_path}"))
saveRDS(props00_2019, file = out_path)
toc()