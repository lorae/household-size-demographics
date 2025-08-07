# src/scripts/reg01/2000_prop.R
#
# This script runs the props for regression 00 in 2000.

# TODO: explore whether I can generate a survey design without pulling data into
# memory. Example code below.
# TODO: add a test comparing outputs and computation time from database and non-database
# to see if it works / is better.
# design_db <- svydesign(
#   ids    = ~CLUSTER,
#   strata = ~STRATA,
#   weights= ~PERWT,
#   data   = "ipums_processed",
#   dbtype = "DuckDB",
#   dbname = "data/db/ipums.duckdb",
#   nest   = TRUE
# ) %>%
#   subset(GQ %in% c(0,1,2))
# # documentation mentioning database driver:
# # https://www.rdocumentation.org/packages/survey/versions/4.4-2/topics/svydesign

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(glue)

# Initialize output path
out_path <- "throughput/reg01/2000_prop.rds"

# Read in the pre-subsetted survey
tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("throughput/design_2000_survey.rds")
toc()

# Source script that defines reg01_predictors
source("src/scripts/reg01/define_formula.R")
prop_vars <- reg01_predictors # Defined in define_formula.R, sourced above

# Convert to one-sided formulas: ~varname
formulas <- lapply(prop_vars, function(var) as.formula(paste0("~", var)))

# Set up parallel plan (this will use available cores)
options(future.globals.maxSize = 10 * 1024^3)
plan(multicore)

tic("Parallelized population proportions (forked)")
props00_2000 <- future_map(formulas, ~svymean(.x, design_2000_survey))
toc()

names(props00_2000) <- prop_vars

# Save results in throughput
message(glue("Saving prop output. Output path: {out_path}"))

tic(glue("Save results to {out_path}"))
saveRDS(props00_2000, file = out_path)
toc()
