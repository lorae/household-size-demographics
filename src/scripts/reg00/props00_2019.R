# kob/refactor/props00_2019.R
# The purpose of this script is to calculate population proportions in 2019.

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)

# Read in the pre-subsetted survey
tic("Read 2019 survey design as RDS")
design_2019_survey <- readRDS("throughput/design_2019_survey.rds")
toc()

# Calculate proportions
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
tic("Save props00_2019 to throughput/reg00/")
saveRDS(props00_2019, file = "throughput/reg00/props00_2019.rds")
toc()