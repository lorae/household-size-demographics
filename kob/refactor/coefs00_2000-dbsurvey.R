# kob/refactor/coefs00_2000-dbsurvey
# This script runs the props for regression 00 in 2000.

# The purpose of this script is to mimic the functionality of coefs00-2000.R but 
# see if I can base the survey design object off a DB directly, rather than pulling
# the data into memory.

# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(duckdb)

# Database API connection
# open the DuckDB driver and connection
drv <- duckdb::duckdb()
con <- DBI::dbConnect(drv, "data/db/ipums.duckdb")
#ipums_db <- tbl(con, "ipums_processed")

tic("Read survey design from duckdb")
design_db <- svydesign(
  ids    = ~CLUSTER,
  strata = ~STRATA,
  weights= ~PERWT,
  data   = NULL,                    # <- must be NULL for a DB‐backed design
  dbname =  "data/db/ipums.duckdb",
  dbtype =  drv,                    # <- pass the driver object, not a string
  table  = "ipums_processed",       # <- name of the table in your DuckDB file
  nest   = TRUE
) %>%
  subset(GQ %in% c(0,1,2))
toc()

tic("Read 2000 survey design as RDS")
design_2000_survey <- readRDS("kob/throughput/design_2000_survey.rds")
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
options(future.globals.maxSize = 10 * 1024^3)
plan(multicore)

tic("Parallelized population proportions (forked)")
props00_2000 <- future_map(formulas, ~svymean(.x, design_2000_survey))
toc()

names(props00_2000) <- prop_vars

# Save results in throughput
tic("Save props00_2000 to kob/throughput")
saveRDS(props00_2000, file = "kob/throughput/props00_2000.rds")
toc()