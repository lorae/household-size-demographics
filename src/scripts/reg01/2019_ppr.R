# src/scripts/reg01/2019_ppr.R
cat("
This script estimates regression coefficients and standard errors for the 2019 IPUMS sample
using the dataduck matrix-based regression backend and successive differences replication (SDR).
This is the main production pipeline.
")

# ----- Step 0: User settings ----- #
# Define output path for model summary
out_path <- "throughput/reg01/2019_ppr.rds"

# Define regression formula
source("src/scripts/reg01/define_formula.R")
formula <- get_formula(
  outcome_var = "persons_per_room", 
  predictors = reg01_predictors, 
  has_intercept = TRUE
)

# ----- Step 1: Config ----- #

library(tictoc)
library(dplyr)
library(glue)
library(purrr)
library(duckdb)
library(devtools)
library(future)
library(furrr)

# dataduck internal package & helper scripts
load_all("../dataduck")
source("src/utils/regression-backends.R")

# Plan parallel session
options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB
plan(multicore, workers = 10)

# ----- Step 2: Load data from saved ipums_2019_tb tibble ----- #
tic("Collect 2019 ipums tibble")
ipums_2019_tb <- readRDS("kob/throughput/ipums_2019_tb.rds")
toc()

tic("Filter out group quarters residents")
filtered_tb <- ipums_2019_tb |> filter(GQ %in% c(0, 1, 2))
toc()

# ----- Step 4: Estimate model using dataduck reg backend ----- #
# TODO: refactor to select reg function from reg_backends list at top of script
tic("Run bootstrap replicates")
bootstrap_results <- bootstrap_replicates_parallel(
  data = filtered_tb,
  f = dataduck_reg_matrix_2,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  # constant = 4 / 80,
  # se_cols = c("estimate"),
  id_cols = "term",
  formula = formula,
  verbose = TRUE
)
toc()

tic("Get model results")
model_output <- se_from_bootstrap(
  bootstrap = bootstrap_results,
  constant = 4 / 80,
  se_cols = c("estimate")
)
toc()
# ----- Step 4: Save results ----- #
message(glue("Saving model output. Output path: {out_path}"))

tic("Save model output")
saveRDS(model_output, file = out_path)
toc()
