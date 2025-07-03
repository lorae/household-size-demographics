# kob/throughput/coefs00_2019_bedroom.R
# TODO: Parallelize bootstrap_replicates and this code
cat("
This script estimates regression coefficients and standard errors for the 2019 IPUMS sample
using the dataduck matrix-based regression backend and successive differences replication (SDR).
This is the main production pipeline.
")

# ----- Step 0: User settings ----- #
# Define output path for model summary
out_path <- "kob/throughput/model00_2019_bedroom_summary.rds"

# Define regression formula
formula <- bedroom ~ -1 + 
  RACE_ETH_bucket +
  AGE_bucket +
  EDUC_bucket +
  INCTOT_cpiu_2010_bucket +
  us_born +
  tenure +
  gender +
  cpuma

# ----- Step 1: Config ----- #

library(tictoc)
library(dplyr)
library(glue)
library(purrr)
library(duckdb)
library(devtools)

# dataduck internal package & helper scripts
load_all("../dataduck")
source("kob/benchmark/regression-backends.R")

# ----- Step 2: Load data from saved ipums_2019_tb tibble ----- #
tic("Collect 2019 ipums tibble")
ipums_2019_tb <- readRDS("kob/throughput/ipums_2019_tb.rds")
toc()

tic("Filter out group quarters residents")
filtered_tb <- ipums_2019_tb |> filter(GQ %in% c(0, 1, 2))
toc()

# ----- Step 3: Estimate model using dataduck matrix backend ----- #
tic("Estimate coefficients and SEs")
model_output <- estimate_with_bootstrap_se(
  data = filtered_tb,
  f = dataduck_reg_matrix,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  constant = 4 / 80,
  se_cols = c("estimate"),
  id_cols = "term",
  formula = formula,
  verbose = TRUE
)
toc()

# ----- Step 4: Save results ----- #
message(glue("Saving model output. Output path: {out_path}"))

tic("Save model output")
saveRDS(model_output, file = out_path)
toc()
