#kob/scripts/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(tibble)
library(purrr)
library(broom)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Initialize values ----- #
cache_path <- "kob/cache"
n_strata <- 100
year <- 2019
formula <- NUMPREC ~ -1 + tenure + gender + cpuma # for regression

# ----- STEP 2: Read in data ----- #
# For now we're going to use a subset of the 2019 data to test functionality
# of the script. This is temporary and will be replaced with the full ipums_tb
create_benchmark_sample(
  year = year,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  output_dir = cache_path,
  force = FALSE
)

# TODO: have create_benchmark_sample always output paths, so I can call them
# from a list rather than re-construct them here.
tb_path <- glue("{cache_path}/benchmark_sample_{year}_{n_strata}/tb.rds")
ipums_tb <- readRDS(tb_path)

# ----- Step 3: Define regression wrapper function ----- #
# Create wrapper function for running regression model, so it can be bootstrapped
# and replicated using se_from_bootstrap() from `dataduck`
run_reg <- function(
    wt_col = "PERWT",
    data = ipums_tb |> filter(GQ %in% c(0, 1, 2)),
    formula = NUMPREC ~ -1 + tenure
) {
  # Defensive: does `wt_col` exist in `data`?
  if (!wt_col %in% names(data)) {
    stop(glue::glue("Column '{wt_col}' not found in the data."))
  }
  
  design <- svydesign(
    ids = ~1,
    weights = as.formula(paste0("~", wt_col)),
    data = data
  )
  
  model <- svyglm(formula, design = design)
  
  # Return tidy-ish dataframe of coefficients
  coefs <- broom::tidy(model) |> 
    dplyr::select(term, estimate) |> 
    dplyr::rename(value = term, coef = estimate)
  
  print("Regression complete.")
  return(coefs)
}

# Dev only: example implementation
run_reg(
  wt_col = "PERWT",
  data = ipums_tb |> filter(GQ %in% c(0,1,2)),
  formula = formula
)

run_reg(
  wt_col = "REPWTP1", # First repwt column
  data = ipums_tb |> filter(GQ %in% c(0,1,2)),
  formula = formula
)

# ----- Step 4: Apple Successive Differences Replication using `dataduck` -----
# I estimate that this will take about 9 minutes to run this on the full sample 
# using this simple reg.
tic("bootstrap 80 replicates")
input_bootstrap <- bootstrap_replicates(
  data = ipums_tb |> filter(GQ %in% c(0,1,2)),
  f = run_reg,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  id_cols = "value",
  formula = formula
)
toc()

tic("use sdr replicates to calculate SEs")
model_output <- se_from_bootstrap(
  bootstrap = input_bootstrap,
  constant = 4/80,
  se_cols = c("coef")
)
model_output
toc()

# Save results in throughput
tic("Save model to kob/throughput")
saveRDS(model_output, file = "kob/throughput/model00_2019.rds")
toc()
