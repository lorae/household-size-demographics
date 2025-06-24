#kob/scripts/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(tibble)
library(purrr)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Read in data ----- #
# For now we're going to use a subset of the 2019 data to test functionality
# of the script before expanding to the entire survey dataset

cache_path <- "kob/cache"
n_strata <- 10
year <- 2019

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

# Initialize a formula. Keeping it simple for now.
formula <- NUMPREC ~ -1 + tenure

model <- lm(
  data = ipums_tb |> filter(YEAR == year & GQ %in% c(0,1,2)),
  weights = PERWT,
  formula = formula
)

model$coefficients

# Create wrapper function for running above regression model, so it can be bootstrapped
# and replicated using se_from_bootstrap() from `dataduck`
run_reg <- function(
    wt_col = "PERWT",
    data = ipums_tb |> filter(GQ %in% c(0, 1, 2)),
    formula
) {
  # Defensive: does `wt_col` exist in `data`?
  if (!wt_col %in% names(data)) {
    stop(glue::glue("Column '{wt_col}' not found in the data."))
  }
  
  # Add temporary weight column
  data$.__wt__ <- data[[wt_col]]
  
  # Run weighted linear regression
  model <- lm(
    formula = formula,
    data = data,
    weights = .__wt__
  )
  
  # Return tidy-ish dataframe of coefficients
  coefs <- coef(model) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "value") |>
    dplyr::rename(coef = 2)
  
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
