#kob/scripts/coefs00_2019.R
# ----- STEP 0: Config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(furrr)
library(tibble)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Read in data ----- #
# For now we're going to use a subset of the 2019 data to test functionality
# of the script before expanding to the entire survey dataset

cache_path <- "kob/cache"
n_strata <- 2
year <- 2019

create_benchmark_sample(
  year = year,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  output_dir = cache_path,
  force = TRUE
)

# TODO: have create_benchmark_sample always output paths, so I can call them
# from a list rather than re-construct them here.
tb_path <- glue("{cache_path}/benchmark_sample_{year}_{n_strata}/tb.rds")
ipums_tb <- readRDS(tb_path)

model <- lm(
  data = ipums_tb |> filter(YEAR == year & GQ %in% c(0,1,2)),
  weights = PERWT,
  formula = NUMPREC ~ -1 + tenure # keep it simple for now; add complexity once it works
)

model$coefficients

# Create wrapper function for running above regression model, so it can be bootstrapped
# and replicated using se_from_bootstrap() from `dataduck`
run_reg <- function(wt_col = "PERWT") {
  
  # Defensive: does wt_col exist in ipums_tb?
  if (!wt_col %in% names(ipums_tb)) {
    stop(glue::glue("Column '{wt_col}' not found in the data."))
  }
  
  # Run weighted linear regression
  model <- lm(
    formula = NUMPREC ~ -1 + tenure,
    data = ipums_tb,
    weights = ipums_tb[[wt_col]]
  )
  
  # Return tidy-ish dataframe of coefficients
  coefs <- coef(model) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "value") |>
    dplyr::rename(coef = 2)
  
  return(coefs)
}

# Dev only: example implementation
run_reg() # default wt_col which is PERWT
run_reg(wt_col = "PERWT") # define explicitly
run_reg(wt_col = "REPWTP1") # run using first repwt column

#################### all below this line is not yet refactored

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

tic("Run model 00")
model00_2019 <- svyglm(NUMPREC ~ -1 + 
                         RACE_ETH_bucket +
                         AGE_bucket +
                         EDUC_bucket +
                         INCTOT_cpiu_2010_bucket +
                         us_born +
                         tenure +
                         gender + 
                         cpuma, design = design_2019_survey)
toc()

# Save results in throughput
tic("Save model00_2019 to kob/throughput")
saveRDS(model00_2019, file = "kob/throughput/model00_2019.rds")
toc()