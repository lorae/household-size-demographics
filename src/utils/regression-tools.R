# src/utils/regression-tools.R
#
# The purpose of this script is to provide testable, modular functions for transforming
# regression results
#
#

# ----- Step 0: Config ----- #

library(devtools)

# Define number of strata to use
n_strata <- 3

# Define regression formulas
formula_int <- BEDROOMS ~ RACE_ETH_bucket + tenure
formula_no_int <- BEDROOMS ~ -1 + RACE_ETH_bucket + tenure

# Load internal packages and helpers
load_all("../dataduck")
source("kob/benchmark/create-benchmark-data.R")
source("kob/benchmark/regression-backends.R")
source("kob/scripts/kob-function.R")

# ----- Step 1: Load and prepare sample ----- #
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("kob/cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# ----- Step 2: Prepare regressions with and without intercepts ----- #
reg_int <- dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT", 
  formula = formula_int) |>
  kob_tidy_output() |>
  rename(coef_2000 = estimate) |>
  mutate(
    coef_2000_se = 0,
    coef_2019 = 0,
    coef_2019_se = 0,
    prop_2000 = 0,
    prop_2000_se = 0,
    prop_2019 = 0,
    prop_2019_se = 0,
    u = 0,
    u_se = 0,
    e = 0,
    e_se = 0,
    c = 0, 
    c_se = 0
  )

reg_no_int <- dataduck_reg_lm(
  data = filtered_tb,
  wt_col = "PERWT", 
  formula = formula_no_int) |>
  kob_tidy_output() |>
  rename(coef_2000 = estimate) |>
  mutate(
    coef_2000_se = 0,
    coef_2019 = 0,
    coef_2019_se = 0,
    prop_2000 = 0,
    prop_2000_se = 0,
    prop_2019 = 0,
    prop_2019_se = 0,
    u = 0,
    u_se = 0,
    e = 0,
    e_se = 0,
    c = 0, 
    c_se = 0
  )

# ----- Step 3: Define add_intercept function and remove_intercept function ----- #
# ADD INTERCEPT
add_intercept <- function(
    reg_data,
    variable, # Variable to draw intercept from
    reference_value # value of variable that will become intercept
  ){
  # Validate that the variable exists in the 'variable' column
  if (!(variable %in% reg_data$variable)) {
    stop(paste0("Variable '", variable, "' not found in reg_data$variable."))
  }
  
  # Validate that the reference_value exists in the 'value' column
  if (!(reference_value %in% reg_data$value)) {
    stop(paste0("Reference value '", reference_value, "' not found in reg_data$value."))
  }
  
  # Validate that the reference_value and variable exist together in exactly one row
  reference_row <- reg_data |>
    filter(.data$variable == variable, .data$value == reference_value)
  
  if (nrow(reference_row) != 1) {
    stop(
      paste0(
        "Expected exactly one row with variable = '", variable,
        "' and value = '", reference_value, "', but found ", nrow(matching_rows), "."
      )
    )
  }
  
  # Validate that all the required cols are present
  required_cols <- c("variable", "value", "coef_2000", "coef_2000_se",
                     "u", "u_se", "e", "e_se", "c", "c_se")
  missing_cols <- setdiff(required_cols, colnames(reg_data))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required column(s): ", paste(missing_cols, collapse = ", ")))
  }
  
  # Pull the affected rows that will be changed by the function
  affected_rows <- reg_data |>
    filter(variable == !!variable)
  
  reference_row <- reg_data |>
    filter(value == !!reference_value)
  
  # Subtract the reference row from the affected rows; modify affected values
  affected_rows_modified <- affected_rows |> 
    filter(value != !!reference_value) |> 
    mutate(
      coef_2000    = coef_2000 - reference_row$coef_2000,
      coef_2019    = coef_2019 - reference_row$coef_2019,
      coef_2000_se = sqrt(coef_2000_se^2 + reference_row$coef_2000_se^2),
      coef_2019_se = sqrt(coef_2019_se^2 + reference_row$coef_2019_se^2)
    )
  
  # Construct the new intercept row
  intercept_row <- reference_row |> 
    mutate(
      term = "(Intercept)",
      value = "(Intercept)",
      coef_2000    = coef_2000,
      coef_2019    = coef_2019,
      coef_2000_se = coef_2000_se,
      coef_2019_se = coef_2019_se,
      prop_2000    = NA_real_,
      prop_2000_se = NA_real_,
      prop_2019    = NA_real_,
      prop_2019_se = NA_real_
    )
  
  # Combine together all the new rows, recompute their kob variables
  new_rows <- bind_rows(intercept_row, affected_rows_modified) |> 
    select(-c("u", "u_se", "e", "e_se", "c", "c_se")) |>
    kob()
  
  output <- reg_data |> 
    filter(variable != !!variable) |> 
    bind_rows(new_rows)
  
}

# example usage
add_intercept(reg_no_int, variable = "RACE_ETH_bucket", reference_value = "AAPI")
# example where it should stop vc variable not found
add_intercept(reg_no_int, variable = "Peaches", reference_value = "White")
# example where it should stop bc value not found
add_intercept(reg_no_int, variable = "RACE_ETH_bucket", reference_value = "Peaches")

# REMOVE INTERCEPT
remove_intercept <- function(
    reg_result,
    variable # Variable to push intercept to
){
  # not needed for now, but would be nice to have. Should reverse add_intercept 
  # perfectly. A good unit test would check if that's the case
}
