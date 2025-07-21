# src/utils/regression-tools.R
# TODO: rename to regression-postprocess-tools or something?
# TODO: move the regression backends to this dir as well
#
# The purpose of this script is to provide testable, modular functions for transforming
# regression results
#
#
library(devtools)
source("kob/scripts/kob-function.R") # The kob funciton is used within add-intercept()


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
        "' and value = '", reference_value, "', but found ", nrow(reference_row), "."
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
      variable = "(Intercept)",
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

# REMOVE INTERCEPT
remove_intercept <- function(
    reg_result,
    variable # Variable to push intercept to
){
  # not needed for now, but would be nice to have. Should reverse add_intercept 
  # perfectly. A good unit test would check if that's the case
}
