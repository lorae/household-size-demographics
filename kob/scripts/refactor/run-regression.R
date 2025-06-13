# kob/scripts/refactor/run-regression.R
# The purpose of this script is to run the regressions as the first stage of the 
# KOB decomposition.
library(rlang)
library(glue)
library(dplyr)

run_regression <- function(
    data,           # The source data set to run the regression on
    weights,        # String name of the column containing weights
    varnames_dict,  # Automatically parsed into the RHS of the formula
    outcome_var     # String name of the outcome variable
) {
  message(glue("The name of the data set is: {deparse(substitute(data))}"))
  glimpse(data)
  
  # --- Validation checks ---
  all_vars <- colnames(data)
  
  # Check varnames_dict keys
  missing_predictors <- setdiff(names(varnames_dict), all_vars)
  if (length(missing_predictors) > 0) {
    stop(glue("The following predictors in varnames_dict are not in the data: {paste(missing_predictors, collapse = ', ')}"))
  }
  
  # Check weights
  if (!(weights %in% all_vars)) {
    stop(glue("Weights column '{weights}' not found in the data."))
  }
  
  # Check outcome variable
  if (!(outcome_var %in% all_vars)) {
    stop(glue("Outcome variable '{outcome_var}' not found in the data."))
  }
  
  # --- Proceed with model construction ---
  # weights
  weights_col <- eval_tidy(sym(weights), data)
  message(glue("The name of the weights variable is: {weights}"))
  
  # formula
  rhs_string <- names(varnames_dict) |> paste(collapse = " + ")
  formula_string <- paste(outcome_var, "~", rhs_string)
  formula <- as.formula(formula_string)
  message(glue("The formula_string is: {formula_string}"))
  
  # contrasts: setting used by `lm` function to specify dummy coding of categorical 
  # variables with ordered factors
  contrasts_list <- lapply(varnames_dict, function(x) "contr.treatment")
  names(contrasts_list) <- names(varnames_dict)
  message("The contrasts_list is:")
  print(contrasts_list)
  
  # --- Run the model ---
  model <- lm(
    formula = formula,
    data = data,
    weights = weights_col,
    contrasts = contrasts_list
  )
  
  return(model)
}
