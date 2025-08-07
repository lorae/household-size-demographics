# src/scripts/reg01/define_formula.R
#
# The purpose of this script is to serve as a universal source for the regression
# equation used in the scripts in this directory.

# Formula function
reg01_predictors <- c("RACE_ETH_bucket", "AGE_bucket", "EDUC_bucket", 
                "INCTOT_cpiu_2010_bucket", "us_born", "tenure", 
                "gender", "cpuma")

# Function to produce the reg01 (or any other) formula
get_formula <- function(outcome_var, predictors, is_intercept = TRUE) {
  return(reformulate(predictors, response = outcome_var, intercept = is_intercept))
}
