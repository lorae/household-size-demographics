# kob/scripts/refactor/run-regression.R
# The purpose of this script is to run the regressions as the first stage of the KOB
# decomposition.
library("rlang")

# TODO: add a se_style argument that specifies the way standard errors are to be
# calculated.
# TODO: add formula argument
run_regression <- function(data, weights) {
  weights_col <- eval_tidy(sym(weights), data)
  
  model <- lm(
    formula = NUMPREC ~ HHINCOME_bucket + EDUC_bucket,
    data = data,
    weights = weights_col,
    contrasts = list(
      HHINCOME_bucket = "contr.treatment",
      EDUC_bucket = "contr.treatment"
    )
  )
  
  print(model)
}

run_regression(data= input, weights = "PERWT")