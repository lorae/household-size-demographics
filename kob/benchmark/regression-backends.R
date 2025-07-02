# kob/benchmark/regression-backends.R
# This file defines regression backends used for benchmarking standard error methods.
# Each backend must implement a function of the form f(data, wt_col, formula) and return
# a tibble with columns: `term`, `estimate` (sorted by `term`).

library(tibble)
library(parsnip)
library(workflows)
library(Matrix)

# Linear regression using lm() via parsnip
dataduck_reg_lm <- function(data, wt_col, formula) {
  model_spec <- linear_reg() |> set_engine("lm")
  fit_obj <- fit(
    model_spec,
    formula = formula,
    data = data,
    case_weights = frequency_weights(data[[wt_col]])
  )
  broom::tidy(fit_obj) |> select(term, estimate) |> arrange(term)
}

# Linear regression using matrix algebra with sparse design matrix
# Solves: (X'WX)^-1 X'Wy, where W is diagonal sqrt(weights)
dataduck_reg_matrix <- function(data, wt_col, formula) {
  X <- model.matrix(formula, data) |> as("dgCMatrix")
  y <- data[[as.character(formula[[2]])]]
  wts <- data[[wt_col]]
  W <- Diagonal(x = sqrt(wts))
  Xw <- W %*% X
  yw <- W %*% y
  coef_vec <- solve(crossprod(Xw), crossprod(Xw, yw))
  tibble(term = rownames(coef_vec), estimate = as.numeric(coef_vec)) |> arrange(term)
}

# Registry of available regression backends
reg_backends <- list(
  lm = dataduck_reg_lm,
  matrix_alg = dataduck_reg_matrix
)

# Default regression function to use (can be overridden in scripts)
my_reg_function <- reg_backends$matrix_alg
