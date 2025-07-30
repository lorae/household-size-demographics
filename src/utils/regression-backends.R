# src/utils/regression-backends.R
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

# A newer safer version
dataduck_reg_matrix_2 <- function(data, wt_col, formula) {
  # Ensure all inputs are aligned by building a model frame
  mf <- model.frame(formula, data, na.action = na.omit)
  X <- model.matrix(formula, mf) |> as("dgCMatrix")
  y <- model.response(mf)
  
  # Align weights with the model frame
  wts_full <- data[[wt_col]]
  wts <- wts_full[as.integer(rownames(mf))]
  
  # Sanitize weights
  if (any(is.na(wts))) stop("Weights contain NA.")
  if (any(wts < 0)) {
    warning(glue::glue("Negative weights found in '{wt_col}'; replacing with 0."))
    wts <- pmax(wts, 0)
  }
  
  stopifnot(length(wts) == nrow(X))
  
  W <- Diagonal(x = sqrt(wts))
  Xw <- W %*% X
  yw <- W %*% y
  coef_vec <- solve(crossprod(Xw), crossprod(Xw, yw))
  
  tibble(term = rownames(coef_vec), estimate = as.numeric(coef_vec)) |> arrange(term)
}

# Fallback function
dataduck_matrix_lm_fallback <- function(data, wt_col, formula) {
  tryCatch({
    result <- dataduck_reg_matrix(data, wt_col, formula)
    if (any(is.nan(result$estimate)) || any(is.infinite(result$estimate))) {
      message("⚠️ Fallback: NaN or Inf detected in matrix regression. Switching to lm().")
      data[[wt_col]] <- pmax(data[[wt_col]], 0)
      return(dataduck_reg_lm(data, wt_col, formula))
    }
    return(result)
  }, error = function(e) {
    message("⚠️ Fallback: Matrix regression error. Switching to lm().")
    data[[wt_col]] <- pmax(data[[wt_col]], 0)
    return(dataduck_reg_lm(data, wt_col, formula))
  })
}


# Registry of available regression backends
reg_backends <- list(
  lm = dataduck_reg_lm,
  matrix_alg = dataduck_reg_matrix,
  lm_with_matrix_fallback = dataduck_matrix_lm_fallback
)

# Default regression function to use (can be overridden in scripts)
my_reg_function <- reg_backends$lm_with_matrix_fallback

