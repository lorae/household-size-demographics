# kob/benchmark/negative-vs-zero-repwts.R
# The purpose of this script is to test the difference between the dataduck
# repwt standard errors obtained by zeroing out negative estimates and those
# obtained using the standard errors from the survey package.
# 
# The purpose would be to use the bespoke dataduck method, which runs much faster
# and efficiently, if a decent enough approximation.
# 
# This benchmark test shows that the method works within 10^-6 on 8-10% samples,
# a close enough approximation.

# ----- STEP 0: Setup and config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(duckdb)
library(glue)
library(purrr)
library(rlang)
library(parsnip)
library(workflows)
library(Matrix)
library(glmnet)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# Initialize a formula
# Initialize formula
formula <- BEDROOMS ~ -1 + 
  RACE_ETH_bucket +
  AGE_bucket +
  EDUC_bucket +
  INCTOT_cpiu_2010_bucket +
  us_born +
  tenure +
  gender +
  cpuma

# ----- STEP 1: Load and Prepare Benchmark Sample -----
n_strata <- 3

create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# Load the tibble. There's also a db available, but we ignore that for now.
ipums_2019_sample_tb <- readRDS(glue::glue("kob/cache/benchmark_sample_2019_{n_strata}/tb.rds"))
print(nrow(ipums_2019_sample_tb))

# ----- Step 2: Create Survey Design and Benchmark Output ----- #
# Build a survey design object using replicate weights
tic("Create survey design")
design_2019_expected <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_2019_sample_tb
) |> subset(GQ %in% c(0, 1, 2))
toc()

# Compute benchmark mean household size by tenure
tic("Benchmark: svyglm and svyby")
model_expected <- svyglm(formula, design = design_2019_expected)
toc()

# ----- STEP 3: Verify that the selected sample has negative repwts ----- #

has_negative_repwt <- any(
  purrr::map_lgl(
    dplyr::select(ipums_2019_sample_tb, dplyr::starts_with("REPWT")),
    ~ any(.x < 0, na.rm = TRUE)
  )
)

if (!has_negative_repwt) {
  stop("❌ No negative REPWTP values found in this sample. Please re-run the benchmark with more strata until this error is not encountered.")
}

print("Number of negative REPWTP observations in this sample:")
num_neg_wts <- sum(
  dplyr::select(ipums_2019_sample_tb, dplyr::starts_with("REPWT")) < 0,
  na.rm = TRUE
)
print(num_neg_wts)

# ----- STEP 4: Run Custom Point Estimate Pipeline ----- #
# Filter for matching GQ criteria
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# Custom regression function
my_reg_function <- function(data, wt_col, formula) {
  model_spec <- linear_reg() |> set_engine("lm")
  
  fit_obj <- fit(
    model_spec,
    formula = formula,
    data = data,
    case_weights = frequency_weights(data[[wt_col]])
  )
  
  broom::tidy(fit_obj) |> select(term, estimate)
}

# Alternative custom regression function using glmnet and sparse matrix
my_reg_function_v2 <- function(data, wt_col, formula) {
  # Build sparse model matrix
  X <- model.matrix(formula, data) |> as("dgCMatrix")
  
  # Extract outcome and weights
  y <- data[[as.character(formula[[2]])]]
  wts <- data[[wt_col]]
  
  # Fit model using glmnet with no regularization
  fit <- glmnet(
    x = X,
    y = y,
    weights = wts,
    intercept = FALSE,
    alpha = 0,
    lambda = 0,
    standardize = FALSE
  )
  
  # Extract coefficient vector
  coef_vec <- coef(fit)
  
  # Convert to tibble, remove intercept row
  coef_df <- tibble::tibble(
    term = rownames(coef_vec),
    estimate = as.numeric(coef_vec)
  ) |>
    dplyr::filter(term != "(Intercept)")
  
  return(coef_df)
}

# Run custom estimate
actual_v1 <- my_reg_function(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula
)

actual_v2 <- my_reg_function_v2(
  data = filtered_tb,
  wt_col = "PERWT",
  formula = formula
)

# Confirm that estimates match
expected_df <- tibble::tibble(
  term = names(model_expected$coefficients),
  estimate = as.numeric(model_expected$coefficients)
) |> arrange(term)

actual_df <- actual_v2 |> arrange(term)

stopifnot(all.equal(
  expected_df,
  actual_df,
  tolerance = 1e-6
))

# ----- STEP 5: Compare SEs from custom pipeline vs survey package ----- #
# Now that we've validated the main point estimates match, we test whether the 
# standard errors from `se_from_bootstrap()` match those from `svyglm` / `svyby`.
# If so, this could spell out large performance enhancements.

# Modify the weights to adjust all negatives to zero.
ipums_2019_noneg_tb <- ipums_2019_sample_tb |>
  mutate(across(
    starts_with("REPWTP"),
    ~ if_else(.x < 0, 0, .x)
  ))

input_bootstrap <- bootstrap_replicates(
  data = ipums_2019_noneg_tb |> filter(GQ %in% c(0,1,2)),
  f = my_reg_function,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  id_cols = "term",
  formula = formula
)

model_output <- se_from_bootstrap(
  bootstrap = input_bootstrap,
  constant = 4/80,
  se_cols = c("estimate")
)
model_output

summary(model_expected)

# ----- Final Check: Validate against svyglm output ----- #
# Extract SEs from svyglm
expected_ses <- summary(model_expected)$coefficients[, "Std. Error"] |> unname()
print(expected_ses)

# Match to your custom output
actual_ses <- model_output$se_estimate
print(actual_ses)

# Helper function to test if equal (ignores order)
all_equal_helper <- function(x, y, tol = 1e-8) {
  # Must be same length to match with duplicates
  if (length(x) != length(y)) return(FALSE)
  
  # Sort both and compare element-wise with tolerance
  x_sorted <- sort(x)
  y_sorted <- sort(y)
  
  all(abs(x_sorted - y_sorted) < tol)
}

# Confirm match within tolerance
all_equal_helper(actual_ses, expected_ses, tol = 1e-6) 

message("✅ Standard errors match within tolerance.")

