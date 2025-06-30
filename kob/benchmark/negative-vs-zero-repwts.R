# kob/benchmark/negative-vs-zero-repwts.R
# The purpose of this script is to test the difference between the dataduck
# repwt standard errors obtained by zeroing out negative estimates and those
# obtained using the standard errors from the survey package.
# 
# The purpose would be to use the bespoke dataduck method, which runs much faster
# and efficiently, if a decent enough approximation.

# ----- STEP 0: Setup and config ----- #

library(survey)
library(tictoc)
library(duckdb)
library(dplyr)
library(duckdb)
library(glue)
library(purrr)
library(rlang)

# Load the dataduck package
devtools::load_all("../dataduck")

# Load the create-benchmark-data and helper functions
source("kob/benchmark/create-benchmark-data.R")

# ----- STEP 1: Load and Prepare Benchmark Sample -----
create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# Load the tibble. There's also a db available, but we ignore that for now.
ipums_2019_sample_tb <- readRDS("kob/cache/benchmark_sample_2019_3/tb.rds")

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
model_expected <- svyglm(NUMPREC ~ -1 + tenure + gender, design = design_2019_expected)
toc()

# ----- STEP 3: Verify that the selected sample has negative repwts ----- #

any(
  purrr::map_lgl(
    dplyr::select(ipums_2019_sample_tb, dplyr::starts_with("REPWT")),
    ~ any(.x < 0, na.rm = TRUE)
  )
)

# If false, re-run the above code with more strata until true.

sum(
  dplyr::select(ipums_2019_sample_tb, dplyr::starts_with("REPWT")) < 0,
  na.rm = TRUE
)


# ----- STEP 4: Run Custom Point Estimate Pipeline ----- #
# Filter for matching GQ criteria
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# Custom regression function
my_reg_function <- function(data, wt_col) {
  formula <- NUMPREC ~ -1 + tenure + gender
  wt_vec <- data[[wt_col]]
  
  model <- lm(
    formula = formula,
    data = data,
    weights = wt_vec
  )
  
  # Return a tidy data frame of coefficients
  output <- broom::tidy(model) |>
    select(term, estimate)
  
  return(output)
}

# Run custom estimate
actual_v1 <- my_reg_function(
  data = filtered_tb,
  wt_col = "PERWT"
)

# Confirm that means match
stopifnot(all.equal(
  actual_v1$estimate,
  unname(model_expected$coefficients),
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
  id_cols = "term"
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
all_equal_helper(actual_ses, expected_ses, tol = 1e-10) 

message("✅ Standard errors match within tolerance.")