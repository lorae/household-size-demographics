# kob/benchmark/negative-vs-zero-repwts.R
cat("\nThis script benchmarks whether the `lm()` regression backend with zeroed-out
negative replicate weights matches the point estimates and approximates standard errors
from svyglm on 2019 data. Unlike the matrix-based solver, `lm()` requires that
all weights be non-negative, so negative replicate weights are set to zero as a workaround.
Theoretically, this has no impact on the coefficients, but it does have an impact
on standard errors. This test evaluates how close the resulting estimates are to 
survey-design-based inference when using this approximation. The higher-level purpose
is to determine whether the `dataduck` method, which runs much faster, is a reliable
substitute for the `survey` package.\n\n")

# ----- Step 0: User settings ----- #

# Define tolerance for coefficient equality (expect exact match)
tol_coef <- 1e-10

# Define tolerance for standard error approximation (expect near match)
tol_se <- 1e-6

# Define number of strata to use
n_strata <- 3

# Define regression formula
formula <- BEDROOMS ~ -1 + 
  RACE_ETH_bucket +
  AGE_bucket +
  EDUC_bucket +
  INCTOT_cpiu_2010_bucket +
  us_born +
  tenure +
  gender +
  cpuma

# ----- Step 1: Config ----- #

library(survey)
library(tictoc)
library(dplyr)
library(glue)
library(purrr)
library(devtools)

# Load internal packages and helpers
load_all("../dataduck")
source("kob/benchmark/create-benchmark-data.R")
source("kob/benchmark/regression-backends.R")

# ----- Step 2: Load and prepare sample ----- #
create_benchmark_sample(
  year = 2019,
  n_strata = n_strata,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

ipums_2019_sample_tb <- readRDS(glue("kob/cache/benchmark_sample_2019_{n_strata}/tb.rds"))
filtered_tb <- ipums_2019_sample_tb |> filter(GQ %in% c(0, 1, 2))

# Count negative replicate weights
num_neg_wts <- sum(
  select(ipums_2019_sample_tb, starts_with("REPWTP")) < 0,
  na.rm = TRUE
)
num_wts <- sum(
  select(ipums_2019_sample_tb, starts_with("REPWTP")),
  na.rm = TRUE
)
if (num_neg_wts == 0) {
  stop("❌ This sample has no negative replicate weights, so this test is not meaningful. 
       This is not your fault: your random sample simply did no pick up any negative
       weights. Try re-running this script with `force = TRUE` in `create_benchmark_sample()` 
       to draw a new sample of the same size, and/or by changing `n_strata` at the top
       of this script to draw a new sample of a different size.")          
} else {
  message(glue("{num_neg_wts} negative replicate weights detected of {num_wts} total weights."))
}

# Zero out negative replicate weights
ipums_2019_noneg_tb <- ipums_2019_sample_tb |> mutate(across(
  starts_with("REPWTP"),
  ~ if_else(.x < 0, 0, .x)
))
filtered_noneg_tb <- ipums_2019_noneg_tb |> filter(GQ %in% c(0, 1, 2))

# ----- Step 3: Benchmark using svyglm ----- #
tic("create survey design")
design_2019_expected <- svrepdesign(
  weights = ~PERWT,
  repweights = "REPWTP[0-9]+",
  type = "Fay",
  rho = 0.5,
  mse = TRUE,
  data = ipums_2019_sample_tb
) |> subset(GQ %in% c(0, 1, 2))
toc()

tic("fit svyglm")
model_expected <- svyglm(formula, design = design_2019_expected)
toc()

expected_coefs <- tibble(
  term = names(model_expected$coefficients),
  estimate = as.numeric(model_expected$coefficients)
) |> arrange(term)

expected_ses <- summary(model_expected)$coefficients[, "Std. Error"] |> unname()

# ----- Step 4: Run lm regression and compare results ----- #

model_actual <- dataduck_reg_lm(
  data = filtered_noneg_tb,
  wt_col = "PERWT",
  formula = formula
)

stopifnot(all.equal(model_actual$estimate, expected_coefs$estimate, tolerance = tol_coef))
message(glue("\u2705 Coefficients match within {tol_coef}."))

# ----- Step 5: Bootstrap SEs using modified replicate weights ----- #

input_bootstrap <- bootstrap_replicates(
  data = filtered_noneg_tb,
  f = dataduck_reg_lm,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", 1:80),
  id_cols = "term",
  formula = formula,
  verbose = TRUE
)

model_output <- se_from_bootstrap(
  bootstrap = input_bootstrap,
  constant = 4 / 80,
  se_cols = c("estimate")
)

actual_ses <- model_output$se_estimate

all_equal_helper <- function(x, y, tol) {
  if (length(x) != length(y)) return(FALSE)
  all(abs(sort(x) - sort(y)) < tol)
}

stopifnot(all_equal_helper(actual_ses, expected_ses, tol = tol_se))
message(glue("\u2705 Standard errors approximately match within {tol_se}."))
