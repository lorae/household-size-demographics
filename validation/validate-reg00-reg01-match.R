# validation/validate-reg00-reg01-match.R
#
# The purpose of this script is to validate that coefficients from reg00 and reg01
# match after accounting for intercept values

library(tibble)
library(purrr)
library(dplyr)

# Helper functions ----

# Extract proportion from table
extract_prop <- function(svystat_obj, year) {
  # Calculate population proportions, SE on those estimates, and extract the variable
  # "value" (varname concatenated with value of variable, like "AGE_bucket0-4")
  prop <- as.numeric(svystat_obj)
  se <- sqrt(diag(attr(svystat_obj, "var")))
  term <- names(svystat_obj)
  
  # turn output into tibble and name cols using the year
  tibble::tibble(
    term = term,
    !!paste0("prop_", year) := prop,
    !!paste0("prop_", year, "_se") := se
  )
}

# Read in coefficient data from 2000
read_coefs_2000 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    select(term, estimate, std.error) |>
    rename(
      coef_2000 = estimate,
      coef_2000_se = std.error
    )

  return(output)
}

# Read in coefficient data from 2019
read_coefs_2019 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    rename(
      coef_2019 = estimate,
      coef_2019_se = se_estimate
    )

  return(output)
}

# Props 2000 ----
cat("Comparing props 2000:\n")
prop_2000_00_raw <- readRDS("throughput/reg00/props00_2000.rds")
prop_2000_00 <- purrr::map_dfr(prop_2000_00_raw, extract_prop, year = 2000)
prop_2000_01_raw <- readRDS("throughput/reg01/2000_prop.rds")
prop_2000_01 <- purrr::map_dfr(prop_2000_01_raw, extract_prop, year = 2000)
cat("Result:", all.equal(prop_2000_01, prop_2000_00, tolerance = 1e-6), "\n\n")

# Props 2019 ----
cat("Comparing props 2019:\n")
prop_2019_00_raw <- readRDS("throughput/reg00/props00_2019.rds")
prop_2019_00 <- purrr::map_dfr(prop_2019_00_raw, extract_prop, year = 2019)
prop_2019_01_raw <- readRDS("throughput/reg01/2019_prop.rds")
prop_2019_01 <- purrr::map_dfr(prop_2019_01_raw, extract_prop, year = 2019)
cat("Result:", all.equal(prop_2019_01, prop_2019_00, tolerance = 1e-6), "\n\n")

# Bedroom 2000 ----
cat("Comparing bedroom 2000:\n")
bedroom_2000_00 <- read_coefs_2000("throughput/reg00/model00_2000_bedroom_summary.rds")
bedroom_2000_01 <- read_coefs_2000("throughput/reg01/2000_b.rds")
bedroom_2000_00_adj <- bedroom_2000_00 |>
  split_term_column() |>
  add_intercept_v2(
    variable = "RACE_ETH_bucket",
    reference_value = "AAPI",
    coef_col = "coef_2000",
    se_col = "coef_2000_se"
  ) |>
  arrange(term)
bedroom_2000_01_adj <- bedroom_2000_01 |>
  split_term_column() |>
  arrange(term)

bedroom_2000_00_adj |> filter(variable == "RACE_ETH_bucket" | variable == "(Intercept)") 
bedroom_2000_01_adj |> filter(variable == "RACE_ETH_bucket" | variable == "(Intercept)") 

cat("Result:", all.equal(bedroom_2000_01_adj, bedroom_2000_00_adj, tolerance = 1e-6), "\n\n")

# Bedroom 2019 ----
cat("Comparing bedroom 2019:\n")
bedroom_2019_00 <- read_coefs_2019("throughput/reg00/model00_2019_bedroom_summary-v5.rds")
bedroom_2019_01 <- read_coefs_2019("throughput/reg01/2019_b.rds")
bedroom_2019_00_adj <- bedroom_2019_00 |>
  split_term_column() |>
  add_intercept_v2(
    variable = "RACE_ETH_bucket",
    reference_value = "AAPI",
    coef_col = "coef_2019",
    se_col = "coef_2019_se"
  ) |>
  arrange(term)
bedroom_2019_01_adj <- bedroom_2019_01 |>
  split_term_column() |>
  arrange(term)

bedroom_2019_00_adj |> filter(variable == "RACE_ETH_bucket" | variable == "(Intercept)") 
bedroom_2019_01_adj |> filter(variable == "RACE_ETH_bucket" | variable == "(Intercept)") 

cat("Result:", all.equal(bedroom_2019_01_adj, bedroom_2019_00_adj, tolerance = 1e-6), "\n\n")

