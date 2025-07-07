# kob/scripts/kob-prepare-data.R
# The purpose of this script is to use outputs of regression to prepare data for input into 
# the kob pipeline in kob-function.R
# It standardizes regression results and population proportinos into a coef data
# frame

# ----- Step 0: Config & source helper functions ----- #
library(purrr)
library(dplyr)

devtools::load_all("../dataduck")

# ----- Step 1: Define throughput file paths and read data ----- #
props_2000_path <- "kob/throughput/props00_2000.rds"
props_2019_path <- "kob/throughput/props00_2019.rds"
coefs_2000_numprec_path <- "kob/throughput/model00_2000_numprec_summary-v2.rds"
coefs_2019_numprec_path <- "kob/throughput/model00_2019_numprec_summary-beta.rds"
coefs_2000_ppr_path <- "kob/throughput/model00_2000_persons_per_room_summary.rds"
coefs_2019_ppr_path <- "kob/throughput/model00_2019_persons_per_room_summary.rds"
coefs_2000_ppbr_path <- "kob/throughput/model00_2000_persons_per_bedroom_summary.rds"
coefs_2019_ppbr_path <- "kob/throughput/model00_2019_persons_per_bedroom_summary.rds"
coefs_2000_room_path <- "kob/throughput/model00_2000_room_summary.rds"
coefs_2019_room_path <- "kob/throughput/model00_2019_room_summary.rds"
coefs_2000_bedroom_path <- "kob/throughput/model00_2000_bedroom_summary.rds"
coefs_2019_bedroom_path <- "kob/throughput/model00_2019_bedroom_summary.rds"

# ----- Step 2: Read in proportion data ----- #
# Read proportions in as a svystat object
props_2000_svystat <- readRDS(props_2000_path)
props_2019_svystat <- readRDS(props_2019_path)

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

prop_2000 <- purrr::map_dfr(props_2000_svystat, extract_prop, year = 2000)
prop_2019 <-purrr::map_dfr(props_2019_svystat, extract_prop, year = 2019)

# ----- Step 3: Read in NUMPREC coefficients ----- #
# The two objects get read in slightly differently, because the estimation strategy
# differed.
coefs_2000_numprec <- readRDS(coefs_2000_numprec_path) |>
  select(term, estimate, std.error) |>
  rename(
    coef_2000 = estimate,
    coef_2000_se = std.error
  )

coefs_2019_numprec <- readRDS(coefs_2019_numprec_path) |>
  rename(
    coef_2019 = estimate,
    coef_2019_se = se_estimate
  )

# ----- Step 4: Read in room coefficients ----- #
coefs_2000_room <- readRDS(coefs_2000_room_path) |>
  select(term, estimate, std.error) |>
  rename(
    coef_2000 = estimate,
    coef_2000_se = std.error
  )

coefs_2019_room <- readRDS(coefs_2019_room_path) |>
  rename(
    coef_2019 = estimate,
    coef_2019_se = se_estimate
  )

# ----- Step 5: Read in bedroom coefficients ----- #
coefs_2000_bedroom <- readRDS(coefs_2000_bedroom_path) |>
  select(term, estimate, std.error) |>
  rename(
    coef_2000 = estimate,
    coef_2000_se = std.error
  )

coefs_2019_bedroom <- readRDS(coefs_2019_bedroom_path) # TODO: compare with v2 when it comes in to confirm identical


# ----- Step 6: REad in persons per room coefficients ----- #
coefs_2000_ppr <- readRDS(coefs_2000_ppr_path) |>
  select(term, estimate, std.error) |>
  rename(
    coef_2000 = estimate,
    coef_2000_se = std.error
  )

coefs_2019_ppr <- readRDS(coefs_2019_ppr_path) |>
  rename(
    coef_2019 = estimate,
    coef_2019_se = se_estimate
  )

# ----- Step 7: Read in persons per bedroom coefficients ----- #
coefs_2000_ppbr <- readRDS(coefs_2000_ppbr_path) |>
  select(term, estimate, std.error) |>
  rename(
    coef_2000 = estimate,
    coef_2000_se = std.error
  )

coefs_2019_ppbr <- readRDS(coefs_2019_ppbr_path)
