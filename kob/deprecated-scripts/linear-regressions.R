# temporal-kob.R
# apply learnings from test-temporal-kob.r to produce the KOB decomposition

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("tibble")
library("fixest")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

tidy_regression <- function(model, varnames = varnames_dict) {
  coefs <- as_tibble(model$coeftable, rownames = "coef") |>
    select(coef, Estimate) |>
    mutate(
      variable = map_chr(coef, ~ varnames[str_detect(.x, fixed(varnames))][1]),
      value = if_else(!is.na(variable), str_remove(coef, fixed(variable)), coef)
    )
  
  # Try to get fixed effects — return empty tibble if they don’t exist
  fes <- tryCatch({
    fixef(model)[["cpuma"]] |>
      enframe(name = "value", value = "Estimate") |>
      mutate(variable = "cpuma", coef = paste0(variable, value))
  }, error = function(e) {
    tibble(coef = character(), Estimate = numeric(), variable = character(), value = character())
  })
  
  # Combine
  out <- bind_rows(coefs, fes)
  
  # Add NA cols for all varnames
  out[varnames] <- NA_character_
  
  # Fill correct column per row
  for (i in seq_len(nrow(out))) {
    var <- out$variable[i]
    if (var %in% varnames) {
      out[[i, var]] <- out$value[i]
    }
  }
  
  return(out)
}

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# I need to "collect" the data into memory in order to use the `fixest::feols()`
# function
ipums_2000_tb <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |>
  collect() 
ipums_2019_tb <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |>
  collect() 

# ----- Step 3: Run regressions ----- #
# Note: I attach a varnames_dict item to each list comprising the regression result.
# This is metadata accessed by `tidy_weighted_means()` function in modular-kob.R


# Regression 00: Simple, no interactions, NO FEs
reg00_varnames <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure"
)

reg00_2000 <- feols(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure, 
  data =  ipums_2000_tb, 
  weights = ~PERWT
)

reg00_2019 <- feols(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure, 
  data =  ipums_2019_tb, 
  weights = ~PERWT
)

reg00 <- list(
  varnames_dict = reg00_varnames,
  reg_2000 = reg00_2000,
  reg_2019 = reg00_2019,
  desc = "Simple, no interactions, no FEs"
)

# Regression 01: Simple, no interactions, cpuma FEs
reg01_varnames <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

reg01_2000 <- feols(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure | cpuma, 
  data =  ipums_2000_tb, 
  weights = ~PERWT
  )

reg01_2019 <- feols(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure | cpuma, 
  data =  ipums_2019_tb, 
  weights = ~PERWT
)

reg01 <- list(
  varnames_dict = reg01_varnames,
  reg_2000 = reg01_2000,
  reg_2019 = reg01_2019,
  desc = "Simple, no interactions + CPUMA FEs"
)

# ----- Step 4: Save results ----- #
save(reg00, reg01, file = "kob/throughput/regression_models.RData")
