# linear-regression-with-se.R
# run the linear regressions 80 times to bootstrap with SEs

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("tibble")
library("fixest")
library("survey")
library("srvyr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- STEP 2: Function to get tidy regressions ----- #

tidy_regression <- function(coeflist, varnames = varnames_dict) {
  
  coefs <- as_tibble(coeflist, rownames = "coef") |>
    select(coef, value) |>
    rename(Estimate = value) |>
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

# TODO: combine the two tidy regression functions
tidy_regression_v2 <- function(table, varnames = varnames_dict) {
  out <- table |>
    mutate(
      variable = map_chr(coef, ~ varnames[str_detect(.x, fixed(varnames))][1]),
      value = if_else(!is.na(variable), str_remove(coef, fixed(variable)), coef)
    )
  
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


run_reg00_tidy <- function(data, wt_col) {
  
  varnames = c(
    "RACE_ETH_bucket",
    "AGE_bucket",
    "EDUC_bucket",
    "INCTOT_cpiu_2010_bucket",
    "us_born",
    "gender",
    "tenure",
    "cpuma"
  )
  
  # Allows negative weights
  model <- lm(
    NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
    + us_born + gender + tenure,
    data = data,
    weights = data[[wt_col]]
  )
  
  tidy_output <- tidy_regression_v2(model, varnames)
    
  return(tidy_output)

}

# ----- STEP 3: Extract the data into memory ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# I need to "collect" the data into memory in order to use the `fixest::feols()`
# function
ipums_2000_tb <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |>
  collect() 
ipums_2019_tb <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |>
  collect() 

# I suppose I can also try not collecting it
ipums_2000_db <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2))
ipums_2019_db <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2))

# ----- STEP 4: Run the regression with SEs using the REPWT cols in 2019 ----- #
design_2019 <- as_survey_rep(
  ipums_2019_tb,
  weight = PERWT,
  repweights = matches("^REPWTP[0-9]+$"),
  type = "ACS",  # equivalent to Fay's method with rho = 0.5
  mse = TRUE     # ensures SEs are computed using mean squared errors
)

# Takes 10-20 minutes
# TODO: test to see if this matches my "handmade" regression code
reg00_2019_results_se <- svyglm(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure,
  design = design)

varnames_dict <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

# Prettify the output
reg00_2019_results <- summary(reg00_2019_results_se)$coefficients |> 
  as_tibble(rownames = "coef") |>
  select(coef, value_2019 = Estimate, value_2019_se = `Std. Error`) |>
  tidy_regression_v2()

# Save it! 
save(reg00_2019_results, file = "kob/cache/reg00_2019_results.rda")

# ----- STEP 5: Run the regression with STRATA and CLUSTER adjusted results in 2000----- #
# Takes about 3 minutes to run
design_2000 <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  # data = ipums_2000_db, # The svyglm below doesn't work when the survey is designed with a db
  # TODO: I bet I could make an open source contribution to the package to make it
  # work
  nest = TRUE
)

# Another 20 minutes
reg00_2000_results_se <- svyglm(
  NUMPREC ~ RACE_ETH_bucket + AGE_bucket + EDUC_bucket + INCTOT_cpiu_2010_bucket 
  + us_born + gender + tenure, 
  design = design_2000)

varnames_dict <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

# Tabulate the coefficients
coefs <- coef(reg00_2000_results_se) |>
  tidy_regression() |>
  rename(est_2000 = Estimate)

# Tabulate the SEs
ses <- sqrt(diag(vcov(reg00_2000_results_se))) |> 
  tidy_regression() |>
  rename(est_2000_se = Estimate) |>
  select(coef, est_2000_se) # keep only the novel information

# Merge together
reg00_2000_results <- left_join(coefs, ses, by = "coef") |>
  select(coef, est_2000, est_2000_se, everything())

# Save it! 
save(reg00_2000_results, file = "kob/cache/reg00_2000_results.rda")



