# fine-grained-counterfactual-multiscenario.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This is the most updated version of the script, but others are currently
# being kept for reference.
# 
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
# These layered results are saved in .rda files that are placed in WHERE?
#
# Inputs:
#   - data/db/ipums.duckdb
#   - draws from function defined in src/utils/counterfactual-tools.R
# Outputs:
#   - WHERE?
# 
# TODO: step 2, 2a, and 2b are identical between this script and counterfactual-regional.R.
# Figure out how potentially to move this data wrangling upstream.
# TODO: this no longer works due to a refactored calculate counterfactual funciton. Fix.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") |>
  # add an is_hoh column that is TRUE if the person is head of household, false otherwise
  mutate(is_hoh =as.integer(PERNUM == 1))

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# ----- Step 3: Run counterfactuals ----- #
# These results are used to produce tables 3.1 and 3.2 (in tab 3 of the shiny app)
# relies on the calculate-counterfactual function, which is loaded in the 
# src/utils/counterfactual-tools.R

# List of scenarios
scenarios <- list(
  c("AGE_bucket"),
  c("SEX"),
  c("us_born"),
  c("EDUC_bucket"),
  c("INCTOT_cpiu_2010_bucket"),
  c("OWNERSHP"),
  c("CPUMA0010"),
  c("RACE_ETH_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010")
)

# Generate data for all scenarios
nrow_pull <- 10000000
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()

# Try one counterfactual
calculate_counterfactual(
  cf_categories = c("AGE_bucket"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample,
  p1_data = p1_sample,
  outcome = "is_hoh"
)$contributions -> y

# Persons per bedroom
headship_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019,
    p0_data = p0_sample,
    p1_data = p1_sample,
    outcome = "is_hoh"
    )$summary # Extract only the summary tibble
  )
)

# TODO: add a check in the function calculate_counterfactual that tests whether
# all of the potential categories are populated by data?
# motivated by my observation that smaller samples (e.g. 10,000) don't capture all
# CPUMA0010s, resulting in NA estimates.
# Persons per household
hhsize_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019, 
    p0_data = p0_sample, 
    p1_data = p1_sample,
    outcome = "NUMPREC"
    )$summary # Extract only the summary tibble
  )
)

##############
#GENERATE FIGUURE 3

# Average headship rate in 2000
headship_2000_observed <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2000, GQ %in% c(0,1,2)),
  value = "is_hoh",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE) |>
  pull(weighted_mean)

# Average headship rate in 2019
headship_2019_observed <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  value = "is_hoh",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE) |>
  pull(weighted_mean)


#----- Step 4: Save the results ----- #
# TODO: save
