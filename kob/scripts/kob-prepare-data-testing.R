# # NOTE: this script was deprecated on 7/7/2025
# kob/scripts/kob-prepare-data-testing
# The purpose of this script is to use outputs of regression to prepare data for input into 
# the kob pipeline in kob-function.R
# This script has been renamed to kob-prepare-data-testing because it contains useful
# code that can be used to eventually write a unit test.

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("kob/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #
# In this refactor, I replace the entire data set with smaller benchmark datasets,
# so that the code runs more quickly locally.

# Load the script for creating benchmark samples
source("kob/benchmark/create-benchmark-data.R")

# Ensure the benchmarks exist
create_benchmark_sample(
  year = 2000,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

create_benchmark_sample(
  year = 2019,
  n_strata = 3,
  db_path = "data/db/ipums.duckdb",
  db_table_name = "ipums_processed",
  force = FALSE
)

# Read in the benchmarks
# TODO: make this step happen when create_benchmark_smaple is run. Perhaps rename
# to load_benchmark_sample. Also, add in a message that says where the path is
# to the file that was just loaded.
ipums_2000 <- readRDS("kob/cache/benchmark_sample_2000_3/tb.rds")
ipums_2019 <- readRDS("kob/cache/benchmark_sample_2019_3/tb.rds")

ipums_tb <- bind_rows(ipums_2000, ipums_2019)

ipums_tb <- ipums_tb |>
  mutate(
    tenure = ifelse(OWNERSHP == 1, "homeowner", "renter"),
    sex = ifelse(SEX == 1, "male", "female")
  )

# ipums_db <- ipums_db |>
#   mutate(
#     tenure = ifelse(OWNERSHP == 1, as.character("homeowner"), as.character("renter")),
#     sex = ifelse(SEX == 1, as.character("male"), as.character("female"))
#   )
# ipums_db <- ipums_db |>
#   mutate(
#     tenure = sql("CASE WHEN OWNERSHP = 1 THEN 'homeowner' ELSE 'renter' END"),
#     sex = sql("CASE WHEN SEX = 1 THEN 'male' ELSE 'female' END")
#   )



# These two models take about a two minutes to compute. No CPUMAs since they are difficult
# to handle. 8 Gb, whoa!
model_2000 <- lm(data = ipums_tb |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket*us_born + AGE_bucket + sex  +
                   EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

model_2019 <- lm(data = ipums_tb |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
                 weights = PERWT,
                 formula = NUMPREC ~ RACE_ETH_bucket*us_born + AGE_bucket + sex  +
                   EDUC_bucket + INCTOT_cpiu_2010_bucket + tenure
)

coef_df <- full_join(
  enframe(model_2000$coefficients, name = "name", value = "mean_2000"),
  enframe(model_2019$coefficients, name = "name", value = "mean_2019"),
  by = "name"
)

# Known varnames (exact strings)
known_varnames <- c(
  "RACE_ETH_bucket", "AGE_bucket", "sex", "us_born",
  "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "tenure"
)

intercept_row <- coef_df |>
  filter(name %in% c(
    "(Intercept)",
    "RACE_ETH_bucketAIAN:us_bornTRUE",
    "RACE_ETH_bucketBlack:us_bornTRUE",
    "RACE_ETH_bucketHispanic:us_bornTRUE",
    "RACE_ETH_bucketMultiracial:us_bornTRUE",
    "RACE_ETH_bucketOther:us_bornTRUE",
    "RACE_ETH_bucketWhite:us_bornTRUE"
  )) |>
  mutate(varname = NA_character_, value = NA_character_)

non_intercepts <- coef_df |>
  filter(!name %in% c(
    "(Intercept)",
    "RACE_ETH_bucketAIAN:us_bornTRUE",
    "RACE_ETH_bucketBlack:us_bornTRUE",
    "RACE_ETH_bucketHispanic:us_bornTRUE",
    "RACE_ETH_bucketMultiracial:us_bornTRUE",
    "RACE_ETH_bucketOther:us_bornTRUE",
    "RACE_ETH_bucketWhite:us_bornTRUE"
  )) |>
  mutate(
    varname = map_chr(name, function(nm) {
      matched <- keep(known_varnames, function(vn) str_starts(nm, vn))
      if (length(matched) != 1) stop(paste("Could not uniquely match varname for:", nm))
      matched
    }),
    value = str_remove(name, varname)
  )

coef_df <- bind_rows(intercept_row, non_intercepts)



get_weighted_count <- function(varname, value, year, name = NULL) {
  # Return NA for intercept or missing input
  if (!is.null(name) && name == "(Intercept)") {
    return(NA_real_)
  }
  
  # Manual exceptions for RACE_ETH_bucket:us_bornTRUE interaction terms
  if (!is.null(name)) {
    if (name == "RACE_ETH_bucketAIAN:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "AIAN", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketBlack:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Black", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketHispanic:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Hispanic", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketMultiracial:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Multiracial", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketOther:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "Other", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    } else if (name == "RACE_ETH_bucketWhite:us_bornTRUE") {
      return(ipums_tb |>
               filter(YEAR == !!year, GQ %in% c(0, 1, 2), RACE_ETH_bucket == "White", us_born == TRUE) |>
               summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
               collect() |>
               pull(weighted_count))
    }
  }
  
  # Coerce specific label strings to their underlying codes
  if (value == "male" && varname == "sex") {
    value <- 1
    varname <- "SEX"
  } else if (value == "homeowner" && varname == "tenure") {
    value <- 1
    varname <- "OWNERSHP"
  }
  
  # Default case
  ipums_tb |>
    filter(YEAR == !!year, GQ %in% c(0, 1, 2)) |>
    filter(!!sym(varname) == !!value) |>
    summarise(weighted_count = sum(PERWT), na.rm = TRUE) |>
    collect() |>
    pull(weighted_count)
}


coef <- coef_df |>
  mutate(
    weighted_count_2000 = pmap_dbl(
      list(varname = varname, value = value, name = name),
      ~ get_weighted_count(..1, ..2, 2000, ..3)
    ),
    weighted_count_2019 = pmap_dbl(
      list(varname = varname, value = value, name = name),
      ~ get_weighted_count(..1, ..2, 2019, ..3)
    )
  )


pop_2000 <- ipums_tb |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)

pop_2019 <- ipums_tb |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |> 
  summarize(weighted_count = sum(PERWT), na.rm = TRUE) |>
  collect() |>
  pull(weighted_count)

coef <- coef |>
  mutate(
    prop_2000 = weighted_count_2000 / pop_2000,
    prop_2019 = weighted_count_2019 / pop_2019
  )