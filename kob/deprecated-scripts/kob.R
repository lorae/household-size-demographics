# The purpose of this script is to manually produce a kob analysis that exactly
# matches the results of the oaxaca function. Over time this script will be gradually
# refactored and functionalized until it is a fully validated working function that
# can be applied to the outputs of the coefXX_YYYY_*.R and propsXX_YYYY.R scripts.
# Everything aside from SEs will be validated.

# ----- STEP 0: Configuration ----- #

library(tibble)
library(dplyr)
library(oaxaca)
library(purrr)
library(tidyr)
library(stringr)

devtools::load_all("../dataduck")

# Load synthetic data
load_path <- "kob/synthetic-data"
c_only <- readRDS(paste0(load_path, "/c-only.rds"))
e_only <- readRDS(paste0(load_path, "/e-only.rds"))
u_only <- readRDS(paste0(load_path, "/u-only.rds"))

# TODO: if needed, move this function elsewhere to declutter the script
# Function for one hot encoding `varlist`categorical variables in `data`
one_hot_encode <- function(data, varlist, prefixes) {
  stopifnot(length(varlist) == length(prefixes))  # sanity check
  
  data <- data |> mutate(row_id = row_number())
  
  encoded_list <- map2(varlist, prefixes, function(var, prefix) {
    data |>
      mutate({{ var }} := as.factor(.data[[var]])) |>
      mutate(dummy = 1L) |>
      pivot_wider(
        id_cols = row_id,
        names_from = all_of(var),
        values_from = dummy,
        values_fill = list(dummy = 0L),
        names_prefix = paste0(prefix, ".")
      )
  })
  
  encoded_data <- reduce(encoded_list, full_join, by = "row_id")
  full_data <- left_join(data, encoded_data, by = "row_id")
  
  return(full_data)
}


# ----- STEP 1: Oaxaca the c_only data ----- #

c_only_binary <- 
  one_hot_encode(c_only, 
                 varlist = c("year", "HHINCOME_bucket", "EDUC_bucket"),
                 prefixes = c("year", "HHINCOME", "EDUC"))

c_only_oaxaca <- oaxaca(
  formula = NUMPREC ~ 
    EDUC.hs + EDUC.some_college + EDUC.college_4yr_plus + 
    HHINCOME.from_10k_to_100k + HHINCOME.greater_than_100k |
    year.2000,
  data = c_only_binary,
  R = NULL # no bootstrapped SEs
)

c_only_oaxaca$threefold$overall # This is what we're trying to match
plot(c_only_oaxaca, components = c("endowments","coefficients", "interaction"))

# ----- STEP 2: Manually KOB the c_only data
c_only <- c_only |>
  mutate(
    EDUC_bucket = factor(EDUC_bucket, ordered = FALSE),
    HHINCOME_bucket = factor(HHINCOME_bucket, ordered = FALSE)
  )

# Compute linear regression results for 2000 and 2019
model_2000 <- lm(data = c_only |> filter(year == 2000),
                 weights = PERWT,
                 formula = NUMPREC ~ EDUC_bucket + HHINCOME_bucket)

model_2019 <- lm(data = c_only |> filter(year == 2019),
                 weights = PERWT,
                 formula = NUMPREC ~ EDUC_bucket + HHINCOME_bucket)

coef_df <- full_join(
  enframe(model_2000$coefficients, name = "name", value = "mean_2000"),
  enframe(model_2019$coefficients, name = "name", value = "mean_2019"),
  by = "name"
)

# Known varnames (exact strings)
known_varnames <- c(
  "RACE_ETH_bucket", "AGE_bucket", "sex", "us_born",
  "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "tenure",
  "HHINCOME_bucket"
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

# Compute weighted counts for 2000 and 2019
# simulate what I'll get from propsXX_YYYY.R. Why not actually run this script
# on the output of that, insted of make a "fake" output? I want to test this as
# a proof-of-concept. Once I show it works, I'll incorporate in the actual props 
# and coef code.
get_prop_table <- function(year) {
  hhinc_tbl <- 
    crosstab_percent(
      data = c_only |> filter(year == year),
      wt_col = "PERWT",
      group_by = c("HHINCOME_bucket"),
      percent_group_by = c(),
      every_combo = TRUE
    ) |>
    rename(value = HHINCOME_bucket) |>
    mutate(varname = "HHINCOME_bucket")

  educ_tbl <- 
    crosstab_percent(
      data = c_only |> filter(year == year),
      wt_col = "PERWT",
      group_by = c("EDUC_bucket"),
      percent_group_by = c(),
      every_combo = TRUE
    ) |>
      rename(value = EDUC_bucket) |>
      mutate(varname = "EDUC_bucket")
  
  output <- bind_rows(hhinc_tbl, educ_tbl) |>
    mutate(prop = percent/100) |>
    select(-percent, -weighted_count, -count) |>
    rename(!!paste0("prop_", year) := prop)
  
  return(output)
}

prop_2000 <- get_prop_table(2000)
prop_2019 <- get_prop_table(2019)

### Now we combine the prop tables with the coef table to get a combined, ready for
# KOB, table
kob <-
  left_join(coef_df, prop_2000, by = c("varname", "value")) %>%
  left_join(., prop_2019, by = c("varname", "value"))
kob_analysis

## Now let's do the KOB
kob <- kob |>
  mutate(
    u = case_when(
      name == "(Intercept)" ~ mean_2019 - mean_2000,
      TRUE ~ NA_real_
    ),
    c = prop_2000*(mean_2019 - mean_2000),
    e = mean_2019*(prop_2019 - prop_2000)
  )

u = sum(kob$u, na.rm = TRUE)
c = sum(kob$c, na.rm = TRUE)
e = sum(kob$e, na.rm = TRUE)
u + c + e

# Does it match the chagne in hh size?
c_only_oaxaca$y

c_only_oaxaca$threefold$overall

