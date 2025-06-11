# ----- STEP 0: Configuration ----- #

library(tibble)
library(dplyr)
library(oaxaca)
library(purrr)
library(tidyr)
library(stringr)

# Load synthetic data
load_path <- "kob/synthetic-data"
c_only <- readRDS(paste0(load_path, "/c-only.rds"))
e_only <- readRDS(paste0(load_path, "/e-only.rds"))
u_only <- readRDS(paste0(load_path, "/u-only.rds"))

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
                 formula = NUMPREC ~ -1 + EDUC_bucket + -1 + HHINCOME_bucket)

model_2019 <- lm(data = c_only |> filter(year == 2019),
                 weights = PERWT,
                 formula = NUMPREC ~ -1 + EDUC_bucket + -1 + HHINCOME_bucket)

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
weighted_2000 <- c_only |> filter(year == 2000) |>
  group_by(EDUC_bucket, HHINCOME_bucket) %>%
  summarise(weighted_count_2000 = sum(PERWT), .groups = "drop")

weighted_2019 <- c_only |> filter(year == 2019) %>%
  group_by(EDUC_bucket, HHINCOME_bucket) %>%
  summarise(weighted_count_2019 = sum(PERWT), .groups = "drop")