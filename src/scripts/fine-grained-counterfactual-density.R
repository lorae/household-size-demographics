# counterfactual-density.R
#
# This script produces two large summary data frames. Each one has one row per
# unique combination of AGE_bucket, SEX, and RACE_ETH_bucket. It then includes
# data for the average household size, the number of person-level observations
# and weighted person-level observations producing that metric, and the standard
# error of the household size mean.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# Create a table suitable for regression
ipums_regready <- ipums_db

ipums_regready_2005 <- ipums_db |>
  mutate(
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ as.character(SEX) # Keep original value if not 1 or 2
    )
  ) |>
  filter(YEAR == 2005)

# Create a table suitable for regression
ipums_regready_2022 <- ipums_db |>
  mutate(
    SEX = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ as.character(SEX) # Keep original value if not 1 or 2
    )
  ) |>
  filter(YEAR == 2022)

# # ----- Step 3: (Proof-of-Concept) Demonstrate regression coefficients equal binned averages ----- #
# model <- lm(
#   NUMPREC ~ 0 + SEX:RACE_ETH_bucket, 
#   weights = PERWT,
#   data = ipums_regready_2005)
# 
# hhsize2005 <- estimate_with_bootstrap_se(
#   data = ipums_regready_2005,
#   f = crosstab_mean,
#   value = "NUMPREC",
#   wt_col = "PERWT",
#   group_by = c("SEX", "RACE_ETH_bucket"),
#   id_cols = c("SEX", "RACE_ETH_bucket"),
#   repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
#   constant = 4/80,
#   se_cols = c("weighted_mean"),
#   every_combo = TRUE
# )  
# 
# # Look! The two models match!

# ---- Step 4: Using SEX only, create counterfactual 2022 household count ----- #
# Produces a table of average household size in 2005 with standard errors
hhsize05 <- estimate_with_bootstrap_se(
  data = ipums_regready_2005,
  f = crosstab_mean,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("SEX"),
  id_cols = c("SEX"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_mean", "weighted_count"),
  every_combo = TRUE
) |>
  select(-count) |>
  rename(
    weighted_mean_05 = weighted_mean,
    se_weighted_mean_05 = se_weighted_mean,
    weighted_count_05 = weighted_count,
    se_weighted_count_05 = se_weighted_count
  )

# Produces a table of average household size in 2022 with standard errors
# (We don't actually need this for the counterfactual, but it's helpful context)
hhsize05 <- estimate_with_bootstrap_se(
  data = ipums_regready_2022,
  f = crosstab_mean,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("SEX"),
  id_cols = c("SEX"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_mean", "weighted_count"),
  every_combo = TRUE
) |>
  select(-count) |>
  rename(
    weighted_mean_05 = weighted_mean,
    se_weighted_mean_05 = se_weighted_mean,
    weighted_count_05 = weighted_count,
    se_weighted_count_05 = se_weighted_count
  )


# Produces a table of number of individuals in 2022 with standard errors
percount22 <- estimate_with_bootstrap_se(
  data = ipums_regready_2022,
  f = crosstab_count,
  wt_col = "PERWT",
  group_by = c("SEX"),
  id_cols = c("SEX"),
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_count"),
  every_combo = TRUE
)  |>
  select(-count) |>
  rename(
    weighted_percount_22 = weighted_count,
    se_weighted_percount_22 = se_weighted_count
  )

# Produces a table of number of households in 2022 with standard errors
hhcount22 <- estimate_with_bootstrap_se(
  data = ipums_regready_2022 |> filter(PERNUM == 1),
  f = crosstab_count,
  wt_col = "HHWT",
  group_by = c("SEX"),
  id_cols = c("SEX"),
  repwt_cols = paste0("REPWT", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("weighted_count"),
  every_combo = TRUE
)  |>
  select(-count) |>
  rename(
    weighted_hhcount_22 = weighted_count,
    se_weighted_hhcount_22 = se_weighted_count
  )

# Merge the three data frames
cf_data <- hhsize05 |>
  left_join(percount22, by = "SEX") |>
  left_join(hhcount22, by = "SEX")

# Save to illustrate in shiny app
sex_2005_2022 <- cf_data |>
  select(SEX, weighted_mean_05, weighted_percount_22, weighted_hhcount_22) |>
  rename(
    sex = SEX,
    mean_hhsize_2005 = weighted_mean_05,
    pop_2022 = weighted_percount_22,
    hh_2022 = weighted_hhcount_22
  )
save(sex_2005_2022, file = "shiny-app/data/sex_2005_2022.rda")

# ----- Step X: Clean up ----- #
DBI::dbDisconnect(con)
