#
# The purpose of this script is to quantify housing shortages according to the results
# of fine-grained-counterfactual-regional

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library("base64enc")
library("sf")
options(scipen = 999)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
load("data/helpers/state-pop-growth.rda") # May be deprecated

# ----- Step 2: Import data ----- #
hhsize_cpuma_summary <- readRDS("throughput/fine-grained-hhsize-diff-cpuma.rds")
headship_cpuma_summary <- readRDS("throughput/fine-grained-headship-diff-cpuma.rds")
cf_summaries <- readRDS("throughput/fine-grained-cf-summaries.rds")

# ----- Housing shortage by CPUMA
hhsize_cpuma_summary |> nrow()
hhsize_cpuma_summary |> filter(diff > 0) |> nrow()
# hhsize: Total shortage among CPUMAs with a shortage
hhsize_cpuma_summary |> filter(diff > 0) |>
  mutate(
    housing_surplus = (pop_2019/observed_2019) - (pop_2019/expected_2019)
  ) |>
  pull(housing_surplus) |>
  sum()

# headship: Total shortage among CPUMAs with a shortage
headship_cpuma_summary |> nrow()
headship_cpuma_summary |> filter(diff < 0) |> nrow()
headship_cpuma_summary |> filter(diff < 0) |> 
  mutate(
    housing_surplus = pop_2019 * diff
    ) |> 
  pull(housing_surplus) |> 
  sum()

# ----- Housing shortage compared to whites
# Average size of a white household
white_hhsize_2019_overall <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("RACE_ETH_bucket")
) |> 
  filter(RACE_ETH_bucket == "White") |>
  pull(weighted_mean)
hhsize_cpuma <- hhsize_cpuma_summary |> 
  mutate(
    white_hhsize = white_hhsize_2019_overall,
    white_diff = observed_2019 - white_hhsize
    )
# nubmer of additional hhs needed to match white hh sizes
hhsize_cpuma |> filter(white_diff > 0) |>
  mutate(
    housing_surplus_white = (pop_2019/observed_2019) - (pop_2019/white_hhsize)
  ) |>
  pull(housing_surplus_white) |>
  sum()

# Average headship rate among whites
white_headship_2019_overall <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)),
  value = "is_hoh",
  wt_col = "PERWT",
  group_by = c("RACE_ETH_bucket")
) |> 
  filter(RACE_ETH_bucket == "White") |>
  pull(weighted_mean)
headship_cpuma <- headship_cpuma_summary |>
  mutate(
    white_headship = white_headship_2019_overall,
    white_diff = observed_2019 - white_headship
  )
# nubmer of additional hhs needed to match white headship rates
headship_cpuma |> filter(white_diff < 0) |>
  mutate(
    housing_surplus_white = pop_2019 * white_diff
  ) |>
  pull(housing_surplus_white) |>
  sum()

population_2019 <- headship_cpuma_summary |> pull(pop_2019) |> sum()

population_aggregates_2019 <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE)

# Actual overall household size in 2019 (excluding those in Group Quarters)
act_hhsize_2019_overall <- population_aggregates_2019 |>
  pull(weighted_mean)
act_hhsize_2019_overall

(population_2019/act_hhsize_2019_overall) - (population_2019/white_hhsize_2019_overall) 
