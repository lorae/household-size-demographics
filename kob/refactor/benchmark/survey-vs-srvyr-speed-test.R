# kob/refactor/benchmark/survey-vs-srvyr-speed-test.R
#
# The purpose of this file is to test which of the two packages, survey or srvyr,
# is faster at calculating means and regression coefficients on the same sample
# subset of data.
#
# The package `srvyr` is, in fact, a wrapper around `survey` that incorporates 
# `dplyr`-esque language into the `survey` functions. Given that both are powered
# by the underlying code of the `survey` package, it doesn't come as a surprise
# that `survey` is faster. However, the differences are quite stark, as you'll see
# below. I test both on a subset of 2, 3, 5, 10, 15, and 20 strata and graph
# their speeds. 

# ----- STEP 0: Config ----- #

library(srvyr)
library(survey)
library(tictoc)
library(furrr)
library(testthat)

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Pseudorandom seed
set.seed(123)

# ----- BENCHMARK 2: speed-test svymean against survey_prop ----- #
# svymean is from the `survey` package. `survey_prop` is from the `srvyr` package

# --- Step 2A: collect data from randomly sampled strata
# I randomly select 20 strata. Then, of those, the first 2, then the first 3,
# 5, 10, 15, and 20 are selected for speed tests.

strata_summary <- ipums_db |> 
  filter(YEAR == 2000) |>
  select(STRATA, CLUSTER) |>
  distinct() |> 
  collect()

sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |>  # Ensure stratum has at least 2 PSUs (they always will)
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 20) # Predictable sample due to pseudorandom seed, set in config

# --- Step 2B: Define a generic speed test function
# This can be speed tested in multiple cases

speed_test <- function(n_strata) {
  # Define the sample of strata in the survey (proxy for survey size)
  strata_sample = sampled_strata$STRATA[1:n_strata]
  ipums_2000_tb <- ipums_db |> 
    filter(YEAR == 2000, STRATA %in% strata_sample) |>
    collect()
  
  # SURVEY section
  
  # SRVYR section
  
  return(
    list(
      nrow_sample = nrow(ipums_2000_tb)#,
      # survey = survey_time,
      # srvyr = srvyr_time
      )
    )
}

speed_test(5)

# "_precut" suffix indicates I've already filtered by GQ
ipums_2000_precut_tb <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2), STRATA %in% !!sampled_strata$STRATA) |>
  collect()
# Absence of "precut" means this sample is *not* pre-filtered by GQ
ipums_2000_tb <- ipums_db |> 
  filter(YEAR == 2000, STRATA %in% !!sampled_strata$STRATA) |>
  collect()



# ----- OLD: 
# TODO: REFACTOR!!!!! / organize
# Uses the srvyr package
tic("survey design, srvyr")
design_2000_precut <- as_survey_design(
  ipums_2000_tb,
  ids = CLUSTER,
  strata = STRATA,
  weights = PERWT,
  # data = ipums_2000_db, # The svyglm below doesn't work when the survey is designed with a db
  # TODO: I bet I could make an open source contribution to the package to make it
  # work
  nest = TRUE
)
toc()

# Uses the survey package
tic("survey design, survey")
design_2000_survey <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  # data = ipums_2000_db, # The svyglm below doesn't work when the survey is designed with a db
  # TODO: I bet I could make an open source contribution to the package to make it
  # work
  nest = TRUE
)
toc()

# TODO: time this 
tic("calculate props, srvyr")
test <- design_2000 |>
  group_by(RACE_ETH_bucket) |>
  summarize(proportion = survey_prop())
toc()

# TODO: time this
tic("Proportion by race/eth bucket")
result <- svymean(~RACE_ETH_bucket, design_2000)
toc()


# TODO: time this
tic("Proportion by race/eth bucket")
plan(multisession, workers = 7)  # Or whatever # cores you want

# List of race buckets
race_levels <- unique(design_2000$variables$RACE_ETH_bucket)

tic("Proportion by race/eth bucket")
# For each bucket, subset and compute proportion
results <- future_map_dfr(race_levels, function(race) {
  sub_design <- subset(design_2000, RACE_ETH_bucket == race)
  
  est <- svymean(~I(RACE_ETH_bucket == race), design_2000)
  tibble(
    RACE_ETH_bucket = race,
    proportion = coef(est)[[1]],
    proportion_se = SE(est)[[1]]
  )
})
toc()


# Collect data into memory
# ipums_2000_tb <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |> 
#   collect()
# 
# ipums_2019_tb <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |>
#   collect()
