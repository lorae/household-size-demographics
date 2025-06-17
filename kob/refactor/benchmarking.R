# kob/refactor/benchmarking.R

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

# ----- BENCHMARK 1: Test subsetted SEs against full-sample SEs ----- #
# Does prematurely filtering the data for domain estimation, as cautioned here - 
# https://usa.ipums.org/usa/resources/complex_survey_vars/UserNote_Variance.pdf
# affect SEs? I test using data that is pre-filtered for GQ in c(0,1,2) and data
# that is not pre-filtered.
# Rather than use the entire dataset, I filter to just 10 strata, to speed up 
# computation.

# --- Step 1A: collect data from randomly sampled strata
strata_summary <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |>
  select(STRATA, CLUSTER) |>
  distinct() |> 
  collect()

sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |>  # Ensure stratum has at least 2 PSUs (they always will)
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 10) # Predictable sample due to pseudorandom seed, set in config

# "_precut" suffix indicates I've already filtered by GQ
ipums_2000_precut_tb <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2), STRATA %in% !!sampled_strata$STRATA) |>
  collect()
# Absence of "precut" means this sample is *not* pre-filtered by GQ
ipums_2000_tb <- ipums_db |> 
  filter(YEAR == 2000, STRATA %in% !!sampled_strata$STRATA) |>
  collect()

# --- Step 1B: create survey design variables for precut and non-precut samples
# I'm going to use the survey package, because as you'll see in subsequent 
# benchmarks, it's faster.
design_2000_precut_survey <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_precut_tb,
  nest = TRUE
)

# We're going to give it the "_postcut" suffix to indicate that this sample was 
# filtered by GQ using the survey package `subset` function
design_2000_survey <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  nest = TRUE
)
design_2000_postcut_survey <- subset(design_2000_survey, GQ %in% c(0, 1, 2))
# --- Step 1C: Produce output and compare estimates and standard errors
# Estimates should definitely match: If they don't, that's an issue.
# Standard errors may or may not match. I'm not sure what to expect.
# SPOILER ALERT: They *do* *don't* match!

benchmark01_precut <- svymean(~RACE_ETH_bucket, design_2000_precut_survey)
benchmark01_postcut <- svymean(~RACE_ETH_bucket, design_2000_postcut_survey)

# Do estimates match? Yes.
all.equal(
  coef(benchmark01_precut),
  coef(benchmark01_postcut),
  tolerance = 1e-6
)

# Do SEs match? No. But the difference is small.
all.equal(
  SE(benchmark01_precut),
  SE(benchmark01_postcut),
  tolerance = 1e-6
)

# --- CONCLUSION: Survey must be subsetted after defining it, otherwise SEs will
# be incorrect. Domain estimation is a valid issue.



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
