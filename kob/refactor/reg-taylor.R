# kob/refactor/reg-taylor.R

library(srvyr)
library(survey)
library(tictoc)
library(furrr)

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# # I need to "collect" the data into memory in order to use the `fixest::feols()`
# # function
# ipums_2000_tb <- ipums_db |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |> 
#   collect()
# 
# ipums_2019_tb <- ipums_db |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)) |>
#   collect()

# Sample the data for faster computing
strata_summary <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2)) |>
  select(STRATA, CLUSTER) |>
  distinct() |> 
  collect()

# Sample 100 strata
set.seed(123)
sampled_strata <- strata_summary |> 
  group_by(STRATA) |> 
  filter(n() >= 2) |>  # Ensure stratum has at least 2 PSUs
  ungroup() |> 
  distinct(STRATA) |> 
  slice_sample(n = 10)

# Then collect all data for those strata
ipums_2000_tb <- ipums_db |> 
  filter(YEAR == 2000, GQ %in% c(0, 1, 2), STRATA %in% !!sampled_strata$STRATA) |>
  collect()

# Uses the srvyr package
tic("survey design, srvyr")
design_2000 <- as_survey_design(
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
