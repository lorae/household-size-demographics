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
