# benchmark/reduce-col-count.R
#
# The purpose of this file is to test the extent to which paring away unneeded
# columns for a survey design estimation can increase speed.
#
# TODO: I can easily eliminate REPWTX columns from the 2000 sample to make things run
# a little faster. That will be a 10% performance boost.

# ----- STEP 0: Config ----- #

library(srvyr)
library(survey)
library(tictoc)
library(testthat)
library(duckdb)
library(dplyr)
library(ggplot2)

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Pseudorandom seed
set.seed(123)

# ----- BENCHMARK 3: reduce cols and see effect on performance ----- #
# I randomly select 20 strata. 

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

sampled_strata_vec <- sampled_strata$STRATA

# Now filter
ipums_2000_tb <- ipums_db |>
  filter(YEAR == 2000, STRATA %in% sampled_strata_vec) |>
  collect()

# --- Step 3b: Design survey with and without unneeded cols
# Large
ncol(ipums_2000_tb)
tic("Design large")
design_2000_survey_large <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_tb,
  nest = TRUE
) |>
  subset(GQ %in% c(0, 1, 2))
toc()

# Small
ipums_2000_small_tb <- ipums_2000_tb |>
  select(pers_id, CLUSTER, STRATA, PERWT, RACE_ETH_bucket, GQ)
ncol(ipums_2000_small_tb)

tic("Design small")
design_2000_survey_small <- svydesign(
  ids = ~CLUSTER,
  strata = ~STRATA,
  weights = ~PERWT,
  data = ipums_2000_small_tb,
  nest = TRUE
) |>
  subset(GQ %in% c(0, 1, 2))
toc()

# --- Step 3c: Time the prop operations

tic("Large")
svymean(~RACE_ETH_bucket, design_2000_survey_large)
toc()

tic("Small")
svymean(~RACE_ETH_bucket, design_2000_survey_small)
toc()

# Reducing the number of columns yielded only small performance gains in svymean()
# estimation: about 10%. The design took about 2x as long for the large 127 column
# survey as for the small 6 column survey.

# Conclusion: reducing columns is not worth the effort, unless survey design is a
# significant hang-up in the process.
