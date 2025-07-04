# kob/refactor/benchmark/subset-domain-estimation-se.R
#
# The purpose of this file is to test whether prematurely filtering the data can 
# distort standard errors in a Taylor series approximated SE calculation. Particularly,
# the issue of "domain estimation" has potential issues:
# 
# "Because the total size of the subpopulation is unknown, it is more complicated 
# to estimate the standard error of a domain mean than a full population mean.  
# Simply restricting the dataset to the subpopulation of interest and then invoking 
# a standard error formula is not correct, even if weights are properly included 
# in the analysis.  This is due to the fact that some clusters may contain no observations 
# from the domain of interest; ignoring such clusters generally yields an underestimate 
# of the standard error (Lohr 1999). "
# https://usa.ipums.org/usa/resources/complex_survey_vars/UserNote_Variance.pdf
#
# I show in this script that premature subsetting has no effect on the coefficient
# estimate (in this case, proportion of the population falling within a certain
# racial-ethnic subgroup in 2000), but it does (slightly) affect standard errors.
# 
# As such, the results of this script guide design decisions in this project to
# use the `subset` function from the survey package in all calculations, whether
# Taylor series approximated (as in the 2000 data) or approximated using successive 
# differences recplication (as in the 2019 data).

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

# --- Step 1A: collect data from randomly sampled strata
# Rather than use the entire dataset, I filter to just 10 strata, to speed up 
# computation.

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
