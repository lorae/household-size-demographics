# #src/figures/fig02-hhsize-race-hist.R
#
# Produce bar charts showing hhsize by race as a histogram
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig02-hhsize-race-hist.png
#
# ----- Step 0: Config ----- #
library("ggplot2")
library("duckdb")
library("dplyr")
library("tidyr")

devtools::load_all("../dataduck") # The crosstab_count() function used here is defined in dataduck

# ----- Step 1: Define functions ----- #


# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# 2019 estimate of non-group-quarters population in household of X size and Y race/ethnicity
hhsize_race_2019 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect()