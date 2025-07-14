# #src/figures/fig01-hhsize-race--year-bars.R
#
# Produce bar charts showing hhsize by race/ethnicity and year (paired bars)
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig01-hhsize-race-year-bars.png
#
# TODO: write unit tests for functions

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")
library("tidyr")

devtools::load_all("../dataduck")

# ----- Step 1: Define functions -----
# Define `tabulate_summary()` and `tabulate_summary_2yr()`
source("src/utils/aggregation-tools.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

race_summary_2000 <- tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket") |> mutate(year = 2000)
race_summary_2019 <- tabulate_summary(data = ipums_db, year = 2019, group_by = "RACE_ETH_bucket") |> mutate(year = 2019)
race_summary <- union_all(race_summary_2000, race_summary_2019) # Row bind the tables

# ----- Step 3: Make plots ----- #
source("src/utils/plotting-tools.R")

fig01 <- plot_year_subgroup_bars(data = race_summary, main_color = "steelblue")

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/fig01-hhsize-race-year-bars.png", 
  plot = fig01, 
  width = 3000, height = 2400, units = "px", dpi = 400
)
