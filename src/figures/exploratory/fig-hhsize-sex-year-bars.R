# #src/figures/exploratory/fig01-hhsize-sex-year-bars.R
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
library("duckdb")
library("dplyr")

devtools::load_all("../dataduck")

# ----- Step 1: Define functions -----
# Define `tabulate_summary()` and `tabulate_summary_2yr()`
source("src/utils/aggregation-tools.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

sex_summary_2000 <- tabulate_summary(data = ipums_db, year = 2000, group_by = "gender") |> mutate(year = 2000)
sex_summary_2019 <- tabulate_summary(data = ipums_db, year = 2019, group_by = "gender") |> mutate(year = 2019)
sex_summary <- union_all(sex_summary_2000, sex_summary_2019) # Row bind the tables

# ----- Step 3: Make plots ----- #
source("src/utils/plotting-tools.R")
bar_fills <- list(
  per1 = list(color = "skyblue", alpha = 0.4, line_type = "dashed"), # 2000
  per2 = list(color = "forestgreen", alpha = 0.5, line_type = "solid") # 2019
)

fig01 <- plot_year_subgroup_bars(
  data = sex_summary,
  yvar = hhsize,
  bar_fills = bar_fills,
  title = NULL,
  ymin = 3,
  ymax = 3.6
)
fig01

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/linear-reg/fig01-hhsize-race-year-bars.png", 
  plot = fig01, 
  width = 3000, height = 2400, units = "px", dpi = 400
)
