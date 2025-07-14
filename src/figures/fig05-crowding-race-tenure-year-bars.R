# #src/figures/fig05-crowding-race-tenure-year-bars.R
#
# Produce bar charts showing people per bedroom (room) by tenure in two panels
# (upper and lower) as well as by race/ethnicity (groups of two bars) and year
# (paired bars)
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig05-crowding-race-tenure-year-bars.png, 
#         output/figures/fig05-appendix-crowding-race-tenure-year-bars.png
#
# TODO: write unit tests for functions

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")
library("tidyr")
library("purrr")
library("patchwork")

devtools::load_all("../dataduck")

# ----- Step 1: Define functions -----
# Define `tabulate_summary()` and `tabulate_summary_2yr()`
source("src/utils/aggregation-tools.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

combos <- expand.grid(
  tenure = c("renter", "homeowner"),
  year = c(2000, 2019),
  stringsAsFactors = FALSE
)

# Apply tabulate_summary over all combinations
fig05_data <- pmap_dfr(combos, function(tenure, year) {
  tabulate_summary(
    data = ipums_db |> filter(tenure == !!tenure),
    year = year,
    group_by = "RACE_ETH_bucket"
  ) |> 
    mutate(year = year, tenure = tenure)
})

# ----- Step 3: Make plots ----- #
# Define `plot_year_subgroup_bars()`
source("src/utils/plotting-tools.R")

# Define a single main color for all bars
main_color <- "steelblue"
ymin = 0
ymax = 5

# Generate the plot
fig05_renter <- plot_year_subgroup_bars(
  fig05_data |> filter(tenure == "renter"), 
  main_color,
  ymin, ymax,
  legend = FALSE,
  title = "Renter-Occupied"
  )
fig05_homeowner <- plot_year_subgroup_bars(
  fig05_data |> filter(tenure == "homeowner"), 
  main_color,
  ymin, ymax,
  legend = TRUE,
  "Owner-Occupied"
  )

# Combine
fig05 <- fig05_renter / fig05_homeowner +
  plot_layout(ncol = 1)

# ----- Step 4: Save plots ----- #
# ggsave(
#   "output/figures/fig01-hhsize-race-year-bars.png", 
#   plot = fig01, 
#   width = 3000, height = 2400, units = "px", dpi = 400
# )
