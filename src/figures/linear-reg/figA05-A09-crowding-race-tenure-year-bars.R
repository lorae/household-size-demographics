# #src/figures/linear-reg/figA05-A09-crowding-race-tenure-year-bars.R
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
library("scales")

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
figA05_data <- pmap_dfr(combos, function(tenure, year) {
  tabulate_summary(
    data = ipums_db |> filter(tenure == !!tenure),
    value = "persons_per_bedroom",
    year = year,
    group_by = "RACE_ETH_bucket"
  ) |> 
    mutate(year = year, tenure = tenure)
})

figA09_data <- pmap_dfr(combos, function(tenure, year) {
  tabulate_summary(
    data = ipums_db |> filter(tenure == !!tenure),
    value = "persons_per_room",
    year = year,
    group_by = "RACE_ETH_bucket"
  ) |> 
    mutate(year = year, tenure = tenure)
})

# ----- Step 3: Make plots ----- #
# Define `plot_year_subgroup_bars()`
source("src/utils/plotting-tools.R")

ymin = 0
ymax = 3

bar_fills <- list(
  per1 = list(color = "skyblue", alpha = 0.4, line_type = "dashed"), # 2000
  per2 = list(color = "forestgreen", alpha = 0.5, line_type = "solid") # 2019
)

#--- Generate the plot: Figure A05
figA05_renter <- plot_year_subgroup_bars(
  figA05_data |> dplyr::filter(tenure == "renter"),
  yvar = ppbedroom,
  bar_fills = bar_fills,
  ymin = 0, ymax = 3,
  legend = FALSE,
  title = "Renter-Occupied"
)

figA05_homeowner <- plot_year_subgroup_bars(
  figA05_data |> dplyr::filter(tenure == "homeowner"),
  yvar = ppbedroom,
  bar_fills = bar_fills,
  ymin = 0, ymax = 3,
  legend = TRUE,
  title = "Owner-Occupied"
)

# Combine
figA05 <- figA05_renter / figA05_homeowner +
  plot_layout(ncol = 1)

#--- Generate the plot: Figure A09
figA09_renter <- plot_year_subgroup_bars(
  figA09_data |> dplyr::filter(tenure == "renter"),
  yvar = pproom,
  bar_fills = bar_fills,
  ymin = 0, ymax = 1.6,
  legend = FALSE,
  title = "Renter-Occupied"
)

figA09_homeowner <- plot_year_subgroup_bars(
  figA09_data |> dplyr::filter(tenure == "homeowner"),
  yvar = pproom,
  bar_fills = bar_fills,
  ymin = 0, ymax = 1.6,
  legend = TRUE,
  title = "Owner-Occupied"
)

# Combine
figA09 <- figA09_renter / figA09_homeowner +
  plot_layout(ncol = 1)

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/linear-reg/figA05-crowding-race-tenure-year-bars-bedroom.png",
  plot = figA05,
  width = 3000, height = 4000, units = "px", dpi = 400
)
ggsave(
  "output/figures/linear-reg/figA09-crowding-race-tenure-year-bars-room.png",
  plot = figA09,
  width = 3000, height = 4000, units = "px", dpi = 400
)
