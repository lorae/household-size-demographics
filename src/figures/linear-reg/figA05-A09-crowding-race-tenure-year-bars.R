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
source("src/utils/aggregation-tools.R")  # tabulate_summary()
source("src/utils/plotting-tools.R")     # plot_year_subgroup_bars()

# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

combos <- expand.grid(
  tenure = c("renter", "homeowner"),
  year   = c(2000, 2019),
  stringsAsFactors = FALSE
)

# Common bar styling
bar_fills <- list(
  per1 = list(color = "skyblue",     alpha = 0.4, line_type = "dashed"), # 2000
  per2 = list(color = "forestgreen", alpha = 0.5, line_type = "solid")   # 2019
)

# =======================
# A05 (persons per bedroom)
# =======================
ymin_A05 <- 0
ymax_A05 <- 3
ytitle_A05 <- "Persons per Bedroom"

figA05_data <- pmap_dfr(combos, function(tenure, year) {
  tabulate_summary(
    data     = ipums_db |> dplyr::filter(tenure == !!tenure),
    value    = "persons_per_bedroom",
    year     = year,
    group_by = "RACE_ETH_bucket"
  ) |> dplyr::mutate(year = year, tenure = tenure)
})

figA05_renter <- plot_year_subgroup_bars(
  figA05_data |> dplyr::filter(tenure == "renter"),
  yvar = ppbedroom,
  bar_fills = bar_fills,
  ymin = ymin_A05, ymax = ymax_A05,
  ytitle = ytitle_A05,
  legend = FALSE,
  title = "Renter-Occupied"
)

figA05_homeowner <- plot_year_subgroup_bars(
  figA05_data |> dplyr::filter(tenure == "homeowner"),
  yvar = ppbedroom,
  bar_fills = bar_fills,
  ymin = ymin_A05, ymax = ymax_A05,
  ytitle = ytitle_A05,
  legend = TRUE,
  title = "Owner-Occupied"
)

figA05 <- figA05_renter / figA05_homeowner + plot_layout(ncol = 1)

# ===================
# A09 (persons per room)
# ===================
ymin_A09 <- 0
ymax_A09 <- 1.6
ytitle_A09 <- "Persons per Room"

figA09_data <- pmap_dfr(combos, function(tenure, year) {
  tabulate_summary(
    data     = ipums_db |> dplyr::filter(tenure == !!tenure),
    value    = "persons_per_room",
    year     = year,
    group_by = "RACE_ETH_bucket"
  ) |> dplyr::mutate(year = year, tenure = tenure)
})

figA09_renter <- plot_year_subgroup_bars(
  figA09_data |> dplyr::filter(tenure == "renter"),
  yvar = pproom,
  bar_fills = bar_fills,
  ymin = ymin_A09, ymax = ymax_A09,
  ytitle = ytitle_A09,
  legend = FALSE,
  title = "Renter-Occupied"
)

figA09_homeowner <- plot_year_subgroup_bars(
  figA09_data |> dplyr::filter(tenure == "homeowner"),
  yvar = pproom,
  bar_fills = bar_fills,
  ymin = ymin_A09, ymax = ymax_A09,
  ytitle = ytitle_A09,
  legend = TRUE,
  title = "Owner-Occupied"
)

figA09 <- figA09_renter / figA09_homeowner + plot_layout(ncol = 1)

# ----- Step 3: Save plots ----- #
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
