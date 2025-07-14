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

devtools::load_all("../dataduck")

# ----- Step 1: Define functions -----
# Define `tabulate_summary()` and `tabulate_summary_2yr()`
source("src/utils/aggregation-tools.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3: Make plots ----- #
race_summary_2000_renter <- tabulate_summary(
  data = ipums_db |> filter(tenure == "renter"), 
  year = 2000, 
  group_by = "RACE_ETH_bucket"
  ) |> mutate(year = 2000)
race_summary_2019_renter <- tabulate_summary(
  data = ipums_db |> filter(tenure == "renter"), 
  year = 2019, 
  group_by = "RACE_ETH_bucket"
  ) |> mutate(year = 2019)
race_summary_renter <- union_all(race_summary_2000_renter, race_summary_2019_renter) |>
  mutate(tenure = "renter")

race_summary_2000_homeowner <- tabulate_summary(
  data = ipums_db |> filter(tenure == "homeowner"), 
  year = 2000, 
  group_by = "RACE_ETH_bucket"
) |> mutate(year = 2000)
race_summary_2019_homeowner <- tabulate_summary(
  data = ipums_db |> filter(tenure == "homeowner"), 
  year = 2019, 
  group_by = "RACE_ETH_bucket"
) |> mutate(year = 2019)
race_summary_homeowner <- union_all(race_summary_2000_homeowner, race_summary_2019_homeowner) |>
  mutate(tenure = "homeowner")

# Define a single main color for all bars
main_color <- "steelblue"

# Generate the plot
# fig01 <- ggplot(race_summary, aes(x = subgroup, y = hhsize, fill = factor(year))) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
#            width = 0.8, color = "black") +  # Bar border
#   geom_text(aes(label = round(hhsize, 2), group = year), 
#             position = position_dodge(width = 0.8), 
#             vjust = -0.5, size = 3) +  
#   scale_fill_manual(
#     values = c("2000" = alpha(main_color, 0.4), "2019" = alpha(main_color, 0.8)), 
#     name = "") +  # Ensures the legend colors match bar colors
#   labs(y = "Average Household Size") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 0, hjust = 0.5),
#     legend.position = "bottom",
#     legend.box = "horizontal",
#     axis.title.x = element_blank(),
#     plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
#   )

# ----- Step 4: Save plots ----- #
# ggsave(
#   "output/figures/fig01-hhsize-race-year-bars.png", 
#   plot = fig01, 
#   width = 3000, height = 2400, units = "px", dpi = 400
# )
