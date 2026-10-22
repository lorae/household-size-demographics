# #src/figures/linear-reg/fig01-hhsize-race-year-bars-slides.R
#
# Slide version of fig01: hhsize by race/ethnicity and year (paired bars),
# with legend on the right, horizontal y-axis title, larger text, wider/shorter canvas
#
# Input: data/db/ipums.duckdb
# Output: output/figures/linear-reg/fig01-hhsize-race-year-bars-slides.png

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

race_summary_2000 <- tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket") |> mutate(year = 2000)
race_summary_2019 <- tabulate_summary(data = ipums_db, year = 2019, group_by = "RACE_ETH_bucket") |> mutate(year = 2019)
race_summary <- union_all(race_summary_2000, race_summary_2019) # Row bind the tables

# ----- Step 3: Make plots ----- #
source("src/utils/plotting-tools.R")
bar_fills <- list(
  per1 = list(color = "skyblue", alpha = 0.4, line_type = "dashed"), # 2000
  per2 = list(color = "forestgreen", alpha = 0.5, line_type = "solid") # 2019
)

fig01_slides <- plot_year_subgroup_bars(
  data = race_summary,
  yvar = hhsize,
  bar_fills = bar_fills,
  ytitle = "Average\nHousehold\nSize",
  title = NULL,
  label_size = 4.5
) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5, size = 13),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    legend.direction = "vertical",
    legend.text = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
fig01_slides

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/linear-reg/fig01-hhsize-race-year-bars-slides.png",
  plot = fig01_slides,
  width = 3600, height = 1800, units = "px", dpi = 300
)

dbDisconnect(con)
