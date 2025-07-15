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
) |> collect() |> arrange(RACE_ETH_bucket, NUMPREC)

# ----- Step 3: Make plots ----- #
# Choose a max NUMPREC (household size) to display in histogram
topcode_hhsize <- 10 

# Topcode the table
fig02_data <- hhsize_race_2019 |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, topcode_hhsize, NUMPREC)) |>
  group_by(RACE_ETH_bucket, NUMPREC) |>
  summarize(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  arrange(RACE_ETH_bucket, NUMPREC) |>
  # Add frequencies for each NUMPREC value within RACE_ETH_bucket subpopulations
  group_by(RACE_ETH_bucket) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup()

# Plot!!!
filtered_data <- fig02_data |> filter(RACE_ETH_bucket == "Black")
main_color <- "steelblue"

ggplot(filtered_data, aes(x = factor(NUMPREC), y = freq)) +
  geom_bar(
    stat = "identity",
    width = 1,
    fill = scales::alpha(main_color, 0.5),
    color = main_color,
    size = 0.3
  ) +
  labs(
    x = "Household Size (NUMPREC)",
    y = "Frequency",
    title = paste("Household Size Distribution —", unique(filtered_data$RACE_ETH_bucket))
  ) +
  theme_minimal()