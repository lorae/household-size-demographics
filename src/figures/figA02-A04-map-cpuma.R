# src/figures/figA02-A04-map-cpuma.R
#
# This script produces choropleth maps of household sizes and other metrics and the
# changes in those values over time.
#
# Inputs:
#   - TODO: list inputs
# Outputs:
#   - TODO: list inputs

# TODO: import data. This is helpful prior code I used to create this map.

# ----- Step 0: Config ----- #
library("duckdb")
library("dplyr")
library("tidyr")

devtools::load_all("../dataduck")
source("src/utils/aggregation-tools.R") # tabulate_summary_2year

# ----- Step 1: Load in data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")
ipums_db_filtered <- ipums_db |> filter(GQ %in% c(0,1,2))

# ----- Step 2: Wrangle data ----- #
hhsize_cpuma <- tabulate_summary_2year(
  data = ipums_db_filtered, 
  years = c(2000, 2019),
  value = "NUMPREC",
  group_by = "CPUMA0010",
  group_encoding = NULL
)


# ----- Step 3: Map ----- #

# Load shapefiles. Data is unzipped from WHERE? TODO: document
cpuma_sf <- st_read("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp") |>
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes

# Rotate and move Alaska and Hawaii to fit on map
alaska_cpuma <- transform_state(cpuma_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii_cpuma <- transform_state(cpuma_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
cpuma_sf_final <- cpuma_sf |>
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska_cpuma, hawaii_cpuma)

# Join the state data with household size differences
cpuma_sf_hhsize <- cpuma_sf_final |>
  left_join(hhsize_cpuma, by = "CPUMA0010") |>
  mutate(hhsize_unexplained = contribution_diff / prop_2019) 

# Choropleth map (color version)
fig04a <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = NA, size = 0) +
  geom_sf(data = state_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()
fig04a


# A function that produces a dotplot by state
dotplot_by_state <- function(
    state = "New Jersey",
    data = hhsize_contributions_state, # or bedroom_contributions_state
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
) {
  # Subset the data to just that state
  boxplot_data <- subset(data, State == state)
  
  # Calculate median, weighted median, and weighted mean
  median <- boxplot_data |>
    pull(diff) |> 
    median()
  weighted_median <- rep(boxplot_data$diff, times = boxplot_data$pop_2019) |>
    median()
  weighted_mean <- weighted.mean(boxplot_data$diff, w = boxplot_data$pop_2019)
  
  # Create the horizontal boxplot with overlaid points
  output_plot <- ggplot(boxplot_data, aes(x = diff, y = "")) +
    geom_dotplot(stackdir = "center", dotsize = 0.5, alpha = 0.6, binwidth = 0.02) +
    theme_minimal() +
    labs(title = "",
         x = "",
         y = "") +
    theme_void() +
    geom_vline(xintercept = weighted_mean, linetype = "dotted", color = "red", size = 0.5) +
    geom_vline(xintercept = weighted_median, linetype = "dotted", color = "blue", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1) +
    xlim(x_min, x_max)
  
  
  return(output_plot)
}
