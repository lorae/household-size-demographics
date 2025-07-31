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
library("sf")
library("ggplot2")

devtools::load_all("../dataduck")
source("src/utils/aggregation-tools.R") # tabulate_summary_2year
source("src/utils/mapping-tools.R") # transform_state

# ----- Step 1: Load in data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")
ipums_db_filtered <- ipums_db |> filter(GQ %in% c(0,1,2))
# This cpuma_sf file is created by running src/utils/mapping-tools.R
cpuma_sf <- readRDS("throughput/cpuma_shapefiles.rds") 

# ----- Step 2: Wrangle data ----- #
hhsize_cpuma <- tabulate_summary_2year(
  data = ipums_db_filtered, 
  years = c(2000, 2019),
  value = "NUMPREC",
  group_by = "CPUMA0010",
  group_encoding = NULL
) |>
  rename(CPUMA0010 = subgroup)


# ----- Step 3: Map ----- #
# Join the state data with household size differences
cpuma_sf_hhsize <- cpuma_sf |>
  left_join(hhsize_cpuma, by = "CPUMA0010")


# Choropleth map (color version) 2000
fig04a <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_2000), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Household size,\n2000",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 3.30,
    breaks = seq(from = 2.5, to = 5.0, by = 0.5)
  ) +
  theme_void()
fig04a

# Choropleth map (color version) 2019
fig04a <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_2019), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Household size,\n2000",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 3.30,
    breaks = seq(from = 2.5, to = 5.0, by = 0.5)
  ) +
  theme_void()
fig04a

# Choropleth map (color version)
fig04a <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_pctchg_2000_2019), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Change in \nHousehold \nSize",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.8, to = 0.6, by = 0.2)
  ) +
  theme_void()
fig04a

