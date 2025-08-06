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
options(scipen = 999)

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

# Join the state data with household size differences
cpuma_sf_hhsize <- cpuma_sf |>
  left_join(hhsize_cpuma, by = "CPUMA0010")

# ----- Step 3: Map ----- #
# Choose the center point of the color scale (white color on figs A02 and A03)
center <- 3.345
# Choropleth map 2000
figA02 <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_2000), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Household size\n(number\nof persons)",
    low = "darkblue", mid = "white", high = "darkred", midpoint = center,
    breaks = seq(from = 2.5, to = 5.0, by = 0.5)
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 20, 0, 0))  # top, right, bottom, left. Add space for legend.
figA02

# Choropleth map 2019
figA03 <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_2019), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Household size\n(number\nof persons)",
    low = "darkblue", mid = "white", high = "darkred", midpoint = center,
    breaks = seq(from = 2.5, to = 5.0, by = 0.5)
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 20, 0, 0)) # top, right, bottom, left. Add space for legend.
figA03

# Choropleth map 2000 -> 2019 percentage change
figA04 <- ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = hhsize_pctchg_2000_2019), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "Percentage\nchange",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -20, to = 20, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 20, 0, 0)) # top, right, bottom, left. Add space for legend.
figA04


### EXPERIMENTAL PLOT: Plot the same CPUMA-level metric, but the CPUMA-level
# counterfactual KOB results
kob_output <- readRDS("throughput/kob_output.rds")
cpuma_cf <- kob_output$p |> filter(variable == "cpuma") |> mutate(CPUMA0010 = as.numeric(value)) |>
  mutate(diff_c = c / prop_2019)
cpuma_cf_sf <- cpuma_cf |>
  left_join(cpuma_sf, by = "CPUMA0010")
  
# Choropleth map 2000 -> 2019 c coeffcieints
figAexp <- ggplot(cpuma_cf_sf) + 
  geom_sf(aes(geometry = geometry, fill = diff_c), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "CPUMA-level diffs",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.4, to = 0.4, by = 0.2),
    labels = function(x) format(x, scientific = FALSE, digits = 3)
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 20, 0, 0)) # top, right, bottom, left. Add space for legend.
figAexp

## EXPERIMENTAL plot: Do the same as above, but # bedrooms instead of # people
kob_output <- readRDS("throughput/kob_output.rds")
cpuma_cf_bed <- kob_output$b |> filter(variable == "cpuma") |> mutate(CPUMA0010 = as.numeric(value)) |>
  mutate(diff_c = c / prop_2019)
cpuma_cf_bed_sf <- cpuma_cf_bed |>
  left_join(cpuma_sf, by = "CPUMA0010")

# Choropleth map 2000 -> 2019 c coeffcieints
# that's REALLY interesting!!
figBexp <- ggplot(cpuma_cf_bed_sf) + 
  geom_sf(aes(geometry = geometry, fill = diff_c), color = NA, size = 0) +
  geom_sf(data = cpuma_sf_hhsize, aes(geometry = geometry), color = "grey50", fill = NA, size = 0.1) +  # Overlay state boundaries
  scale_fill_gradient2(
    name = "CPUMA-level diffs",
    low = "darkblue", mid = "white", high = "darkred", midpoint = 0,
    breaks = seq(from = -0.4, to = 0.4, by = 0.2),
    labels = function(x) format(x, scientific = FALSE, digits = 3)
  ) +
  theme_void() +
  theme(plot.margin = margin(0, 20, 0, 0)) # top, right, bottom, left. Add space for legend.
figBexp

# TODO: repeat this CPUMA mapping, but for individual cities. Create lists of the 
# CPUMAs that fall in a certain MSA/CSA

# ----- Step 4: Save maps ----- #
ggsave(
  "output/figures/linear-reg/figA02-hhsize-2000-cpuma.png", 
  plot = figA02, 
  width = 4000, height = 3000, units = "px", dpi = 300
)
ggsave(
  "output/figures/linear-reg/figA03-hhsize-2019-cpuma.png", 
  plot = figA03, 
  width = 4000, height = 3000, units = "px", dpi = 300
)
ggsave(
  "output/figures/linear-reg/figA04-hhsize-pct-change-cpuma.png", 
  plot = figA04, 
  width = 4000, height = 3000, units = "px", dpi = 300
)
