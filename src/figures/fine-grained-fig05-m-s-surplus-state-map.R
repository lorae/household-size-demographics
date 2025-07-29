# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library("base64enc")
library("sf")
options(scipen = 999)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
load("data/helpers/state-pop-growth.rda") # May be deprecated

# ----- Step 2: Import data ----- #
state_sf <- readRDS("throughput/state_shapefiles.rds") # One shapefile row per state
ms_state_summary <- readRDS("throughput/mcclure-schwartz-analysis-state.rds") |>
  filter(region != "United States", region != "Puerto Rico") |>
  rename(State = region) |>
  mutate(
    scaled_surplus = unit_surplus_2000_2020 / units_2020
  )

write.csv(ms_state_summary, "mcclure-scwartz-state.csv")

# ----- Step 4: Map ----- #
# --- Fig04a: hhsize diff by state ----
state_sf_hhsize <- state_sf |>
  left_join(ms_state_summary, by = "State")

fig05a <- ggplot(state_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = unit_surplus_2000_2020), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Housing surplus, number of units",
    low = "#F94144", mid = "white", high = "blue", midpoint = 0,
    breaks = seq(from = -0, to = 350000, by = 50000)
  ) +
  theme_void()
fig05a

fig05b <- ggplot(state_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = scaled_surplus), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Housing surplus, number of units",
    low = "#F94144", mid = "white", high = "blue", midpoint = 0,
    breaks = seq(from = 0, to = 0.06, by = 0.01)
  ) +
  theme_void()
fig05b