# src/figures/fine-grained-fig04-diff-state-map.R
#
# This script produces choropleth maps of differences between actual and expected
# household size and headship rates by state in 2019
#
# Inputs:
#   - data/db/ipums.duckdb
#   - src/utils/counterfactual-tools.R
#   - TODO name the other througput files / helper files and also add those scripts
# Outputs:
#   - TBD

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
hhsize_state_summary <- readRDS("throughput/fine-grained-hhsize-diff-state.rds")
headship_state_summary <- readRDS("throughput/fine-grained-headship-diff-state.rds")
cf_summaries <- readRDS("throughput/fine-grained-cf-summaries.rds")

# ----- Step 4: Map ----- #
# --- Fig04a: hhsize diff by state ----
state_sf_hhsize <- state_sf |>
  left_join(hhsize_state_summary, by = "State")

fig04a <- ggplot(state_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Unexplained \nDifference, \nPersons per \nHousehold",
    low = "blue", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.5, to = 0.2, by = 0.05)
  ) +
  theme_void()
fig04a

# --- Fig04b: headship diff by state --- 
state_sf_headship <- state_sf |>
  left_join(headship_state_summary, by = "State")

# Choropleth map (color version)
fig04b <- ggplot(state_sf_headship |> filter(State != "District of Columbia")) + 
  geom_sf(aes(geometry = geometry, fill = diff), color = "black", size = 0.5) +
  scale_fill_gradient2(
    name = "Unexplained \nDifference, \nHeadship \nRate",
    low = "blue", mid = "white", high = "#F94144", midpoint = 0,
    breaks = seq(from = -0.04, to = 0.1, by = 0.01)
  ) +
  theme_void()
fig04b

# ----- Step 5: Save output ----- #
# Figures
ggsave("output/figures/fine-grained/fig04a-hhsize-diff-state-map.png", plot = fig04a, width = 6.5, height = 4, dpi = 300)
ggsave("output/figures/fine-grained/fig04b-headship-diff-state-map.png", plot = fig04b, width = 6.5, height = 4, dpi = 300)
