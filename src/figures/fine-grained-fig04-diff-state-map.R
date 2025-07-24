# src/figures/fine-grained-fig04-diff-state-map.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This script takes on the counterfactual question with a geographic focus.
# It calculates CPUMA0010-level and state-level counterfactuals.
#
# This script will also hopefully eventually be used to create choropleth maps.
#
# Inputs:
#   - data/db/ipums.duckdb
#   - draws from function defined in src/utils/counterfactual-tools.R
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
state_sf <- readRDS("throughput/state_shapefiles.rds") # One shapefile row per state
load("data/helpers/state-pop-growth.rda") # May be deprecated

# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") |>
  # add an is_hoh column that is TRUE if the person is head of household, false otherwise
  mutate(is_hoh =as.integer(PERNUM == 1)) |>
  left_join(cpuma_state_cross, by = "CPUMA0010", copy = TRUE)

# Check: did we match all the states to statefips?
ipums_db |> pull(STATEFIP) |> is.na() |> sum() # yep it's 0
  
# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# Create a list of states to loop through later
list_of_states <- cpuma_state_cross |>
  select(State) |>
  unique()

# Generate data for all scenarios
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) 
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) 


# ---- Step 3: Calculate counterfactuals ---- #
# --- hhsize cf ---
# Calculate the fine-grained counterfactual (2 minutes)
cf_hhsize <- counterfactual_components(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "NUMPREC"
) |>
  # TODO: rename "State" upstream to "state"
  left_join(cpuma_state_cross, by = "CPUMA0010") # Adds "State" as a col
  
# Summarize the results from the counterfactual to make results ready to be merged 
# with state shapefiles for mapping
hhsize_state_summary <- summarize_counterfactual(
  cf_hhsize,
  counterfactual_by = "State",
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019
)$by |> select(
  State, 
  prop_2000,
  prop_2019,
  pop_2000,
  pop_2019,
  actual_2000,
  actual_2019,
  cf_2019,
  diff
)

# --- headship cf ---
# Calculate the fine-grained counterfactual (2 minutes)
cf_headship <- counterfactual_components(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019,
  p0_data = p0_sample, 
  p1_data = p1_sample,
  outcome = "is_hoh"
) |>
  # TODO: rename "State" upstream to "state"
  left_join(cpuma_state_cross, by = "CPUMA0010") # Adds "State" as a col

# Summarize the results from the counterfactual to make results ready to be merged 
# with state shapefiles for mapping
headship_state_summary <- summarize_counterfactual(
  cf_headship,
  counterfactual_by = "State",
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019
)$by |> select(
  State, 
  prop_2000,
  prop_2019,
  pop_2000,
  pop_2019,
  actual_2000,
  actual_2019,
  cf_2019,
  diff
)

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
    breaks = seq(from = -0.04, to = 0.0, by = 0.01)
  ) +
  theme_void()
fig04b

# ----- Step 5: Save figures ----- #
ggsave("output/figures/fine-grained/fig04a-hhsize-diff-state-map.png", plot = fig04a, width = 6.5, height = 5, dpi = 300)
ggsave("output/figures/fine-grained/fig04b-headship-diff-state-map.png", plot = fig04b, width = 6.5, height = 5, dpi = 300)

### FIG 4a: Unexplained diff by cpuma, nationally
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
  left_join(hhsize_contributions_state, by = "CPUMA0010") |>
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
##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################


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

# Function to generate base64-encoded ggplot images
dotplot_base64 <- function(
    state, 
    data, 
    x_min = -0.5, # Lowest x-value on dotplot
    x_max = 0.5 # Highest x-value on dotplot
    ) {
  file_path <- tempfile(fileext = ".png")
  plot <- dotplot_by_state(state, data, x_min, x_max)
  # Save plot as PNG to a temporary file
  ggsave(file_path, plot = plot, width = 5, height = 1, dpi = 100, units = "in")
  # Convert to base64
  base64_img <- base64encode(file_path)
  # Create an HTML img tag with the base64 string
  img_tag <- sprintf('<img src="data:image/png;base64,%s" width="400px" height="80px"/>', base64_img)
  
  return(img_tag)
}

# Add to tables 3.3 and 3.4: "Plot" column which includes base 64 encoded images of dotplots.
# Table 3.3 (Persons per household)
# The warning messages of removal of rows are expected: since our x range is -0.5 to
# 0.5, we exclude two observations falling outside that range. Small sacrifice to make 
# the data easier to view.
# TODO: Add warning showing number of excluded observations based on inputted x_min
# and x_max
# TODO: attach metadata about these x-axis limits to plots themselves and automatically
# read/display in the server rendering of figures
hhsize_state_summary$plot <- sapply(hhsize_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = hhsize_contributions_state,
    x_min = -0.5,
    x_max = 0.5 
    )
})
# Table 3.4 (Persons per bedroom)
bedroom_state_summary$plot <- sapply(bedroom_state_summary$State, function(state) {
  dotplot_base64(
    state = state, 
    data = bedroom_contributions_state,
    x_min = -0.9,
    x_max = 0.1
  )
})

# ----- Step 4: Merge data for scatterplots ----- #
# Figures 3.1 and 3.2

# Figure 3.1: Scatter household density change with bedroom density change

# Figure 3.2: Scatter household density change with population change
fig3.2_tab <- hhsize_state_summary |> select(-plot) |>
  inner_join(state_pop_growth, by = "State")

# ----- Step 5: Save the results ----- #

# Diff data
save(
  hhsize_contributions_state,
  hhsize_state_summary,
  bedroom_contributions_state,
  bedroom_state_summary,
  list_of_states,
  fig_3.2_tab,
  file = "shiny-app/data/diffs-by-geography.rda"
)
