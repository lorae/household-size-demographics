# mcclure-schwartz/tab1-replication.R
# 
# The purpose of this script is to replicate the McClure-Schwartz housing shortage
# analysis at a national level in Figure 1 of their paper.

library("tidycensus")
library("dplyr")
library("tidyr")

# Load API key
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

years <- c(2000, 2010, 2020)

# Function to fetch state-level or national data
get_census_data <- function(year, geography = "state") {
  if (year == 2020) {
    vars <- c(units = "H1_001N", occ_units = "H1_002N")
    sumfile <- "pl"
  } else {
    vars <- c(units = "H001001", occ_units = "H003002")
    sumfile <- "sf1"
  }
  
  get_decennial(
    geography = geography,
    variables = vars,
    year = year,
    sumfile = sumfile,
    output = "wide"
  ) |>
    mutate(
      region = if (geography == "us") "United States" else NAME,
      year = year,
      vac_units = units - occ_units,
      vr = vac_units / units
    ) |>
    select(region, year, units, occ_units, vac_units, vr)
}

# State-level data
state_data <- bind_rows(lapply(years, get_census_data, geography = "state"))

# National row
national_data <- bind_rows(lapply(years, get_census_data, geography = "us"))

# Combine and reshape
summary_table <- bind_rows(state_data, national_data) |>
  pivot_longer(cols = -c(region, year), names_to = "metric", values_to = "value") |>
  mutate(column = paste0(metric, "_", year)) |>
  select(region, column, value) |>
  pivot_wider(names_from = column, values_from = value) |>
  mutate(
    grow_occ_2000_2010 = occ_units_2010 - occ_units_2000,
    grow_occ_2010_2020 = occ_units_2020 - occ_units_2010,
    grow_occ_2000_2020 = occ_units_2020 - occ_units_2000,
    grow_units_2000_2010 = units_2010 - units_2000,
    grow_units_2010_2020 = units_2020 - units_2010,
    grow_units_2000_2020 = units_2020 - units_2000,
    unit_surplus_2000_2010 = grow_units_2000_2010 - grow_occ_2000_2010,
    unit_surplus_2010_2020 = grow_units_2010_2020 - grow_occ_2010_2020,
    unit_surplus_2000_2020 = grow_units_2000_2020 - grow_occ_2000_2020
  )
