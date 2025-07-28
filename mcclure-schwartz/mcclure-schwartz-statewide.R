# mcclure-schwartz/mcclure-schwartz-statewide.R
# 
# The purpose of this script is to replicate the McClure-Schwartz housing shortage
# analysis at a state level.

library("tidycensus")
library("dplyr")
library("tidyr")

# Load API key
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

years <- c(2000, 2010, 2020)

get_census_data <- function(year) {
  if (year == 2020) {
    vars <- c(units = "H1_001N", occ_units = "H1_002N")
    sumfile <- "pl"
  } else {
    vars <- c(units = "H001001", occ_units = "H003002")
    sumfile <- "sf1"
  }
  
  get_decennial(
    geography = "us",
    variables = vars,
    year = year,
    sumfile = sumfile,
    output = "wide"
  ) |>
    mutate(
      year = year,
      vac_units = units - occ_units,
      vr = vac_units / units
    ) |>
    select(year, units, occ_units, vac_units, vr)
}

# Fetch data and pivot
summary_table <- bind_rows(lapply(years, get_census_data)) |>
  pivot_longer(cols = -year, names_to = "metric", values_to = "value") |>
  mutate(column = paste0(metric, "_", year)) |>
  select(column, value) |>
  pivot_wider(names_from = column, values_from = value) |>
  select(
    units_2000, occ_units_2000, vac_units_2000, vr_2000,
    units_2010, occ_units_2010, vac_units_2010, vr_2010,
    units_2020, occ_units_2020, vac_units_2020, vr_2020
  ) |>
  mutate(
    
  )

summary_table |> glimpse()
