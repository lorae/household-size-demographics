# mcclure-schwartz/mcclure-schwartz-statewide.R
# 
# The purpose of this script is to replicate the McClure-Schwartz housing shortage
# analysis at a state level.

library("tidycensus")
library("dplyr")

# Load API key from .Renviron
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

years <- c(2000, 2010, 2020)

get_census_data <- function(year) {
  if (year == 2020) {
    # Use PL 94-171 file for 2020
    get_decennial(
      geography = "us",
      variables = c(
        total_units = "H1_001N",       # Total housing units
        occupied_units = "H1_002N"     # Occupied housing units (households)
      ),
      year = 2020,
      sumfile = "pl"
    )
  } else {
    # Use SF1 file for 2000 and 2010
    get_decennial(
      geography = "us",
      variables = c(
        total_units = "H001001",       # Total housing units
        occupied_units = "H003002"     # Occupied units (households)
      ),
      year = year,
      sumfile = "sf1"
    )
  }
}

get_census_data(2000) # Matches table 1 exactly
get_census_data(2010) # Matches table 1 exactly
get_census_data(2020) # MAtches table 1 exactly