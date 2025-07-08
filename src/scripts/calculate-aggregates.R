#src/scripts/calculate-aggregates.R
#
# The purpose of this script is to calculate a select number of aggregate values:
# - NUMPREC: Mean number of housemates a non-group-quartered individual lives with, 
#   2000 and 2019
# - room: Mean number of rooms a non-group-quartered individual has in their home, 
#   2000 and 2019
# - bedroom: Mean number of bedrooms a non-group-quartered individual has in their
#   home, 2000 and 2019
# - persons_per_room: Mean number of persons per room
# - persons_per_bedroom: Mean number of persons per bedroom
#
# These aggregates are needed to validate the KOB analysis.

# ----- Step 0: Config ----- #

library(duckdb)
library(dplyr)
library(tibble)

# ----- Step 1: Load in survey data ----- #
# TODO: If needed, read in survey data to make this run with standard errors.
# If not, keep as-is. It is simple and fast.

# Database API connection
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Define the function from before
weighted_mean_ipums <- function(var, year) {
  ipums_db |>
    filter(GQ %in% c(0, 1, 2), YEAR == year) |>
    summarise(weighted_mean = sum(.data[[var]] * PERWT) / sum(PERWT)) |>
    pull()
}

# List of variables to compute
vars <- c("NUMPREC", "bedroom", "room", "persons_per_bedroom", "persons_per_room")

# Create tibble of results
aggregates <- tibble(
  variable = vars,
  mean_2000 = sapply(vars, weighted_mean_ipums, year = 2000),
  mean_2019 = sapply(vars, weighted_mean_ipums, year = 2019)
)

# Save the results tibble
saveRDS(aggregates, "throughput/aggregates.rds")