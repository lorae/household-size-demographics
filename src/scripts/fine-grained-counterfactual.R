# src/scripts/fine-grained-counterfactual.R
#
# The purpose of this script is to produce throughput data used in fine-grained
# chart outputs: fine-grained-fig02, -fig03, -fig04
#
# Inputs:
# - TODO
#
# Outputs:
# - TODO


# ----- Step 0: Load required packages ----- #
# TODO: remove unneeded imports
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
# TODO: the below analysis is replicated in fig04; perhaps do once elsewhere

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation
load("data/helpers/cpuma-state-cross.rda") # Crosswalks CPUMA0010 to state
state_sf <- readRDS("throughput/state_shapefiles.rds") # One shapefile row per state

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
  expected_2019,
  diff
)

# Top line summary
hhsize_summary <- summarize_counterfactual(
  cf_hhsize,
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019
)$by |>
  mutate(
    name = "Number of Persons in Household"
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
  expected_2019,
  diff
)

# Top line summary
headship_summary <- summarize_counterfactual(
  cf_headship,
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010"),
  p0 = 2000,
  p1 = 2019
)$by |>
  mutate(
    name = "Headship Rate"
  )

# ----- Step 3: Save throughput ----- #
saveRDS(hhsize_state_summary, "throughput/fine-grained-hhsize-diff-state.rds")
saveRDS(headship_state_summary, "throughput/fine-grained-headship-diff-state.rds")
write.csv(hhsize_state_summary, "throughput/fine-grained-hhsize-diff-state.csv", row.names = FALSE)
write.csv(headship_state_summary, "throughput/fine-grained-headship-diff-state.csv", row.names = FALSE)
saveRDS(
  bind_rows(hhsize_summary, headship_summary),
  "throughput/fine-grained-cf-summaries.rds"
)
