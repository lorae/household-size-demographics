# process-ipums.R
#
# This script adds bucket columns to raw data based on specifications outlined in
# CSV files in the `lookup_tables/` directory.
# It reads data from the "ipums" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/db/ipums-processed.duckdb`.
#
# According to the Census Bureau: "A combination of SAMPLE and SERIAL provides a unique 
# identifier for every household in the IPUMS; the combination of SAMPLE, SERIAL, 
# and PERNUM uniquely identifies every person in the database."
#
# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")
library("readxl")

# temp: set working directory
setwd("/scratch/gpfs/ls4540/household-size-demographics")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums")

# For data validation: ensure no rows are dropped
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()

# ----- Step 3: Create a new table to write processed columns to ----- #

compute(
  tbl(con, "ipums"),
  name = "ipums_processed",
  temporary = FALSE,
  overwrite = TRUE
)

# ----- Step 4: Add pers_id" and "hh_id" columns to "ipums_processed" ---- #

# "pers_id" column
dbExecute(con, "
  ALTER TABLE ipums_processed ADD COLUMN pers_id TEXT;
  UPDATE ipums_processed
  SET pers_id = SAMPLE || '_' || SERIAL || '_' || PERNUM;
")

validate_row_counts(
  db = tbl(con, "ipums_processed"),
  expected_count = obs_count,
  step_description = "pers_id column was added"
)

# "hh_id" column
dbExecute(con, "
  ALTER TABLE ipums_processed ADD COLUMN hh_id TEXT;
  UPDATE ipums_processed
  SET hh_id = SAMPLE || '_' || SERIAL;
")

validate_row_counts(
  db = tbl(con, "ipums_processed"),
  expected_count = obs_count,
  step_description = "hh_id column was added"
)

# ----- Step 5: Generate and execute SQL queries for bucketed columns ----- #

# Define the list of bucket columns to be added
# TODO: adjust logic so -Inf and Inf values are accepted in lookup tables.
bucket_columns <- list(
  list(
    lookup_filepath = "lookup_tables/age/age_buckets01.csv",
    input_column = "AGE"
  ),
  list(
    lookup_filepath = "lookup_tables/hhincome/hhincome_buckets03.csv",
    input_column = "HHINCOME"
  ),
  list(
    lookup_filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    input_column = "HISPAN"
  ),
  list(
    lookup_filepath = "lookup_tables/race/race_buckets00.csv",
    input_column = "RACE"
  ),
  list(
    lookup_filepath = "lookup_tables/educ/educ_buckets00.csv",
    input_column = "EDUC"
  )
)

# Add bucketed columns using the lookup tables
for (bucket in bucket_columns) {
  # Load lookup table
  lookup_table <- read_csv(bucket$lookup_filepath, col_types = cols())
  
  # Split lookup table into range and value tables
  lookup_split <- split_lookup_table(bucket$lookup_filepath)
  range_lookup_table <- lookup_split$range
  value_lookup_table <- lookup_split$value
  
  # Write SQL to add the bucketed column
  start_time <- Sys.time()
  sql_query <- write_sql_query(
    range_lookup_table = range_lookup_table,
    value_lookup_table = value_lookup_table,
    col = bucket$input_column,
    table = "ipums_processed"
  )
  
  # Execute the query to add the new column
  # Split the SQL query in 2: you cannot alter table and execute at once
  queries <- strsplit(sql_query, ";")[[1]]
  # Extracting each part
  sql_query_alter <- paste0(queries[1], ";")  # First part (ALTER TABLE)
  sql_query_update <- paste0(queries[2], ";")  # Second part (UPDATE)
  
  dbExecute(con, sql_query_alter)  # Execute the ALTER TABLE part
  dbExecute(con, sql_query_update)  # Execute the UPDATE part
  end_time <- Sys.time()
  cat("Time taken for", bucket$input_column, "bucketing: ", end_time - start_time, "\n")
  
  # Validate row count
  validate_row_counts(
    db = tbl(con, "ipums_processed"),
    expected_count = obs_count,
    step_description = glue::glue("{bucket$input_column} bucketed column was added")
  )
}

# "RACE_ETH_bucket" (by combining entries in HISPAN_bucket and RACE_bucket)
start_time <- Sys.time()
sql_query <- write_race_eth_sql_query(
  table = "ipums_processed"
)
dbExecute(con, sql_query)
end_time <- Sys.time()
cat("Time taken for race/ethnicity bucketing: ", end_time - start_time, "\n")

validate_row_counts(
  db = ipums_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)

# ----- Step 6: Add CPI-U data ----- #
cpiu <- read_excel(
  path = "data/helpers/CPI-U.xlsx",
  sheet = "BLS Data Series",
  range = "A12:N36",
  col_names = TRUE
) |>
  select(Year, Annual) |>
  rename(
    YEAR = Year,
    cpiu = Annual
  )

# Get the 2010 value of cpi_u
cpiu_2010_value <- cpiu |>  filter(YEAR == 2010) |> pull(cpiu)

# Add a new column cpi_u_2010
cpiu <- cpiu |>
  mutate(cpiu_2010_deflator = cpiu / cpiu_2010_value)

# Recalculate ipums_db using the latest table
ipums_db <- tbl(con, "ipums_processed")

# Enrich with new derived columns
ipums_processed_final <- ipums_db |>
  left_join(cpiu, by = "YEAR", copy = TRUE) |>
  mutate(
    INCTOT_cpiu_2010 = if_else(
      INCTOT %in% c(9999999, 9999998), 
      NA_real_, 
      INCTOT / cpiu_2010_deflator
    ),
    INCTOT_cpiu_2010 = if_else(
      AGE < 15,
      0,
      INCTOT_cpiu_2010
    ),
    # TODO: incorporate this logic into a lookup table. Preferably the HHINCOME
    # one.
    INCTOT_cpiu_2010_bucket = case_when(
      INCTOT_cpiu_2010 < 0 ~ "neg",
      INCTOT_cpiu_2010 >= 0 & INCTOT_cpiu_2010 < 30000 ~ "00-30k",
      INCTOT_cpiu_2010 >= 30000 & INCTOT_cpiu_2010 < 60000 ~ "30-60k",
      INCTOT_cpiu_2010 >= 60000 & INCTOT_cpiu_2010 < 100000 ~ "60-100k",
      INCTOT_cpiu_2010 >= 100000 & INCTOT_cpiu_2010 < 150000 ~ "100-150k",
      INCTOT_cpiu_2010 >= 150000 & INCTOT_cpiu_2010 < 200000 ~ "150-200k",
      INCTOT_cpiu_2010 >= 200000 ~ "200kplus",
      TRUE ~ NA_character_
    ),
    us_born = BPL <= 120,
    persons_per_bedroom = NUMPREC / BEDROOMS
  ) |>
  mutate(
    tenure = ifelse(OWNERSHP == 1, "homeowner", "renter"),
    gender = ifelse(SEX == 1, "male", "female"),
    cpuma = as.character(CPUMA0010)
  )

# Write to a temporary new table name
compute(
  ipums_processed_final,
  name = "ipums_processed_temp",
  temporary = FALSE,
  overwrite = TRUE
)

# Drop the original table
dbExecute(con, "DROP TABLE ipums_processed")

# Rename the temporary table
dbExecute(con, "ALTER TABLE ipums_processed_temp RENAME TO ipums_processed")

# (Optional) Reload ipums_db
ipums_db <- tbl(con, "ipums_processed")

validate_row_counts(
  db = ipums_db,
  expected_count = obs_count,
  step_description = "final columns (income, US-born, persons/bedroom) were added"
)

# ----- Step 6: Clean up ----- #

DBI::dbDisconnect(con)

