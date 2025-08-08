# src/scripts/kob-build-input.R
# 
# Read raw regression results and population proportions. Standardize into kob_input 
# list of data frames, with each named entry (e.g. $numprec, $bedroom) having all 
# the necessary columns to be ingested in kob-build-output.R
#
# Input: throughput/ files - see input_paths below
# Output: throughput/kob_input.rds
#

# ----- Step 0: Config & source helper functions ----- #
library(purrr)
library(dplyr)
library(duckdb)

devtools::load_all("../dataduck")
source("src/utils/regression-postprocess-tools.R")

# ----- Step 1: Define throughput file paths  ----- #
# Initialize adjust_by
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed") 

adjust_by = list(
  AGE_bucket = ipums_db |> pull(AGE_bucket) |> unique(),
  EDUC_bucket = ipums_db |> pull(EDUC_bucket) |> unique(),
  INCTOT_cpiu_2010_bucket = ipums_db |> pull(INCTOT_cpiu_2010_bucket) |> unique(),
  us_born = ipums_db |> pull(us_born) |> unique(),
  tenure = ipums_db |> pull(tenure) |> unique(),
  gender = ipums_db |> pull(gender) |> unique(),
  cpuma = ipums_db |> pull(cpuma) |> unique(),
  RACE_ETH_bucket = ipums_db |> pull(RACE_ETH_bucket) |> unique()
)

dbDisconnect(con)

# TODO: unify terms with abbrev_variable and other ways I refer to these 
# regression outcomes across the pipeline workflow
input_paths <- tibble::tribble(
  ~term,     ~year, ~path,
  "props",   2000,  "throughput/reg01/2000_prop.rds",
  "props",   2019,  "throughput/reg01/2019_prop.rds",
  "numprec", 2000,  "throughput/reg01/2000_p.rds",
  "numprec", 2019,  "throughput/reg01/2019_p.rds",
  "ppr",     2000,  "throughput/reg01/2000_ppr.rds",
  "ppr",     2019,  "throughput/reg01/2019_ppr.rds",
  "ppbr",    2000,  "throughput/reg01/2000_ppbr.rds",
  "ppbr",    2019,  "throughput/reg01/2019_ppbr.rds",
  "room",    2000,  "throughput/reg01/2000_r.rds",
  "room",    2019,  "throughput/reg01/2019_r.rds",
  "bedroom", 2000,  "throughput/reg01/2000_b.rds",
  "bedroom", 2019,  "throughput/reg01/2019_b.rds"
)

get_path <- function(term, year) {
  input_paths |> 
    filter(term == !!term, year == !!year) |> 
    pull(path)
}

# Sanity check: Do all above defined files exist?
missing_paths <- input_paths |> filter(!file.exists(path))

if (nrow(missing_paths) > 0) {
  stop("❌ Missing file(s):\n", paste(missing_paths$path, collapse = "\n"))
}

# Helper function to combine two dataframes with identical terms (used for joining
# 2000 and 2019 data)
join_data_by_term <- function(data1, data2) {
  # Check that 'term' exists
  if (!("term" %in% names(data1)) || !("term" %in% names(data2))) {
    stop("Both data frames must contain a 'term' column.")
  }
  
  # Check that terms match exactly (including duplicates)
  if (!setequal(data1$term, data2$term)) {
    stop("Terms do not match exactly between the two data frames.")
  }
  
  # Optional: ensure no duplicates
  if (any(duplicated(data1$term)) || any(duplicated(data2$term))) {
    stop("Duplicate terms found. Cannot safely join.")
  }
  
  # Delete the variable and value cols
  data1 <- data1 |> select(-any_of(c("variable", "value")))
  data2 <- data2 |> select(-any_of(c("variable", "value")))
  
  # Safe left join
  joined <- dplyr::left_join(data1, data2, by = "term")
  
  return(joined)
}

# ----- Step 2: Read in proportion data ----- #
# Read proportions in as a svystat object
props_2000_svystat <- readRDS(get_path("props", 2000))
props_2019_svystat <- readRDS(get_path("props", 2019))

extract_prop <- function(svystat_obj, year) {
  # Calculate population proportions, SE on those estimates, and extract the variable
  # "value" (varname concatenated with value of variable, like "AGE_bucket0-4")
  prop <- as.numeric(svystat_obj)
  se <- sqrt(diag(attr(svystat_obj, "var")))
  term <- names(svystat_obj)
  
  # turn output into tibble and name cols using the year
  tibble::tibble(
    term = term,
    !!paste0("prop_", year) := prop,
    !!paste0("prop_", year, "_se") := se
  )
}

prop_2000 <- purrr::map_dfr(props_2000_svystat, extract_prop, year = 2000)
prop_2019 <-purrr::map_dfr(props_2019_svystat, extract_prop, year = 2019)

# Combined props from both years
props <- join_data_by_term(prop_2000, prop_2019)

# ----- Step 3: Read in coefficients, combine with props in output list ----- #
# Define list of coefficients to cycle through
coef_names <- c("numprec", "ppr", "ppbr", "room", "bedroom")

# Define helper functions for reading in data from 2000, 2019
read_coefs_2000 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    select(term, estimate, std.error) |>
    rename(
      coef_2000 = estimate,
      coef_2000_se = std.error
    )
  
  processed_output <- standardize_coefs(
    reg_data = output,
    adjust_by = adjust_by,
    coef_col = "coef_2000",
    se_col = "coef_2000_se"
  )
  
  return(processed_output)
}

read_coefs_2019 <- function(path, adjust_by) {
  output <- readRDS(path) |>
    rename(
      coef_2019 = estimate,
      coef_2019_se = se_estimate
    )
  
  processed_output <- standardize_coefs(
    reg_data = output,
    adjust_by = adjust_by,
    coef_col = "coef_2019",
    se_col = "coef_2019_se"
  )
  
  return(processed_output)
}

# Define function to pull coefficient data from both years and combine with props
# data
# Relies on props being efined outside the function. A bit sloppy.
join_coefs_props <- function(coef_name, props) {
  coefs <- join_data_by_term(
    read_coefs_2000(get_path(coef_name, 2000), adjust_by), 
    read_coefs_2019(get_path(coef_name, 2019), adjust_by)
  )

  # Left join: keep only terms found in regression results; OK if some props are unused
  # Also, if regression results include an (Intercept) term, that will produce an NA
  # in the props columns, which is expected behavior
  left_join(coefs, props, by = c("term")) 
}

# Apply this function and save outcomes in a named list
kob_input <- map(coef_names, join_coefs_props, props = props) |> set_names(coef_names)

# Quality check: Do any of above defined files have NA values?
na_summary <- map(kob_input, ~ .x |> filter(term != "(Intercept)") |> anyNA())

if (any(unlist(na_summary))) {
  failed <- names(na_summary[na_summary == TRUE])
  stop("❌ NA values detected in the following `kob_input` entries: ", paste(failed, collapse = ", "))
}

# Quality check: validate row counts
expected_len <- sum(lengths(adjust_by)) + 1  # +1 for intercept
bad_len <- map_lgl(kob_input, ~ nrow(.x) != expected_len)

if (any(bad_len)) {
  stop(
    "❌ Unexpected number of rows in: ",
    paste(names(kob_input)[bad_len], collapse = ", "),
    "\nExpected ", expected_len, " rows in each."
  )
}

# Save kob_input to throughput/
saveRDS(kob_input, "throughput/kob_input.rds")
