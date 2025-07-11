# kob/scripts/kob-prepare-data.R
# The purpose of this script is to use outputs of regression to prepare data for input into 
# the kob pipeline in kob-function.R
# It standardizes regression results and population proportinos into kob_input 
# list of data frames, with each named entry (e.g. $numprec, $bedroom) having
# all the necessary columns to compute the kob function.

# ----- Step 0: Config & source helper functions ----- #
library(purrr)
library(dplyr)

devtools::load_all("../dataduck")

# ----- Step 1: Define throughput file paths  ----- #
input_paths <- tibble::tribble(
  ~term,     ~year, ~path,
  "props",   2000,  "throughput/props00_2000.rds",
  "props",   2019,  "throughput/props00_2019.rds",
  "numprec", 2000,  "throughput/model00_2000_numprec_summary-v2.rds",
  "numprec", 2019,  "throughput/model00_2019_numprec_summary.rds", #throughput/model00_2019_numprec_summary-beta.rds"
  "ppr",     2000,  "throughput/model00_2000_persons_per_room_summary.rds",
  "ppr",     2019,  "throughput/model00_2019_persons_per_room_summary-v5.rds",
  "ppbr",    2000,  "throughput/model00_2000_persons_per_bedroom_summary.rds",
  "ppbr",    2019,  "throughput/model00_2019_persons_per_bedroom_summary-v5.rds",
  "room",    2000,  "throughput/model00_2000_room_summary.rds",
  "room",    2019,  "throughput/model00_2019_room_summary-v5.rds",
  "bedroom", 2000,  "throughput/model00_2000_bedroom_summary.rds",
  "bedroom", 2019,  "throughput/model00_2019_bedroom_summary-v5.rds"
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

# ----- Step 2: Read in proportion data ----- #
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
  
  # Safe left join
  joined <- dplyr::left_join(data1, data2, by = "term")
  
  return(joined)
}

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
read_coefs_2000 <- function(path) {
  output <- readRDS(path) |>
    select(term, estimate, std.error) |>
    rename(
      coef_2000 = estimate,
      coef_2000_se = std.error
    )
  
  return(output)
}

read_coefs_2019 <- function(path) {
  output <- readRDS(path) |>
    rename(
      coef_2019 = estimate,
      coef_2019_se = se_estimate
    )
  
  return(output)
}

# Define function to pull coefficient data from both years and combine with props
# data
join_coefs_props <- function(coef_name) {
  coefs <- join_data_by_term(
    read_coefs_2000(get_path(coef_name, 2000)), 
    read_coefs_2019(get_path(coef_name, 2019))
  )
  
  # Left join: keep only terms found in regression results; OK if some props are unused
  # Also, if regression results include an (Intercept) term, that will produce an NA
  # in the props columns, which is expected behavior
  left_join(coefs, props, by = "term") 
}

# Apply this function and save outcomes in a named list
kob_input <- map(coef_names, join_coefs_props) |>  set_names(coef_names)

# Quality check: Do any of above defined files have NA values?
na_summary <- map(kob_input, ~ anyNA(.x))

if (any(unlist(na_summary))) {
  failed <- names(na_summary[na_summary == TRUE])
  stop("❌ NA values detected in the following `kob_input` entries: ", paste(failed, collapse = ", "))
}

# Save kob_input to throughput/
saveRDS(kob_input, "throughput/kob_input.rds")
  