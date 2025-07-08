# kob/scripts/kob-prepare-data.R
# The purpose of this script is to use outputs of regression to prepare data for input into 
# the kob pipeline in kob-function.R
# It standardizes regression results and population proportinos into a coef data
# frame
# TODO: this whole script could use a DRY refactor
# TODO: based on the regression #, build a list of expected terms and compare 
# after each step against those terms to validate rows haven't been added/dropped.
# Also, ensure no NAs/NaNs

# ----- Step 0: Config & source helper functions ----- #
library(purrr)
library(dplyr)

devtools::load_all("../dataduck")

# ----- Step 1: Define throughput file paths and read data ----- #
# 2000
props_2000_path <- "throughput/props00_2000.rds"
numprec_2000_path <- "throughput/model00_2000_numprec_summary-v2.rds"
ppr_2000_path <- "throughput/model00_2000_persons_per_room_summary.rds"
ppbr_2000_path <- "throughput/model00_2000_persons_per_bedroom_summary.rds"
room_2000_path <- "throughput/model00_2000_room_summary.rds"
bedroom_2000_path <- "throughput/model00_2000_bedroom_summary.rds"

# 2019
props_2019_path <- "throughput/props00_2019.rds"
numprec_2019_path <- "throughput/model00_2019_numprec_summary-beta.rds"
ppr_2019_path <- "throughput/model00_2019_persons_per_room_summary-v5.rds"
ppbr_2019_path <- "throughput/model00_2019_persons_per_bedroom_summary-v5.rds"
room_2019_path <- "throughput/model00_2019_room_summary-v5.rds"
bedroom_2019_path <- "throughput/model00_2019_bedroom_summary-v5.rds"

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
props_2000_svystat <- readRDS(props_2000_path)
props_2019_svystat <- readRDS(props_2019_path)

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

# ----- Step 3: Read in coefficients ----- #
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

coefs_numprec <- join_data_by_term(
  read_coefs_2000(numprec_2000_path), 
  read_coefs_2019(numprec_2019_path)
  )

coefs_room <- join_data_by_term(
  read_coefs_2000(room_2000_path), 
  read_coefs_2019(room_2019_path)
)

coefs_bedroom <- join_data_by_term(
  read_coefs_2000(bedroom_2000_path), 
  read_coefs_2019(bedroom_2019_path)
)

coefs_ppr <- join_data_by_term(
  read_coefs_2000(ppr_2000_path), 
  read_coefs_2019(ppr_2019_path)
)

coefs_ppbr <- join_data_by_term(
  read_coefs_2000(ppbr_2000_path), 
  read_coefs_2019(ppbr_2019_path)
)

# ----- Step 4: Combine into list of data frames for kob input----- #

join_coefs_props <- function(coef_name) {
  coefs <- get(paste0("coefs_", coef_name)) # e.g. coefs_ppbr, coefs_bedroom
  left_join(coefs, props, by = "term") # Left join: props has some variables that are omitted in regression
}

kob_input <- map(coef_names, join_coefs_props) |>  set_names(coef_names)

  