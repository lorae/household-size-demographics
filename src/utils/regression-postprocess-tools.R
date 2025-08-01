# src/utils/regression-postprocess-tools.R
#
# The purpose of this script is to provide testable, modular functions for transforming
# regression results
#
#
library(devtools)
source("kob/scripts/kob-function.R") # The kob function is used within add-intercept()


# ADD INTERCEPT
add_intercept <- function(
    reg_data,
    variable, # Variable to draw intercept from
    reference_value # value of variable that will become intercept
  ){
  # Validate that the variable exists in the 'variable' column
  if (!(variable %in% reg_data$variable)) {
    stop(paste0("Variable '", variable, "' not found in reg_data$variable."))
  }
  
  # Validate that the reference_value exists in the 'value' column
  if (!(reference_value %in% reg_data$value)) {
    stop(paste0("Reference value '", reference_value, "' not found in reg_data$value."))
  }
  
  # Validate that the reference_value and variable exist together in exactly one row
  reference_row <- reg_data |>
    filter(.data$variable == variable, .data$value == reference_value)
  
  if (nrow(reference_row) != 1) {
    stop(
      paste0(
        "Expected exactly one row with variable = '", variable,
        "' and value = '", reference_value, "', but found ", nrow(reference_row), "."
      )
    )
  }
  
  # Validate that all the required cols are present
  required_cols <- c("variable", "value", "coef_2000", "coef_2000_se",
                     "u", "u_se", "e", "e_se", "c", "c_se")
  missing_cols <- setdiff(required_cols, colnames(reg_data))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required column(s): ", paste(missing_cols, collapse = ", ")))
  }
  
  # Pull the affected rows that will be changed by the function
  affected_rows <- reg_data |>
    filter(variable == !!variable)
  
  reference_row <- reg_data |>
    filter(value == !!reference_value)
  
  # Subtract the reference row from the affected rows; modify affected values
  affected_rows_modified <- affected_rows |> 
    filter(value != !!reference_value) |> 
    mutate(
      coef_2000    = coef_2000 - reference_row$coef_2000,
      coef_2019    = coef_2019 - reference_row$coef_2019,
      coef_2000_se = sqrt(coef_2000_se^2 + reference_row$coef_2000_se^2),
      coef_2019_se = sqrt(coef_2019_se^2 + reference_row$coef_2019_se^2)
    )
  
  # Construct the new intercept row
  intercept_row <- reference_row |> 
    mutate(
      term = "(Intercept)",
      value = "(Intercept)",
      variable = "(Intercept)",
      coef_2000    = coef_2000,
      coef_2019    = coef_2019,
      coef_2000_se = coef_2000_se,
      coef_2019_se = coef_2019_se,
      prop_2000    = NA_real_,
      prop_2000_se = NA_real_,
      prop_2019    = NA_real_,
      prop_2019_se = NA_real_
    )
  
  # Combine together all the new rows, recompute their kob variables
  new_rows <- bind_rows(intercept_row, affected_rows_modified) |> 
    select(-c("u", "u_se", "e", "e_se", "c", "c_se")) |>
    kob()
  
  output <- reg_data |> 
    filter(variable != !!variable) |> 
    bind_rows(new_rows)
  
}

# REMOVE INTERCEPT
remove_intercept <- function(
    reg_result,
    variable # Variable to push intercept to
){
  # not needed for now, but would be nice to have. Should reverse add_intercept 
  # perfectly. A good unit test would check if that's the case
}




# Varnames fed into split_term_column() function, below
varnames_dict <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

# Function takes in any data frame with a "term" column and outputs the same
# data frame but with new variable (e.g. RACE_ETH_bucket) and value (e.g. "White")
# columns.
# Used in tidying KOB output but can also be used in regression outputs, etc.
# Necessitates a varnames_dict, which is defined globally above.
#
# The `term` column of the kob output takes the format variablevalue, e.g.
# RACE_ETH_bucketAAPI. This function uses the above varnames_dict to split up
# this string into variable (e.g. RACE_ETH_bucket) and value (AAPI), and appends
# to the kob_output data frame.
split_term_column <- function(kob_output, varnames = varnames_dict) {
  # Define a helper function to extract the variable prefix
  extract_variable <- function(term, varnames) {
    if (term == "(Intercept)") return("(Intercept)")  # explicitly handle intercept
    matched <- varnames[str_detect(term, fixed(varnames))]
    if (length(matched) > 0) return(matched[1])
    return(NA_character_)
  }
  
  kob_output <- kob_output |> 
    mutate(
      variable = map_chr(term, ~ extract_variable(.x, varnames)),
      value = case_when(
        term == "(Intercept)" ~ "(Intercept)",
        !is.na(variable) ~ str_remove(term, fixed(variable)),
        TRUE ~ term
      )
    ) |>
    select(term, variable, value, everything())
  
  unmatched <- kob_output |> filter(is.na(variable)) |> pull(term)
  if (length(unmatched) > 0) {
    warning("Some terms could not be matched to a variable prefix: ", paste(unique(unmatched), collapse = ", "))
  }
  
  return(kob_output)
}


# This function implements the Gardeazabal-Ugidos adjustment on regression results.
# Essentially, it makes regression coefficients insensitive to the choice of omitted
# category.
# References:
# https://cran.r-project.org/web/packages/oaxaca/vignettes/oaxaca.pdf
# TODO: add link to original G-U paper
gu_adjust <- function(
    reg_output,
    adjust_vars = c("RACE_ETH_bucket"),
    coef_col = "estimate"
) {
  # Ensure 'variable' and 'value' columns exist (safe to reapply, idempotent)
  reg_output <- split_term_column(reg_output)
  
  # Validate adjust_vars
  present_vars <- intersect(adjust_vars, unique(reg_output$variable))
  missing_vars <- setdiff(adjust_vars, present_vars)
  
  if (length(present_vars) == 0) {
    stop(glue::glue("None of the adjust_vars were found in regression output: {paste(adjust_vars, collapse = ', ')}"))
  } else if (length(missing_vars) > 0) {
    warning(glue::glue("Some adjust_vars were not found and will be ignored: {paste(missing_vars, collapse = ', ')}"))
  }
  
  # Compute per-variable alpha values
  alpha_df <- reg_output |>
    filter(variable %in% present_vars) |>
    group_by(variable) |>
    summarize(alpha = sum(.data[[coef_col]], na.rm = TRUE) / (n() + 1), .groups = "drop")
  
  # Total adjustment to apply to intercept
  total_alpha <- sum(alpha_df$alpha, na.rm = TRUE)
  
  # Join alpha values back to reg_output
  reg_output_adj <- reg_output |>
    left_join(alpha_df, by = "variable") |>
    mutate(
      !!coef_col := case_when(
        term == "(Intercept)" ~ .data[[coef_col]] + total_alpha,
        variable %in% present_vars ~ .data[[coef_col]] - alpha,
        TRUE ~ .data[[coef_col]]
      )
    ) |>
    select(-alpha)  # Clean up
  
  return(reg_output_adj)
}


# The purpose of this function is to "complete" a regression by adding any implicit 
# 0 coefficients.
# For example, suppose we have a regression (with saddening results) of this type:
# wages = 1 + 1(is_male)
# That can equally be expressed as 
# wages = 1 + 1(is_male) + 0(is_female)
# Q: Isn't this overdetermined? Why would we want this?
# A: so that we can more explicitly apply the gardeazabal-ugidos adjustment, and 
# express all coefficients relative to the total mean. This allows us to decompose
# the contribution of each component in a way that is invariable to the omitted
# category.
# e.g. after applying G-U, the equation above becomes, equivalently:
# wages = 0.5 + 0.25(is_male) - 0.25(is_female)
complete_implicit_zeros <- function(
    reg_output,
    adjust_by = list(
      RACE_ETH_bucket = c("AAPI", "AIAN", "Black", "Hispanic", "Multiracial", "Other", "White")
    ),
    coef_col = "estimate"
) {
  # get a varnames dict to use in split_term_column
  varnames_dict = names(adjust_by)
  
  # Step 1: Ensure 'variable' and 'value' columns exist
  reg_output <- split_term_column(reg_output, varnames = varnames_dict)  # assumes this parses 'term' into 'variable' and 'value'
  
  # Step 2: Get variables that are actually present in the regression output
  present_vars <- intersect(names(adjust_by), unique(reg_output$variable))
  
  # Step 3: For each present var, check for exactly one missing value and build a row
  missing_rows <- purrr::map_dfr(present_vars, function(var) {
    observed_values <- reg_output |> filter(variable == var) |> pull(value)
    expected_values <- adjust_by[[var]]
    missing_value <- setdiff(expected_values, observed_values)
    
    if (length(missing_value) == 0) {
      warning(glue::glue(
        "No levels were missing for variable '{var}' — nothing added."))
      return(tibble::tibble())  # return empty tibble
    }
    
    if (length(missing_value) > 1) {
      stop(glue::glue(
        "For variable '{var}', expected exactly one missing level, but found {length(missing_value)}: {paste(missing_value, collapse = ', ')}."))
    }
    
    # Return the missing row with a 0 coefficient
    tibble::tibble(
      term = paste0(var, missing_value),
      variable = var,
      value = missing_value,
      !!coef_col := 0
    )
  })
  
  # Step 4: Add the missing rows to reg_output and return
  reg_output_full <- bind_rows(reg_output, missing_rows)
  
  return(reg_output_full)
}
