# kob-function.R
# TODO: this function will need to have standard errors built in as a functionality.
# It should also include basic checks on the validity of the coef input.
# also, user should be able to specify through arguments which cols indicate the 
# beta coefficients (to user Blinder's terminiology) and X coefficients.
# This should be detailed clearly in the documetnation ,with latex-formatted 
# terminology.

kob <- function(
    kob_input # A data frame of the style outputted by running src/kob/kob-prepare-data.R above
) {
  #--- 0: Input checks
  # Check for presence of required columns
  required_cols <- c("term", "coef_2000", "coef_2019", "prop_2000", "prop_2019")
  missing_cols <- setdiff(required_cols, names(kob_input))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for multiple intercepts
  intercept_count <- sum(kob_input$term == "(Intercept)")
  if (intercept_count > 1) {
    stop("❌ Multiple '(Intercept)' rows detected. Only one is allowed.")
  }
  
  # --- 1: Initialize output
  # Output is the kob_input data frame, plus six additional columns: 
  # u, u_se, e, e_se, c, c_se
  kob_output <- kob_input |>
    mutate(
      u = NA_real_,
      u_se = NA_real_,
      e = NA_real_,
      e_se = NA_real_,
      c = NA_real_,
      c_se = NA_real_
    )
  
  # --- 2a: Calculate u
  # Message user about intercept presence
  if (intercept_count == 1) {
    message("✅ Intercept detected. u term will be calculated in output.")
  } else {
    message("ℹ️ Intercept not detected. u term will not be calculated in output.")
  }
  
  # Calculate u if (Intercept) row exists
  if ("(Intercept)" %in% kob_input$term) {
    intercept_2000 <- kob_input |> filter(term == "(Intercept)") |> pull(coef_2000)
    intercept_2019 <- kob_input |> filter(term == "(Intercept)") |> pull(coef_2019)

    u_val <- intercept_2019 - intercept_2000
    u_se_val <- 2  # placeholder

    kob_output <- kob_output |>
      mutate(
        # Update the u col so only the term in the "(Intercept)" row has a numeric
        u = case_when(
          term == "(Intercept)" ~ u_val, 
          TRUE ~ u),
        # Update the u_se col accordingly
        u_se = case_when(
          term == "(Intercept)" ~ u_se_val, 
          TRUE ~ u_se)
      )
  }

  # --- 2b: Calculate e
  kob_output <- kob_output |>
    mutate(
      e = coef_2019 * (prop_2019 - prop_2000),
      e_se = 2 # placeholder
    )
  
  # --- 2c: Calculate c
  kob_output <- kob_output |>
    mutate(
      c = (coef_2019 - coef_2000) * prop_2000,
      c_se = 2 # placeholder
    )
  
  # --- 3: Return
  return(kob_output)
}


kob_input <- readRDS("throughput/kob_input.rds")

kob_output <- kob(kob_input$bedroom)

# Time to validate
aggregates <- readRDS("throughput/aggregates.rds")

# Takes in an object of type kob_output, plus the outcome mean in 2000 and 2019
# to determine whether the kob matches the expected tolerance.
kob_output_validate <- function(
    kob_output,
    mean_2000,
    mean_2019,
    tol = 1e-10
    ) {
  actual <- kob_output |>
    # Sum up u, e, and c in each row
    mutate(x = rowSums(across(c(u, e, c)), na.rm = TRUE)) |>
    # Sum the resultant column
    summarize(actual = sum(x)) |>
    pull(actual)
  
  expected <- mean_2019 - mean_2000
  difference <- abs(actual - expected)
  
  if (difference <= tol) {
    message(glue::glue("✅ Decomposition matches expected total within {tol} tolerance."))
    return(TRUE)
  } else {
    stop(glue::glue(
      "❌ Decomposition mismatch:\
      actual   = {round(actual, 6)}
      expected = {round(expected, 6)}
      diff     = {round(difference, 6)}"))
  }
}

kob_output_validate(
  kob_output = kob_output,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)