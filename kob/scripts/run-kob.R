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
  # Initialize the output. It is comprised of the input plus six additional added
  # columns: u, u_se, e, e_se, c, c_se
  kob_output <- kob_input |>
    mutate(
      u = NA_real_,
      u_se = NA_real_,
      e = NA_real_,
      e_se = NA_real_,
      c = NA_real_,
      c_se = NA_real_
    )
  
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
  
  return(kob_output)
  
  # Calculate e
  
  
  # # Calculate e and c components
  # coef <- coef |>
  #   mutate(
  #     e_component = mean_2019*(prop_2019 - prop_2000),
  #     c_component = (mean_2019 - mean_2000)*prop_2000
  #   )
  # 
  # # Calculate aggregate u, e, and c components
  # u <- intercept_2019 - intercept_2000
  # e <- sum(coef$e_component, na.rm = TRUE)
  # c <- sum(coef$c_component, na.rm = TRUE)
  # 
  # # Construct output list
  # output <- list(
  #   components = list(u = u, e = e, c = c),
  #   coef = coef
  #   )
}


kob_input <- readRDS("throughput/kob_input.rds")

kob(kob_input$bedroom)