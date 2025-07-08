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

  # Calculate u (if applicable)
  if ("(Intercept)" %in% kob_input$term) {
    intercept_2000 <- kob_input |> filter(term == "(Intercept)") |> pull(coef_2000)
    intercept_2019 <- kob_input |> filter(term == "(Intercept)") |> pull(coef_2019)
    
    u <- intercept_2019 - intercept_2000
    # TODO: fill in with actual SE formula
    se_u <- 2
    
  } else {
    
    u <- NULL
    se_u <- NULL
    
  }
  
  return(list(u, se_u))
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
  # 
  # return(output)
}

kob_input <- readRDS("throughput/kob_input.rds")

kob(kob_input$bedroom)