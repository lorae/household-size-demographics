# kob-function.R
# TODO: this function will need to have standard errors built in as a functionality.
# It should also include basic checks on the validity of the coef input.
# also, user should be able to specify through arguments which cols indicate the 
# beta coefficients (to user Blinder's terminiology) and X coefficients.
# This should be detailed clearly in the documetnation ,with latex-formatted 
# terminology.

kob <- function(
    coef # A data frame of the style outputted by running src/kob/kob-prepare-data.R above
) {
  # Initialize intercept values
  intercept_2000 <- coef |> filter(name == "(Intercept)") |> pull(mean_2000)
  intercept_2019 <- coef |> filter(name == "(Intercept)") |> pull(mean_2019)
  
  # Calculate e and c components
  coef <- coef |>
    mutate(
      e_component = mean_2019*(prop_2019 - prop_2000),
      c_component = (mean_2019 - mean_2000)*prop_2000
    )
  
  # Calculate aggregate u, e, and c components
  u <- intercept_2019 - intercept_2000
  e <- sum(coef$e_component, na.rm = TRUE)
  c <- sum(coef$c_component, na.rm = TRUE)
  
  # Construct output list
  output <- list(
    components = list(u = u, e = e, c = c),
    coef = coef
    )
  
  return(output)
}
