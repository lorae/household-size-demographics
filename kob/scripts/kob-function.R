# temporal-kob.R
# apply learnings from test-temporal-kob.r to produce the KOB decomposition

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")
library("oaxaca")
library("tibble")

# ----- Step 1: Load in data from kob-prepare-data.R ----- #
source("kob/scripts/kob-prepare-data.R")

# coef is the key output here

# ----- Step 2: Run the kob analysis ----- #
# This is for validation. The difference in mean hhsize from 2000 to 2019 should
# exactly equal the sum of the u, c, and e components.
lm(data = ipums_tb |> filter(YEAR == 2000, GQ %in% c(0, 1, 2)), 
   formula = NUMPREC ~ 1,
   weights = PERWT)$coefficients -> mean_hhsize_2000
lm(data = ipums_tb |> filter(YEAR == 2019, GQ %in% c(0, 1, 2)), 
   formula = NUMPREC ~ 1,
   weights = PERWT)$coefficients -> mean_hhsize_2019

validation_diff <- mean_hhsize_2019 - mean_hhsize_2000
# ---- 

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

# Apply the kob function
kob_output <- kob(coef)
kob_output

# Validate whether the sum of the outputs matches the validation diff
output_diff <- kob_output$components |>
  unlist() |>
  sum()

# No output means the test passed (i.e. kob worked!)
testthat::expect_equal(output_diff, as.numeric(validation_diff), tolerance = 1e-8)


# ----- Step 3: Save and graph ----- #

write.csv(coef, "results/coef2.csv")

# Filter out rows with NA c_component and create label
coef_clean <- coef |>
  filter(!is.na(c_component)) |>
  mutate(label = name)

# Plot
ggplot(coef_clean, aes(x = reorder(label, c_component), y = c_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Coefficients (c_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()

ggplot(coef_clean, aes(x = reorder(label, e_component), y = e_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Endowments (e_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()