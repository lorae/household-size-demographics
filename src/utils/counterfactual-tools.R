# First step of calculating the counterfactual
counterfactual_components <- function(
    cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
    p0 = 2000, # An integer for the year of the first (base) period
    p1 = 2019, # An integer for the year of the second (recent) period
    p0_data, # Data for period 0
    p1_data, # Data for period 1
    outcome = "NUMPREC" # Name of the outcome variable.
    # TODO: add back standard errors later. Not needed for now.
) {
  # TODO: add a step that catches errors if the specified data set is empty.
  # Note that this should be done at this level, but I'm also surprised the crosstab_mean
  # and crosstab_percent functions aren't producing errors when I do this.
  
  # Calculate the mean outcome in periods 0 and 1 of the data
  print(glue("Calculating {p0} means..."))
  mean_p0 <- crosstab_mean(
    data = p0_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p0} means done!"))
  
  print(glue("Calculating {p1} means..."))
  mean_p1 <- crosstab_mean(
    data = p1_data,
    value = outcome,
    wt_col = "PERWT",
    group_by = cf_categories,
    every_combo = TRUE
  )
  print(glue("{p1} means done!"))
  
  # Calculate the population composition in period 0
  # Note: not strictly needed for counterfactual calculation, but useful context
  print(glue("Calculating {p0} percents..."))
  percent_p0 <- crosstab_percent(
    data = p0_data,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    every_combo = TRUE
  ) |>
    select(-weighted_count, -count)
  print(glue("{p0} percents done!"))
  
  # Calculate the population composition in period 1
  print(glue("Calculating {p1} percents..."))
  percent_p1 <- crosstab_percent(
    data = p1_data,
    wt_col = "PERWT",
    group_by = cf_categories,
    percent_group_by = c(),
    every_combo = TRUE
  ) |>
    select(-weighted_count, -count)
  print(glue("{p1} percents done!"))
  
  # Combine means from periods 0 and 1 with population composition from period 1
  crosstab_p0_p1 <- 
    full_join(
      # Join the p0 and p1 mean data frames, appending a _{year} suffix to columns
      mean_p0 |> rename_with(~paste0(., "_", p0), -all_of(cf_categories)),
      mean_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      # Join with p1 percent data frame, appending _{year} suffixes
      percent_p1 |> rename_with(~paste0(., "_", p1), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    full_join(
      # Join with p0 percent data frame, appending _{year} suffixes
      percent_p0 |> rename_with(~paste0(., "_", p0), -all_of(cf_categories)),
      by = setNames(cf_categories, cf_categories)
    ) |>
    mutate(
      # Apply rules for handling NA values
      across(
        all_of(paste0("weighted_mean_", p1)), 
        ~replace_na(., 0),
        .names = "{.col}"
      ),
      across(
        all_of(paste0("weighted_mean_", p0)),
        ~ifelse(is.na(.), .data[[paste0("weighted_mean_", p1)]], .),
        .names = "{.col}"
      )
    ) |>
    arrange(across(all_of(cf_categories))) |>
    mutate(
      # Diff = period 1 household size minus period 0 household size
      diff = .data[[paste0("weighted_mean_", p1)]] - .data[[paste0("weighted_mean_", p0)]],
      # Calculate contributions (See Shiny app, main tab, for more explanation on contributions).
      cont_p1 = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p1)]] / 100,
      cont_p1_cf = .data[[paste0("percent_", p1)]] * .data[[paste0("weighted_mean_", p0)]] / 100,
      contribution_diff = cont_p1 - cont_p1_cf,
    ) |>
    select(any_of(c(
      # Reorder columns more intuitively
      cf_categories, 
      # paste0("count_", p0), paste0("count_", p1), # We don't need the counts. meh
      paste0("weighted_count_", p0), paste0("weighted_count_", p1),
      paste0("percent_", p0), paste0("percent_", p1),
      paste0("weighted_mean_", p0), paste0("weighted_mean_", p1),
      "diff", 
      "contribution_diff"
    )))
  
  return(crosstab_p0_p1)
}

summarize_counterfactual <- function(
    cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
    counterfactual, # an object returned by counterfactual_components()
    p0 = 2000,
    p1 = 2019
) {
  # Calculate overall values
  actual_outcome_p1 <- counterfactual |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p1)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  cf_outcome_p1 <- counterfactual |>
    summarize(total = sum(.data[[paste0("weighted_mean_", p0)]] * .data[[paste0("percent_", p1)]] / 100)) |>
    pull(total)
  
  # TODO: Make this less brittle! Why are the varnames hard-coded in?
  return(tibble(
      RACE_ETH_bucket = ("RACE_ETH_bucket" %in% cf_categories),
      AGE_bucket = ("AGE_bucket" %in% cf_categories),
      SEX = ("SEX" %in% cf_categories),
      us_born = ("us_born" %in% cf_categories),
      EDUC_bucket = ("EDUC_bucket" %in% cf_categories),
      INCTOT_cpiu_2010_bucket = ("INCTOT_cpiu_2010_bucket" %in% cf_categories),
      OWNERSHP = ("OWNERSHP" %in% cf_categories),
      CPUMA0010 = ("CPUMA0010" %in% cf_categories),
      counterfactual = cf_outcome_p1,
      actual = actual_outcome_p1,
      diff = actual_outcome_p1 - cf_outcome_p1
    ))
}

# This function takes data and metadata about desired counterfactual simulations
# as an input. As an output, it produces a list with two elements. The first output
# is a tibble with a single row summarizing the variables controlled for, the 
# counterfactual output, and the actual output. The second output is a crosstab 
# table that shows the exact contribution from each component into the summary statistics.

# TODO: This function needs to be modularized and unit-tested.
calculate_counterfactual <- function(
    cf_categories = c("AGE_bucket", "RACE_ETH_bucket"), # A vector of string names for the group_by variable 
    p0 = 2000, # An integer for the year of the first (base) period
    p1 = 2019, # An integer for the year of the second (recent) period
    p0_data, # Data for period 0
    p1_data, # Data for period 1
    outcome = "NUMPREC" # Name of the outcome variable.
    # TODO: add back standard errors later. Not needed for now.
) {
  # Combine means from periods 0 and 1 with population composition from period 1
  crosstab_p0_p1 <- counterfactual_components(
    cf_categories = cf_categories,
    p0 = p0,
    p1 = p1,
    p0_data = p0_data,
    p1_data = p1_data,
    outcome = outcome
  )
  

}