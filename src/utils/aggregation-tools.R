weighted_mean <- function(
    data, 
    value_column, 
    weight_column, 
    group_by_columns) {
  # Use quasiquotation to handle column names passed as strings
  value_col <- sym(value_column)
  weight_col <- sym(weight_column)
  
  # Dynamically reference grouping columns
  group_by_cols <- syms(group_by_columns)
  
  # Calculate the weighted mean, sum of weights, and count of observations
  data %>%
    group_by(!!!group_by_cols) |>
    summarize(
      total_value_weighted = sum(!!value_col * !!weight_col, na.rm = TRUE),
      sum_weights = sum(!!weight_col, na.rm = TRUE),
      count = n()
      # Add weighted variance. Potential resources:
      # https://stats.stackexchange.com/questions/51442/weighted-variance-one-more-time
      # https://influentialpoints.com/Training/two-sample_t-test-principles-properties-assumptions.htm
    ) |>
    mutate(weighted_mean = total_value_weighted / sum_weights) |>
    select(!!!group_by_cols, count, sum_weights, weighted_mean)
}




# TODO: genericize this function further to difference any two data sources, without
# enforcing the _2000 and _2020 suffixes.
difference_means <- function(
    data2000, 
    data2020,
    match_by,   # The column to match the data from both years along
    diff_by,    # The column along which to calculate differences
    keep = NULL # The columns to keep in addition to the diff_by column
) {
  
  suffixed_cols <- c(diff_by, keep)
  
  # Rename columns with their year suffixes; Keep only `match_by` and suffixed columns
  data2000 <- data2000 |>
    select({{ match_by }}, all_of(suffixed_cols)) |>
    rename_with(~ paste0(., "_2000"), all_of(suffixed_cols))
  
  # Rename columns with their year suffixes; Keep only `match_by` and suffixed columns
  data2020 <- data2020 |>
    select({{ match_by }}, all_of(suffixed_cols)) |>
    rename_with(~ paste0(., "_2020"), all_of(suffixed_cols))
  
  # Merge data2000 and data2020 by the matching column
  diff <- data2000 |>
    inner_join(data2020, by = as_string(ensym(match_by))) |>
    mutate(
      diff = !!sym(paste0(diff_by, "_2020")) - !!sym(paste0(diff_by, "_2000"))
    ) |>
    # Arrange column order
    select(
      {{ match_by }},
      diff,
      everything()
    )
  
  return(diff)
}



# TODO: Unit test!!!
# Function to compute weighted household size (or bedroom size) by designated group_by category
# in designated year
tabulate_summary <- function(
    data, 
    year = 2000,
    value = "NUMPREC", # Could also be `persons_per_bedroom`
    group_by = NULL,  # Supports only NULL or a single string (e.g., "SEX")
    group_encoding = NULL # Optional: a named vector for mapping values (e.g., c("1" = "Male", "2" = "Female"))
) {
  # Filter data for the specified year and not living in group quarters
  data_filtered <- data |> filter(YEAR == year, GQ %in% c(0,1,2))
  
  # Compute weighted mean household size by the specified group
  result <- crosstab_mean(
    data = data_filtered,
    value = value,
    wt_col = "PERWT",
    group_by = group_by,
    every_combo = TRUE # Necessary to ensure all tables with the same inputs produce the same rows
  ) 
  
  # Add the "subgroup" column
  if (is.null(group_by)) { 
    # If no group_by string is given, add a column called "subgroup" and title the one
    # row entry "overall"
    result <- result |> mutate(subgroup = "overall")
  } else {
    # If group_by string is given, rename the output column with a more generic name of
    # "subgroup"
    result <- result |> rename(subgroup = all_of(group_by))
    
    # Apply factor encoding if provided
    if (!is.null(group_encoding)) {
      result <- result |> 
        mutate(subgroup = recode(subgroup, !!!group_encoding)) |>  # Rename values
        mutate(subgroup = factor(subgroup, levels = group_encoding)) |> # Ensure correct order
        arrange(subgroup)
    }
  }
  
  # Rename the weighted_mean column
  if (value == "NUMPREC") {
    result <- result |> rename(hhsize = weighted_mean)
  } else if (value == "persons_per_bedroom") {
    result <- result |> rename(ppbedroom = weighted_mean)
  } else {
    stop("`value` argument must either be \"NUMPREC\" or \"persons_per_bedroom\"")
  }
  
  # Keep only needed columns, drop the rest
  result <- result |> select(any_of(c("subgroup", "hhsize", "ppbedroom")))
  
  return(result)
}

# Wrapper function to tabulate summary of 2 years
tabulate_summary_2year <- function(
    data, 
    years = c(2000, 2019),
    value = "NUMPREC", # could also be `persons_per_bedroom`
    group_by = NULL, # For now, only NULL or one string (e.g. "SEX") are supported. No multi-string vectors
    group_encoding = NULL # Optional encoding of factor labels for group_by variable. E.g. if
    # group_by = "SEX", you may input 1 = "Male", 2 = "Female"
) {
  # Extract years dynamically
  year1 <- years[1]
  year2 <- years[2]
  
  # Compute summaries for both years
  year1_table <- tabulate_summary(data, year1, value, group_by, group_encoding)
  year2_table <- tabulate_summary(data, year2, value, group_by, group_encoding)
  
  # Merge results, add _year suffixes to analogous columns
  combined_table <- left_join(year1_table, year2_table, by = "subgroup", suffix = paste0("_", years))
  
  # Compute percent changes dynamically
  for (var in intersect(names(year1_table), names(year2_table))) {
    if (var != "subgroup") { # Ensure we don't try to mutate 'subgroup'
      col1 <- paste0(var, "_", year1)
      col2 <- paste0(var, "_", year2)
      pct_col <- paste0(var, "_pctchg_", year1, "_", year2)
      
      combined_table <- combined_table |> 
        mutate(!!pct_col := (!!sym(col2) - !!sym(col1)) / !!sym(col1) * 100)
    }
  }
  
  return(combined_table)
}


# # Example usage of tabulate_summary()
# tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket")
# tabulate_summary(data = ipums_db, year = 2000, group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
# tabulate_summary(data = ipums_db, year = 2000, group_by = c())
# #tabulate_summary(data = ipums_db, year = 2000, value = "persons_per_bedroom", group_by = c())
# 
# # Example usage of tabulate_summary_2year()
# tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "RACE_ETH_bucket")
# tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
# tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())