# src/scripts/fast-facts.R
# The purpose of this script is to derive and document single statistics used
# in the manuscript, such as "X% of the population was Hispanic in 2000", etc.
# To my best ability, I'll try to indicate the sentence the fact belongs in,
# although the manuscript is subject to change.

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("readxl")
library("ggplot2")
library("base64enc")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3: Define functions for tabulating summaries ----- #
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


# Example usage of tabulate_summary()
tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket")
tabulate_summary(data = ipums_db, year = 2000, group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
tabulate_summary(data = ipums_db, year = 2000, group_by = c())
#tabulate_summary(data = ipums_db, year = 2000, value = "persons_per_bedroom", group_by = c())

# Example usage of tabulate_summary_2year()
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "RACE_ETH_bucket")
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "SEX", group_encoding = c("1" = "Male", "2" = "Female"))
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())

# ----- Step 4: Get those facts! ----- #
# Average household size also varies significantly by ownership status, a gap that 
# has ____ over time. In the year 2000, average household size for individuals living 
# in rental housing was ____, while the equivalent figure for homeowners was _____. 
# By 2019, average household size was ___ for renters and ____ for homeowners.
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "tenure")

# Between 2000 and 2019, American households not only grew smaller, they also became 
# less crowded. The average American household in 2000 had ____ people per bedroom, 
# and ___% exceeded a threshold of two people per bedroom. By 2019, this had fallen 
# to an average of _____ people per bedroom—a decline of __%—and ___% of households 
# with more than two people per bedroom. As we demonstrate in Figure 5, this decrease 
# in crowding held true for members of all racial/ethnic groups and for renters 
# as well as homeowners, although renters consistently live in more-crowded housing 
# than homeowners. The largest declines were among _____ individuals living in ______ 
# homes.
tabulate_summary_2year(
  data = ipums_db, 
  years = c(2000,2019), 
  group_by = NULL, 
  value = "persons_per_bedroom"
  )

#####
# As documented above, average household size fell 2.7% over this period (from 3.47 
# to 3.37 people). Simultaneously, the average person lived in a housing unit that 
# increased in size from ____ bedrooms in 2000 to ___ bedrooms in 2019, an increase 
# of ___%. In the aggregate, these average household sizes and bedroom counts indicate 
# average crowding of ____ people per bedroom in 2000 and ____ in 2019. (As expected, 
# these figures differ from the crowding levels reported above, which were calculated 
# at the individual level.) 
aggregates <- readRDS("throughput/aggregates.rds")

# bedrooms
mean_2000 <- aggregates |> filter(variable == "bedroom") |> pull(mean_2000)
mean_2019 <- aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
(mean_2019 / mean_2000 - 1) * 100

#####
# Over the same period, average bedroom counts rose from 2.82_____ to 3.01_____. 
# Had 2000 preferences remained stable and been applied to the 2019 population 
# structure, we would have expected an average of 2.78_____ bedrooms. That is, 
# observed 2019 average bedroom counts are ___% larger than expected. Likewise, 
# we find that crowding fell from ___ to ___ persons per bedroom, but that in the 
# counterfactual scenario it would have fallen to just _____ persons per bedroom.
# Note to self: run the kob-control-script: fig06_data contains this info.
# TODO: somehow save the fig06_data and make this less brittle. Perhaps put into 
# a counterfactuals script or something that is saved, later
# TODO: !!!!!!!!!!! FIX BY SAVING FIG06_data!!!!
# o_2000 <- fig06_data |> filter(variable == "bedroom") |> pull(observed_2000)
# o_2000
# o_2019 <- fig06_data |> filter(variable == "bedroom") |> pull(observed_2019)
# o_2019
# e_2019 <- fig06_data |> filter(variable == "bedroom") |> pull(expected_2019)
# e_2019
# ( o_2019 / e_2019 - 1) * 100
# 
# o_2000 <- fig06_data |> filter(variable == "persons_per_bedroom") |> pull(observed_2000)
# o_2000
# o_2019 <- fig06_data |> filter(variable == "persons_per_bedroom") |> pull(observed_2019)
# o_2019
# e_2019 <- fig06_data |> filter(variable == "persons_per_bedroom") |> pull(expected_2019)
# e_2019
# ( o_2019 / e_2019 - 1) * 100

# As noted above, the average American household size in 2000 was 3.47 people, a 
# figure that fell to 3.37 people in 2019. Had preferences remained stable from 
# 2000 onwards, we would expect that average household size in 2019 would have 
# fallen to 3.33456 people. In other words, average household size fell by 0.093 p
# eople between 2000 and 2019, which amounts to  83% of the expected decline based 
# on changes  in population structure (0.112 people). This gap suggests that unobserved 
# factors partially offset the demographic forces driving household size decline. 
# TODO: !!!!!!!!!!! FIX BY SAVING FIG06_data!!!!
# o_2000 <- fig06_data |> filter(variable == "NUMPREC") |> pull(observed_2000)
# o_2000
# o_2019 <- fig06_data |> filter(variable == "NUMPREC") |> pull(observed_2019)
# o_2019
# e_2019 <- fig06_data |> filter(variable == "NUMPREC") |> pull(expected_2019)
# e_2019
# o_2000 - o_2019
# o_2000 - e_2019
# (o_2000 - o_2019)/(o_2000 - e_2019)

# In Figure 2 we plot the shifting distribution of household size for all Americans 
# (top-left panel) and for individuals in the three largest racial/ethnic groups 
# (white, Hispanic, and Black, which jointly make up ____% of the population in 2019).
fig02_data <- readRDS("output/figures/fig02-data.rds")
# Total number of measured people not in GQs in 2019
pop_2019 <- sum(fig02_data |> filter(RACE_ETH_bucket == "All" & year == "2019") |> pull(weighted_count))
pop_2019
# Sanity check: this should also equal total number of measured people not in GQs in 2019
sum(fig02_data |> filter(RACE_ETH_bucket != "All" & year == "2019") |> pull(weighted_count))
# Total number of Black + Hispanic + white
pop_white_2019 <- sum(fig02_data |> filter(RACE_ETH_bucket == "White" & year == "2019") |> pull(weighted_count))
pop_black_2019 <- sum(fig02_data |> filter(RACE_ETH_bucket == "Black" & year == "2019") |> pull(weighted_count))
pop_hispanic_2019 <- sum(fig02_data |> filter(RACE_ETH_bucket == "Hispanic" & year == "2019") |> pull(weighted_count))
# Fraction Black + Hispanic + white
(pop_white_2019 + pop_black_2019 + pop_hispanic_2019) / pop_2019

# In both 2000 and 2019, the most-common household size for Americans was ____ individuals, 
# an arrangement that held for ___% of individuals in 2000 and ___% in 2019. Fully ___% 
# of all households had four or fewer individuals in 2000 (___% in 2019), and only __% 
# had six or more (___% in 2019). Between 2000 and 2019, the share of individuals living 
# in the smallest households increased, with ________

