# counterfactual-multiscenario.R
#
# The purpose of this script is to calculate what - after controlling for demographic 
# factors - average person-level household size would be in 2019 compared to 2000 
# values. This is the newer, updated version of src/counterfactual-density.R, which
# will soon be deprecated.
# 
# The script is closely related to counterfactual-regional.R. This script produces
# the data underlying:
# - Table 3.1
# - Table 3.2
# in the Shiny app, while counterfactual-regional.R produces the data underlying
# Tables 3.3 and 3.4 in the shiny app.
#
# Rather than using all the demographic features at once, it layers them on one at
# a time. This allows us to conclude what happens with the introduction of each 
# individual layer and how robust the results are to various controls.
#
# These layered results are saved in .rda files that are placed in the shiny-app/data
# directory.
#
# Inputs:
#   - data/db/ipums.duckdb
#   - draws from function defined in src/utils/counterfactual-tools.R
# Outputs:
#   - shiny-app/data/counterfactuals.rda 
# TODO: Do a shiny-app wide audit of data names: right now, the RDA files are
# sillily named and it's basically the Wild West out here.
# 
# TODO: step 2, 2a, and 2b are identical between this script and counterfactual-regional.R.
# Figure out how potentially to move this data wrangling upstream.
#
# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# TODO: eventually write a function in dataduck that when buckets are created,
# the code automatically writes a list of vectors containing factor
# labels. For now, I'm just generating factor labels directly from the lookup
# table here, but this code is more brittle since it relies on me remembering
# which lookup table I used.
age_factor_levels <- extract_factor_label(
  lookup_table = read.csv("lookup_tables/age/age_buckets01.csv"),
  colname = "bucket_name"
)

# ----- Step 3: Run counterfactuals ----- #
# These results are used to produce tables 3.1 and 3.2 (in tab 3 of the shiny app)
# relies on the calculate-counterfactual function, which is loaded in the 
# src/utils/counterfactual-tools.R

# List of scenarios
scenarios <- list(
  c("AGE_bucket"),
  c("SEX"),
  c("us_born"),
  c("EDUC_bucket"),
  c("INCTOT_cpiu_2010_bucket"),
  c("OWNERSHP"),
  c("CPUMA0010"),
  c("RACE_ETH_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP"),
  c("RACE_ETH_bucket", "AGE_bucket", "SEX", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "OWNERSHP", "CPUMA0010")
)

# Generate data for all scenarios
nrow_pull <- 10000000
p0_sample <- ipums_db |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()
p1_sample <- ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) # |> head(nrow_pull) |> collect()

# Persons per bedroom
bedroom_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019,
    p0_data = p0_sample,
    p1_data = p1_sample,
    outcome = "persons_per_bedroom"
    )$summary # Extract only the summary tibble
  )
)

# TODO: add a check in the function calculate_counterfactual that tests whether
# all of the potential categories are populated by data?
# motivated by my observation that smaller samples (e.g. 10,000) don't capture all
# CPUMA0010s, resulting in NA estimates.
# Persons per household
hhsize_cf <- bind_rows(
  lapply(scenarios, function(cf) calculate_counterfactual(
    cf_categories = cf, 
    p0 = 2000, 
    p1 = 2019, 
    p0_data = p0_sample, 
    p1_data = p1_sample,
    outcome = "NUMPREC"
    )$summary # Extract only the summary tibble
  )
)

##############
#GENERATE FIGUURE 3

# Average household size in 2000
hhsize_2000_observed <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2000, GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE) |>
  pull(weighted_mean)

# Average household size in 2019
hhsize_2019_observed <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE) |>
  pull(weighted_mean)

# Counterfactual household size in 2019
# TODO: Don't rely on index numbers!!!!! 15, is buggy and error prone
hhsize_2019_expected <- hhsize_cf[15,] |> pull(counterfactual) 

library(ggplot2)
library(dplyr)

# Data frame for plotting
fig03_data <- tibble(
  Category = factor(c("2000\nObserved", "2019\nObserved", "2019\nCounterfactual"), 
                    levels = c("2000\nObserved", "2019\nObserved", "2019\nCounterfactual")),
  Household_Size = c(hhsize_2000_observed, hhsize_2019_observed, hhsize_2019_expected),
  Type = c("Observed", "Observed", "Counterfactual") # Differentiate for styling
)

# Compute differences for brackets
diff_2000_2019 <- hhsize_2000_observed - hhsize_2019_observed
diff_2019_obs_exp <- hhsize_2019_observed - hhsize_2019_expected

# Adjustable settings
bracket_offset <- 0.005         # Clearance above shorter bar
difference_text_offset <- 0.1   # Horizontal offset for difference text labels

# Create the bar plot
fig03 <- ggplot(fig03_data, aes(x = Category, y = Household_Size, fill = Type)) +
  geom_bar(stat = "identity", aes(linetype = Type), color = "black", linewidth = 0.4, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
  scale_fill_manual(values = c("Observed" = "steelblue", "Counterfactual" = scales::alpha("steelblue", 0.5))) +
  scale_linetype_manual(values = c("Observed" = "solid", "Counterfactual" = "dotted")) +
  labs(y = "Household Size", x = NULL, 
       caption = "Note: Values may not sum exactly due to rounding.") +
  coord_cartesian(ylim = c(3, 3.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9) # Align caption to the left
  ) +
  
  # First I-shaped segment (2000 vs 2019 Observed)
  annotate("segment", x = 2, xend = 2, 
           y = hhsize_2019_observed + bracket_offset, 
           yend = hhsize_2000_observed, color = "black") +  # Vertical part
  annotate("segment", x = 1.9, xend = 2.1, 
           y = hhsize_2000_observed, 
           yend = hhsize_2000_observed, color = "black") +  # Top horizontal tick
  annotate("segment", x = 1.9, xend = 2.1, 
           y = hhsize_2019_observed + bracket_offset, 
           yend = hhsize_2019_observed + bracket_offset, color = "black") +
  annotate("text", x = 2 + difference_text_offset, y = (hhsize_2000_observed + hhsize_2019_observed) / 2, 
           label = sprintf("-%.3f", diff_2000_2019), size = 4, hjust = 0) +
  
  # Second I-shaped segment (2019 Observed vs 2019 Counterfactual)
  annotate("segment", x = 3, xend = 3, 
           y = hhsize_2019_expected + bracket_offset, 
           yend = hhsize_2019_observed, color = "black") +  # Vertical part
  annotate("segment", x = 2.9, xend = 3.1, 
           y = hhsize_2019_observed, 
           yend = hhsize_2019_observed, color = "black") +  # Top horizontal tick
  annotate("segment", x = 2.9, xend = 3.1, 
           y = hhsize_2019_expected + bracket_offset, 
           yend = hhsize_2019_expected + bracket_offset, color = "black") +
  annotate("text", x = 3 + difference_text_offset, y = (hhsize_2019_observed + hhsize_2019_expected) / 2, 
           label = sprintf("-%.3f", diff_2019_obs_exp), size = 4, hjust = 0)

# Display the plot
fig03

# Save the plot
ggsave("results/fig03.png", plot = fig03, width = 6.5, height = 5.5, dpi = 500)


##### Variation Fig 3a without the I-bar differences
# Create the bar plot (without I-segments or differences)
fig3a <- ggplot(fig03_data, aes(x = Category, y = Household_Size, fill = Type)) +
  geom_bar(stat = "identity", aes(linetype = Type), color = "black", linewidth = 0.4, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
  scale_fill_manual(values = c("Observed" = "steelblue", "Counterfactual" = scales::alpha("steelblue", 0.5))) +
  scale_linetype_manual(values = c("Observed" = "solid", "Counterfactual" = "dotted")) +
  labs(y = "Household Size", x = NULL) +
  coord_cartesian(ylim = c(3, 3.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9) # Align caption to the left
  )

# Save the plot
ggsave("results/fig3a.png", plot = fig3a, width = 6.5, height = 5.5, dpi = 500)


#----- Step 4: Save the results to the Shiny app ----- #
# Counterfactuals
save(
  hhsize_cf, 
  bedroom_cf,
  file = "shiny-app/data/counterfactuals.rda"
)
