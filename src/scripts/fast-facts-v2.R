# The purpose of this script is to produce fast facts that are used in the draft 
# version of this paper. 
# Last modified mid-March 2025.

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
library("patchwork")
library("sf")

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


# ----- Step 4a: RESULTS - Household size in 2000 and 2019 ----- #
# TODO: export as table to Shiny app
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = c())

# ----- Step 4b: RESULTS - Household size in 2000 and 2019 by race/ethnicity ----- #
# TODO: export as table to Shiny app
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "RACE_ETH_bucket")

### # FIGURE 1: Create the bar plot with side-by-side bars for 2000 and 2019
# Create data for bar plot
race_summary_2000 <- tabulate_summary(data = ipums_db, year = 2000, group_by = "RACE_ETH_bucket") |> mutate(year = 2000)
race_summary_2019 <- tabulate_summary(data = ipums_db, year = 2019, group_by = "RACE_ETH_bucket") |> mutate(year = 2019)
race_summary <- union_all(race_summary_2000, race_summary_2019) # Row bind the tables

# Define a single main color for all bars
main_color <- "steelblue"

# Generate the plot
fig01 <- ggplot(race_summary, aes(x = subgroup, y = hhsize, fill = factor(year))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.8, color = "black") +  # Bar border
  geom_text(aes(label = round(hhsize, 2), group = year), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +  
  scale_fill_manual(
    values = c("2000" = alpha(main_color, 0.4), "2019" = alpha(main_color, 0.8)), 
    name = "") +  # Ensures the legend colors match bar colors
  labs(y = "Average Household Size") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  )

# Save the plot
ggsave("results/fig01.png", plot = fig01, width = 6.5, height = 3.5, dpi = 500)

# ----- Step 4c: RESULTS - Household size in 2000 and 2019 by age ----- #
# TODO: export as table to Shiny app
age_summary <- tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "AGE")

# Ages with the largest household sizes
# TODO: highlight these rows in the Shiny app table
age_summary |> slice_max(hhsize_2000, n = 1)
age_summary |> slice_max(hhsize_2019, n = 1)

# ----- Step 4d: RESULTS - Household size in 2000 and 2019 by age bucket ----- #
# Define ordered levels for AGE_bucket
# Note, this is brittle, since age buckets may change
# TODO: repo-wide factor dictionary, updated upon initial data ingestion
age_bucket_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", 
                       "25-29", "30-34", "35-39", "40-44", "45-49", 
                       "50-54", "55-59", "60-64", "65-69", "70-74", 
                       "75-79", "80-84", "85plus")

# Create named vector for group_encoding
group_encoding_age_bucket <- setNames(age_bucket_levels, age_bucket_levels)

# Use `tabulate_summary_2year()` with group_encoding to enforce natural order of age
# buckets and generate data for figure 2
# Define race/ethnicity categories, including "All"
race_ethnicities <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")

# Generate summary for all categories in a loop (with 5 cores, takes about 10 seconds)
age_bucket_summary <- map_dfr(race_ethnicities, function(race) {
  data_filtered <- if (race == "All") ipums_db else ipums_db |> filter(RACE_ETH_bucket == race)
  
  tabulate_summary_2year(
    data = data_filtered,
    years = c(2000, 2019),
    group_by = "AGE_bucket",
    group_encoding = group_encoding_age_bucket
  ) |> mutate(RACE_ETH_bucket = race) # Add race/ethnicity label
})

### # FIGURE 2: Bar plot with percentage differences between 2000 and 2019 by age;
# faceted by race
# Graph
ggplot(age_bucket_summary |> filter(RACE_ETH_bucket == "All"), 
       aes(x = factor(subgroup), y = hhsize_pctchg_2000_2019)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Percentage Change in Household Size (2000-2019) by Age",
    x = "Age",
    y = "Percentage Change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Create the bar plot
# ggplot(age_summary_filtered, aes(x = factor(subgroup), y = hhsize_diff_2019_2000)) +
#   geom_bar(stat = "identity", fill = "darkred") +
#   labs(
#     title = "Absolute Change in Household Size (2000-2019) by Age (≤90)",
#     x = "Age",
#     y = "Household Size Change (Persons per Household)"
#   ) +
#   scale_x_discrete(breaks = seq(0, 90, by = 5)) +  # Labels only every 5th subgroup
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv# Figure 2
##############################################
# Define race order (ensuring correct factor levels for consistent ordering)
race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- age_bucket_summary %>%
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# Split data into two halves
left_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[1:4])  # First 4 races
right_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[5:8])  # Last 4 races

# Left plot (Facet titles on the LEFT)
left_plot <- ggplot(left_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019, 
                                   fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +  # Define colors
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 10),  # Labels on the left
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Hide left-side axis labels
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    
    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),  # Right-align text
    axis.ticks.y.right = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),  # Add percentage symbol
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))  # Adds tick marks with percentage on the right
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

left_plot

# Right plot (Facet titles on the RIGHT)
right_plot <- ggplot(right_data, aes(x = subgroup, 
                                     y = pmin(hhsize_pctchg_2000_2019, 14))) +  # Cap at 14% (leave room for asterisk at 14.5%)
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  
  # Add an asterisk above the capped bar at a safe y-position (14.5 instead of 16)
  geom_text(data = right_data %>% filter(RACE_ETH_bucket == "Other", subgroup == "80-84"),
            aes(x = subgroup, y = 14.5, label = "*"),
            size = 5, color = "black") +
  
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, color = "black", size = 10),  # Labels on the right
    strip.placement = "outside",  # Fixing the warning by removing strip.position
    panel.spacing = unit(0.8, "lines"),  # Slightly reduce panel spacing
    panel.spacing.y = unit(0.1, "lines"),  # Reduce vertical spacing
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Add subtle tick marks on the LEFT side without labels
    axis.text.y.left = element_blank(),  # No numeric labels
    axis.ticks.y.left = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Hide right-side axis labels
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Adjust plot margins to reduce left-side empty space
    plot.margin = margin(5, 5, 5, -10)  # Reduce left margin
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    sec.axis = dup_axis(name = NULL)  # Keep tick marks without labels
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

right_plot

# Combine both plots into one, side-by-side
combined_plot <- left_plot + right_plot + 
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(
    caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%."
    )

# Save the combined plot
ggsave("results/fig02.png", plot = combined_plot, width = 6.5, height = 6.5, dpi = 500)

# Display the combined plot
print(combined_plot)
##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################

##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv# Modified Figure 2 (Figure 2a, 2b)
##############################################

race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- age_bucket_summary |>
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# # Split data into two halves
# left_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[1:4])  # First 4 races
# right_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[5:8])  # Last 4 races

# Left plot (Facet titles on the LEFT)
fig02a <- ggplot(age_bucket_summary |> filter(RACE_ETH_bucket == "All"), aes(x = subgroup, y = hhsize_pctchg_2000_2019, 
                    fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  # facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +  # Define colors
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 8),  # Labels on the left
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 8),

    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),  # Right-align text
    axis.ticks.y.right = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

fig02a
ggsave("results/fig02a.png", plot = fig02a, width = 6.5, height = 3, dpi = 500)


# -----
# ---- Split data into four parts ----
first_data  <- age_bucket_summary |> subset(RACE_ETH_bucket %in% race_order[1:2])
second_data <- age_bucket_summary |> subset(RACE_ETH_bucket %in% race_order[3:4])
third_data  <- age_bucket_summary |> subset(RACE_ETH_bucket %in% race_order[5:6])
fourth_data <- age_bucket_summary |> subset(RACE_ETH_bucket %in% race_order[7:8])

# ---- Labels for each panel ----
label_data <- data.frame(
  RACE_ETH_bucket = unique(first_data$RACE_ETH_bucket),
  label = unique(first_data$RACE_ETH_bucket),
  x = 1,
  y = 14
)
label_data_2 <- data.frame(
  RACE_ETH_bucket = unique(second_data$RACE_ETH_bucket),
  label = unique(second_data$RACE_ETH_bucket),
  x = 1.5,
  y = 14
)
label_data_3 <- data.frame(
  RACE_ETH_bucket = unique(third_data$RACE_ETH_bucket),
  label = unique(third_data$RACE_ETH_bucket),
  x = 1.5,
  y = 14
)
label_data_4 <- data.frame(
  RACE_ETH_bucket = unique(fourth_data$RACE_ETH_bucket),
  label = unique(fourth_data$RACE_ETH_bucket),
  x = 1.5,
  y = 14
)

# ---- Optional asterisk data ----
asterisk_data <- fourth_data |> subset(RACE_ETH_bucket == "Other" & subgroup == "80-84")

# ---- Plot 1 ----
first_plot <- ggplot(first_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019,
                                     fill = RACE_ETH_bucket == "All")) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +
  theme_minimal() +
  theme(
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),
    axis.ticks.y.right = element_line(color = "black", size = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))
  ) +
  labs(y = NULL, x = NULL) +
  geom_text(data = label_data, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2, fontface = "bold")

# ---- Plot 2 ----
second_plot <- ggplot(second_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.8, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_line(color = "black", size = 0.3),
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),
    axis.ticks.y.right = element_line(color = "black", size = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))
  ) +
  labs(y = NULL, x = NULL) +
  geom_text(data = label_data_2, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2, fontface = "bold")

# ---- Plot 3 ----
third_plot <- ggplot(third_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.8, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_line(color = "black", size = 0.3),
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),
    axis.ticks.y.right = element_line(color = "black", size = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))
  ) +
  labs(y = NULL, x = NULL) +
  geom_text(data = label_data_3, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2, fontface = "bold")

# ---- Plot 4 ----
fourth_plot <- ggplot(fourth_data, aes(x = subgroup,
                                       y = pmin(hhsize_pctchg_2000_2019, 14))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  geom_text(data = asterisk_data,
            aes(x = subgroup, y = 14.5, label = "*"),
            size = 5, color = "black") +
  theme_minimal() +
  theme(
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0.8, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  scale_y_continuous(
    breaks = NULL,
    limits = c(-15, 15),
    labels = NULL,
    sec.axis = dup_axis(name = NULL, labels = NULL)
  ) +
  labs(y = NULL, x = NULL) +
  geom_text(data = label_data_4, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, hjust = 0, size = 2, fontface = "bold")

# ---- Combine plots ----
fig02b <- (
  first_plot + second_plot + third_plot + fourth_plot +
    plot_layout(widths = c(1, 1, 1, 1)) +
    plot_annotation(
      caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%.",
      theme = theme(plot.caption = element_text(size = 5))
    )
)


fig02b

# ---- Save ----
ggsave("results/fig02b.png", plot = fig02b, width = 6.5, height = 4, dpi = 500)

##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################


# Household size in 2000 and 2019 by age bucket
tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "AGE_bucket")

##############################################
# Fast fact: percent of the population living in institutions in 2000 and 2019
##############################################
source("src/utils/aggregation-tools.R")

## 2000 GROUP QUARTERS STATUS ##
# Summarize GQ status for 2000 survey
gq_2000 <- weighted_mean(
  data = ipums_db |> filter(YEAR == 2000),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("GQ")
) |> collect() |>
  mutate(pct_of_pop = sum_weights / sum(sum_weights))

# Count % in GQ in 2000
in_gq_2000 <- gq_2000 |> 
  filter(GQ %in% c(3, 4, 5)) |>
  summarize(sum_pct = sum(pct_of_pop)) |>
  pull(sum_pct)

# Count % out GQ in 2000
out_gq_2000 <- gq_2000 |> 
  filter(GQ %in% c(1, 2)) |>
  summarize(sum_pct = sum(pct_of_pop)) |>
  pull(sum_pct)

gq_2000
in_gq_2000
out_gq_2000

## 2019 GROUP QUARTERS STATUS ##
# Summarize GQ status for 2019 survey
gq_2019 <- weighted_mean(
  data = ipums_db |> filter(YEAR == 2019),
  value_column = "NUMPREC",
  weight_column = "PERWT",
  group_by_columns = c("GQ")
) |> collect() |>
  mutate(pct_of_pop = sum_weights / sum(sum_weights))

# Count % in GQ in 2000
in_gq_2019 <- gq_2019 |> 
  filter(GQ %in% c(3, 4, 5)) |>
  summarize(sum_pct = sum(pct_of_pop)) |>
  pull(sum_pct)

# Count % out GQ in 2000
out_gq_2019 <- gq_2019 |> 
  filter(GQ %in% c(1, 2)) |>
  summarize(sum_pct = sum(pct_of_pop)) |>
  pull(sum_pct)

gq_2019
in_gq_2019
out_gq_2019

##############################################
# Fast fact: household size by age in 2000 and 2019
##############################################

## Table of household sizes in 2000 and 2019 by age group
age_bucket_summary |> filter(RACE_ETH_bucket == "All")

## Age buckets with the largest household sizes
age_bucket_summary |> filter(RACE_ETH_bucket == "All") |> slice_max(hhsize_2000, n = 1)
age_bucket_summary |> filter(RACE_ETH_bucket == "All") |> slice_max(hhsize_2019, n = 1)

## Average household size among children age 0-19
ipums_db_age_v1 <- ipums_db |>
  # Add columns for whether an individual is under 20 or over 65
  mutate(
    under20 = (AGE < 20), # excludes 20 year-olds
    over65 = (AGE >= 65), # includes 65 year-olds
    from20to64 = (AGE >= 20 & AGE < 65) # includes 20 yos, excludes 65 yos
  )

ipums_db_age_v2 <- ipums_db_age_v1 |>
  # Add a column for whether a household contains an individual under 20
  group_by(SAMPLE, SERIAL, YEAR) |> # uniquely IDs HHs
  mutate(
    contains_under20 = any(under20),
    count_under20 = sum(as.integer(under20))
    ) |>
  ungroup()

ipums_db_age_v3 <- ipums_db_age_v2 |>
  # Add a column for whether an individual is an adult over 20 living in a hh with an under-20 yo
  # Also add a column for the number of over 20 adults in a household with children
  mutate(
    cohabit_under20 = (contains_under20 & under20 == FALSE),
    n_cohabit_under20 = NUMPREC - count_under20
  )

# # Visually inspect a few entries to ensure logic works properly
# # Running this line will take 1-2 minutes
# x <- ipums_db_age_v3 |> head(100) |> collect() |>
#   select(SERIAL, NUMPREC, PERNUM, AGE, under20, from20to64, over65, contains_under20, count_under20, cohabit_under20, n_cohabit_under20)
# View(x) # logic appears consistent with intention

# Note: I needed 12 cores to make this step work. 5 cores, and the session crashes.
tabulate_summary_2year(data = ipums_db_age_v3, years = c(2000,2019), group_by = "under20")
tabulate_summary_2year(data = ipums_db_age_v3, years = c(2000,2019), group_by = "from20to64")
tabulate_summary_2year(data = ipums_db_age_v3, years = c(2000,2019), group_by = "over65")

## Average number of adults (age 20+) per household that contains at least one child (age <20)
# ... in 2019
crosstab_mean(
  data = ipums_db_age_v3 |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)) |> filter(contains_under20 == TRUE),
  value = "n_cohabit_under20",
  wt_col = "PERWT",
  group_by = c()
)
# ... in 2000
crosstab_mean(
  data = ipums_db_age_v3 |> filter(YEAR == 2000) |> filter(GQ %in% c(0,1,2)) |> filter(contains_under20 == TRUE),
  value = "n_cohabit_under20",
  wt_col = "PERWT",
  group_by = c()
)

##############################################
# Fast fact: household size by cPUMA
##############################################
## Average hhsize by CPUMA0010 in 2000 and 2019
cpuma_hhsize <- tabulate_summary_2year(data = ipums_db, years = c(2000,2019), group_by = "CPUMA0010")
cpuma_hhsize

## Household size in the median CPUMA in 2000
median(cpuma_hhsize$hhsize_2000)

## Minimum household size in a CPUMA in 2000
min(cpuma_hhsize$hhsize_2000)

## Maximum household size in a CPUMA in 2000
max(cpuma_hhsize$hhsize_2000)

## Average household size increased/decreased in X% of CPUMAs from 2000 - 2019
cpuma_hhsize <- cpuma_hhsize |>
  mutate(
    hhsize_decreased = (hhsize_pctchg_2000_2019 < 0)
  )
sum(cpuma_hhsize$hhsize_decreased) # number of CPUMAs where HH size decreased
nrow(cpuma_hhsize) # total number of CPUMAs

sum(cpuma_hhsize$hhsize_decreased) / nrow(cpuma_hhsize) # Proportion of CPUMAs where HH size decreased

## Household size in the median CPUMA in 2019
median(cpuma_hhsize$hhsize_2019)

## Change in range in CPUMA-level HH size from 2000 to 2019
# 2000 min, max, range
min_2000 <- min(cpuma_hhsize$hhsize_2000)
max_2000 <- max(cpuma_hhsize$hhsize_2000)

min_2000
max_2000
max_2000 - min_2000

# 2019 min, max, range
min_2019 <- min(cpuma_hhsize$hhsize_2019)
max_2019 <- max(cpuma_hhsize$hhsize_2019)

min_2019
max_2019
max_2019 - min_2019

##############################################
# Fast fact: number of additional housing units needed
##############################################
source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

ipums_db <- tbl(con, "ipums_processed")

# Calculate CPUMA-level fully-controlled diffs
hhsize_contributions <- calculate_counterfactual(
  cf_categories = c("RACE_ETH_bucket", "AGE_bucket", "gender", "us_born", "EDUC_bucket", "INCTOT_cpiu_2010_bucket", "CPUMA0010", "tenure"),
  p0 = 2000,
  p1 = 2019,
  p0_data = ipums_db |> filter(YEAR == 2000, GQ %in% c(0,1,2)), 
  p1_data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  outcome = "NUMPREC"
)$contributions  

# Counterfactual overall household size in 2019
cf_hhsize_2019_overall <- (hhsize_contributions$weighted_mean_2000 * hhsize_contributions$percent_2019) |> sum() / 100
cf_hhsize_2019_overall

population_aggregates_2019 <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019, GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c(),
  every_combo = TRUE)

# Actual overall household size in 2019 (excluding those in Group Quarters)
act_hhsize_2019_overall <- population_aggregates_2019 |>
  pull(weighted_mean)
act_hhsize_2019_overall

# Population in 2019 (excluding those in Group Quarters)
population_2019 <- population_aggregates_2019 |>
  pull(weighted_count)
population_2019

## Overall housing unit shortage/surfeit in 2019 (netting out positives and negatives within CPUMAs)
(population_2019/act_hhsize_2019_overall) - (population_2019/cf_hhsize_2019_overall)

##############################################
# Fast fact: number of additional housing units needed, accounting for CPUMA-level surfeit/surplus
##############################################

# Sum the counterfactuals by CPUMA
hhsize_contributions_cpuma <- hhsize_contributions |>
  mutate(
    contribution = weighted_mean_2000 * percent_2019 / 100
  ) |>
  group_by(CPUMA0010) |>
  summarize(
    contribution = sum(contribution, na.rm = TRUE),
    population = sum(weighted_count_2019, na.rm = TRUE),
    percent = sum(percent_2019, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(cf_hhsize = contribution/percent * 100)

# Validity check: ensure the sum of contributions equals cf_hhsize_2019_overall
# Rounded to 5 decimal places due to floating point errors
round((hhsize_contributions_cpuma$contribution |> sum()), 5) == round(cf_hhsize_2019_overall, 5)

# Validity check: sum of `population` column matches population_2019
sum(hhsize_contributions_cpuma$population) == population_2019

# Validity check: cf_hhsize*percent/100 |> sum() = cf_hhsize_2019_overall
round(((hhsize_contributions_cpuma$cf_hhsize*hhsize_contributions_cpuma$percent/100)) |> sum(), 5) == round(cf_hhsize_2019_overall, 5)

cf_by_cpuma <- cpuma_hhsize |>
  rename(CPUMA0010 = subgroup) |>
  left_join(hhsize_contributions_cpuma, by = "CPUMA0010") |>
  mutate(
    housing_surfeit = (population/hhsize_2019) - (population/cf_hhsize)
  )

# Housing shortage and surplus, and number of CPUMAs with a shortage or surplus
cf_by_cpuma_summary <- cf_by_cpuma |> 
  summarize(
    total = sum(housing_surfeit, na.rm = TRUE),
    count_total = sum(!is.na(housing_surfeit)),
    sum_negative = sum(housing_surfeit[housing_surfeit < 0], na.rm = TRUE),
    sum_positive = sum(housing_surfeit[housing_surfeit > 0], na.rm = TRUE),
    count_negative = sum(housing_surfeit < 0, na.rm = TRUE),
    count_positive = sum(housing_surfeit > 0, na.rm = TRUE)
  )
cf_by_cpuma_summary

# Percentage with a shortage
pull(cf_by_cpuma_summary, count_negative) / pull(cf_by_cpuma_summary, count_total)

# Largest surfeit
slice_max(cf_by_cpuma, housing_surfeit) |> View()
# Largest shortage
slice_min(cf_by_cpuma, housing_surfeit) |> View()

# Figure out where the greatest surfeit/surplus is by mapping.
# TODO: Don't just copy and paste this code from counterfactual-regional.R. Create
# a function that can be called to map things.
rot <- function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}
transform_state <- function(
    df, 
    state_fp, 
    rotation_angle, 
    scale_factor, 
    shift_coords
) {
  state <- df %>% filter(STATEFIP == state_fp)
  state_geom <- st_geometry(state)
  state_centroid <- st_centroid(st_union(state_geom))
  rotated_geom <- (state_geom - state_centroid) * rot(rotation_angle * pi / 180) / scale_factor + state_centroid + shift_coords
  state %>% st_set_geometry(rotated_geom) %>% st_set_crs(st_crs(df))
}
# Load shapefiles. Data is unzipped from WHERE? TODO: document
cpuma_sf <- st_read("data/ipums-cpuma0010-sf/ipums_cpuma0010.shp") |>
  filter(!STATEFIP %in% c('60', '64', '66', '68', '69', '70', '72', '78')) |># Remove excluded states, like Puerto Rico
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance =  5000))  # Simplify shapes

# Rotate and move Alaska and Hawaii to fit on map
alaska_cpuma <- transform_state(cpuma_sf, "02", -39, 2.3, c(1000000, -5000000))
hawaii_cpuma <- transform_state(cpuma_sf, "15", -35, 1, c(5200000, -1400000))

# Final map after transforming non-contiguous states
cpuma_sf_final <- cpuma_sf |>
  filter(!STATEFIP %in% c("02", "15")) |>
  bind_rows(alaska_cpuma, hawaii_cpuma)

# Highlight CPUMA 973 in red
cpuma_sf_hhsize <- cpuma_sf_final |>
  mutate(
    fill_color = ifelse(CPUMA0010 == "973", "red", "white")
  )
ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = fill_color), color = "grey50", size = 0.1) +
  scale_fill_identity() + 
  theme_void()


# Highlight CPUMA 550 in red
cpuma_sf_hhsize <- cpuma_sf_final |>
  mutate(
    fill_color = ifelse(CPUMA0010 == "550", "red", "white")
  )
ggplot(cpuma_sf_hhsize) + 
  geom_sf(aes(geometry = geometry, fill = fill_color), color = "grey50", size = 0.1) +
  scale_fill_identity() + 
  theme_void()


##############################################
# Fast fact: number of additional housing units needed, all CPUMAs have average HH size of white americans
##############################################

# Average size of a white household
white_hhsize_2019_overall <- crosstab_mean(
  data = ipums_db |> filter(YEAR == 2019) |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("RACE_ETH_bucket")
) |> 
  filter(RACE_ETH_bucket == "White") |>
  pull(weighted_mean)

## Overall housing unit shortage/surfeit in 2019 relative to white household norms (netting out positives and negatives within CPUMAs)
(population_2019/act_hhsize_2019_overall) - (population_2019/white_hhsize_2019_overall) # huge number!

## cPUMA-level household counterfactual, except we replace every cPUMA's cf_hhsize with 3.09 (the value of `white_hhsize_2019_overall`)
hhsize_contributions_cpuma_white <- hhsize_contributions_cpuma |>
  select(-cf_hhsize) |>
  mutate(cf_hhsize_white = white_hhsize_2019_overall)

cf_by_cpuma_white <- cpuma_hhsize |>
  rename(CPUMA0010 = subgroup) |>
  left_join(hhsize_contributions_cpuma_white, by = "CPUMA0010") |>
  mutate(
    housing_surfeit = (population/hhsize_2019) - (population/cf_hhsize_white)
  )

# Housing shortage and surplus, and number of CPUMAs with a shortage or surplus
cf_by_cpuma_summary_white <- cf_by_cpuma_white |> 
  summarize(
    total = sum(housing_surfeit, na.rm = TRUE),
    count_total = sum(!is.na(housing_surfeit)),
    sum_negative = sum(housing_surfeit[housing_surfeit < 0], na.rm = TRUE),
    sum_positive = sum(housing_surfeit[housing_surfeit > 0], na.rm = TRUE),
    count_negative = sum(housing_surfeit < 0, na.rm = TRUE),
    count_positive = sum(housing_surfeit > 0, na.rm = TRUE)
  )
cf_by_cpuma_summary_white

# Percentage with a shortage
pull(cf_by_cpuma_summary_white, count_negative) / pull(cf_by_cpuma_summary_white, count_total)
