# This script produces Figure 2 
# 
# 
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
ipums_db <- tbl(con, "ipums_processed") |>
  filter(GQ %in% c(0,1,2))

# ----- Step 3: Define functions for tabulating summaries ----- #
source("src/utils/aggregation-tools.R")



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
#ggsave("results/fig01.png", plot = fig01, width = 6.5, height = 3.5, dpi = 500)

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


# Combine both plots into one, side-by-side
combined_plot <- left_plot + right_plot + 
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(
    caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%."
  )


# Display the combined plot
print(combined_plot)
# Save the combined plot
#ggsave("results/fig02.png", plot = combined_plot, width = 6.5, height = 6.5, dpi = 500)
