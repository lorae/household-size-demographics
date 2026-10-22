# #src/figures/linear-reg/fig02-pctchg-hhsize-age-race-slides.R
#
# Slide version of fig02: % change in hhsize 2000-2019 by age bucket and race/ethnicity,
# with a wider canvas, larger x-axis labels, and every other age bucket labelled
#
# Input: data/db/ipums.duckdb
# Output: output/figures/linear-reg/fig02-pctchg-hhsize-age-race-slides.png

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("purrr")
library("ggplot2")
library("patchwork")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
source("src/utils/aggregation-tools.R")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

age_bucket_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                       "25-29", "30-34", "35-39", "40-44", "45-49",
                       "50-54", "55-59", "60-64", "65-69", "70-74",
                       "75-79", "80-84", "85plus")
group_encoding_age_bucket <- setNames(age_bucket_levels, age_bucket_levels)

race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- map_dfr(race_order, function(race) {
  data_filtered <- if (race == "All") ipums_db else ipums_db |> filter(RACE_ETH_bucket == race)

  tabulate_summary_2year(
    data = data_filtered,
    years = c(2000, 2019),
    group_by = "AGE_bucket",
    group_encoding = group_encoding_age_bucket
  ) |> mutate(RACE_ETH_bucket = race)
}) |>
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# ----- Step 3: Make plots ----- #

# Label every other age bucket; all bars are still drawn
x_label_breaks <- age_bucket_levels[c(TRUE, FALSE)]

left_data <- age_bucket_summary |> filter(RACE_ETH_bucket %in% race_order[1:4])
right_data <- age_bucket_summary |> filter(RACE_ETH_bucket %in% race_order[5:8])

# Left plot (Facet titles on the LEFT)
left_plot <- ggplot(left_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019,
                                   fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +
  scale_x_discrete(breaks = x_label_breaks) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 10),
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),

    # Hide left-side axis labels
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),

    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 10, color = "black", hjust = 1),
    axis.ticks.y.right = element_line(color = "black", linewidth = 0.3),

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
  labs(y = NULL, x = NULL)

# Right plot (Facet titles on the RIGHT)
right_plot <- ggplot(right_data, aes(x = subgroup,
                                     y = pmin(hhsize_pctchg_2000_2019, 14))) +  # Cap at 14% (leave room for asterisk at 14.5%)
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +

  # Asterisk above the capped bar
  geom_text(data = right_data |> filter(RACE_ETH_bucket == "Other", subgroup == "80-84"),
            aes(x = subgroup, y = 14.5, label = "*"),
            size = 5, color = "black") +
  scale_x_discrete(breaks = x_label_breaks) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, color = "black", size = 10),
    strip.placement = "outside",
    panel.spacing = unit(0.8, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),

    # Add subtle tick marks on the LEFT side without labels
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_line(color = "black", linewidth = 0.3),

    # Hide right-side axis labels
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),

    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),

    plot.margin = margin(5, 5, 5, -10)
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),
    limits = c(-15, 15),
    sec.axis = dup_axis(name = NULL)
  ) +
  labs(y = NULL, x = NULL)

combined_plot <- left_plot + right_plot +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%."
  )
combined_plot

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/linear-reg/fig02-pctchg-hhsize-age-race-slides.png",
  plot = combined_plot,
  width = 11, height = 7.25, dpi = 500
)

dbDisconnect(con)
