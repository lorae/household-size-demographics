# A script for generating plots of household size by tenure status

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("duckdb")
library("stringr")
library("tidyr")
library("purrr")
library("glue")
library("ggplot2")
library("rlang")
library("ggplot2")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")
# source("src/utils/counterfactual-tools.R") # Includes function for counterfactual calculation

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3: Summarize mean household size by tenure status at every age

tenure_age_2000 <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2) & YEAR == 2000),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "OWNERSHP"),
  every_combo = TRUE
) |>
  mutate(year = 2000)

tenure_age_2019 <- crosstab_mean(
  data = ipums_db |> filter(GQ %in% c(0,1,2) & YEAR == 2019),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("AGE", "OWNERSHP"),
  every_combo = TRUE
) |>
  mutate(year = 2019)

# Combine the datasets
tenure_long <- bind_rows(tenure_age_2000, tenure_age_2019) |>
  mutate(
    tenure_label = case_when(
      OWNERSHP == 1 ~ "Homeowner",
      OWNERSHP == 2 ~ "Renter",
      TRUE ~ "Other"
    ),
    group = paste0(tenure_label, " ", year)
  ) |>
  select(AGE, weighted_mean, group)


# ---- Ensure desired order ---- #
tenure_long <- tenure_long |> 
  mutate(group = factor(group, levels = c(
    "Renter 2000",
    "Renter 2019",
    "Homeowner 2000",
    "Homeowner 2019"
  )))

# ---- Style settings ---- #
style_map <- list(
  "Renter 2000"    = list(color = "#F94144", linetype = "solid"),
  "Homeowner 2000" = list(color = "steelblue", linetype = "solid"),
  "Renter 2019"    = list(color = "#F94144", linetype = "dotted"),
  "Homeowner 2019" = list(color = "steelblue", linetype = "dotted")
)

colors <- sapply(style_map, `[[`, "color")
linetypes <- sapply(style_map, `[[`, "linetype")

# ---- Plot ---- #
ggplot(tenure_long, aes(
  x = AGE, 
  y = weighted_mean, 
  color = group, 
  linetype = group
)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    minor_breaks = seq(0, 100, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  labs(
    title = "Mean Household Size by Age and Tenure",
    x = "Age",
    y = "Household Size",
    color = NULL,
    linetype = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(margin = margin(r = 12)),
    panel.grid.minor = element_line(color = "gray90"),
    panel.grid.major = element_line(color = "gray80")
  )