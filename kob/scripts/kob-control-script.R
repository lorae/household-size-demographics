# kob/scripts/kob-control-script.R
# The purpose of this script is to run a full kob analysis by running two subtasks:
# 1. create an object suitable for input in the kob function using the 
#    kob-prepare-data.R script
# 2. put the output object from that script into the kob function, defined in
#    kob-function.R
# 3. Create graphs and tables of the results using a kob-graphs-tables.R
# TODO: functionalize the kob-prepare-data.R script
# TODO: functionalize the kob-graphs-tables.R script
# Both of these to-dos are essential to create a robust pipeline. The current 
# version (a work in progress) is fragile because it depends on environmental
# variables that are not well-documented in pass-offs from script-to-script.

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

source("src/utils/regression-tools.R") # add_intercept function

# ----- Step 1: Load in data ----- #
# Only run this if kob_input.rds doesn't already exist
if (!file.exists("throughput/kob_input.rds")) {
  source("kob/scripts/kob-prepare-data.R")
}

kob_input <- readRDS("throughput/kob_input.rds")

# This is for validation (kob_output_validate)
aggregates <- readRDS("throughput/aggregates.rds")

# This is for tidying (kob_tidy_output)
varnames_dict <- c(
  "RACE_ETH_bucket",
  "AGE_bucket",
  "EDUC_bucket",
  "INCTOT_cpiu_2010_bucket",
  "us_born",
  "gender",
  "tenure",
  "cpuma"
)

# ----- Step 2: Run the kob analysis ----- #
# Each section covers a separate outcome
source("kob/scripts/kob-function.R") # defines the `kob` function and `kob-output-validate()`

# TODO: refactor teh aggregates df so the variable names match the aliases in kob_input
# --- Bedroom ---
kob_bedroom <- kob(kob_input$bedroom) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_bedroom,
  mean_2000 = aggregates |> filter(variable == "bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "bedroom") |> pull(mean_2019)
)

# --- Number of People ---
kob_numprec <- kob(kob_input$numprec) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_numprec,
  mean_2000 = aggregates |> filter(variable == "NUMPREC") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "NUMPREC") |> pull(mean_2019)
)

# --- Rooms ---
kob_room <- kob(kob_input$room) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_room,
  mean_2000 = aggregates |> filter(variable == "room") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "room") |> pull(mean_2019)
)

# --- Persons per Room (PPR) ---
kob_ppr <- kob(kob_input$ppr) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppr,
  mean_2000 = aggregates |> filter(variable == "persons_per_room") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "persons_per_room") |> pull(mean_2019)
)

# --- Persons per Bedroom (PPBR) ---
kob_ppbr <- kob(kob_input$ppbr) |>
  kob_tidy_output() |>
  add_intercept(variable = "RACE_ETH_bucket", reference_value = "White")

kob_output_validate(
  kob_ppbr,
  mean_2000 = aggregates |> filter(variable == "persons_per_bedroom") |> pull(mean_2000),
  mean_2019 = aggregates |> filter(variable == "persons_per_bedroom") |> pull(mean_2019)
)

# ----- Step 3: Graphs ----- #
# General: Create named list of KOB outputs and plot titles
kob_outputs <- list(
  "Bedrooms" = kob_bedroom,
  "Number of People" = kob_numprec,
  "Rooms" = kob_room,
  "Persons per Room" = kob_ppr,
  "Persons per Bedroom" = kob_ppbr
)

# --- 3.6: Figure 6 -  KOB counterfactual bar charts
# TODO: include appendix chart
label_lookup <- tibble::tibble(
  variable = c("NUMPREC", "bedroom", "room", "persons_per_room", "persons_per_bedroom"),
  name = c("Number of People", "Bedrooms", "Rooms", "Persons per Room", "Persons per Bedroom")
)

# Rounding helpers
round_down_to <- function(x, base) base * floor(x / base)
round_up_to <- function(x, base) base * ceiling(x / base)

# Create an observed, expected table used to produce these figures. Observed values
# in 2000 are already in `aggregates` - we rename those cols and add on a third 
# col using kob results
increment <- 0.5  # Change this to 1 or 0.2 if you want

fig06_data <- fig06_data |>
  rowwise() |>
  mutate(
    e = sum(kob_outputs[[name]]$e, na.rm = TRUE),
    c = sum(kob_outputs[[name]]$c, na.rm = TRUE),
    u = sum(kob_outputs[[name]]$u, na.rm = TRUE),
    expected_2019 = observed_2000 + e,
    min_val = min(observed_2000, observed_2019, expected_2019, na.rm = TRUE),
    max_val = max(observed_2000, observed_2019, expected_2019, na.rm = TRUE),
    ymin = round_down_to(min_val, increment),
    ymax = round_up_to(max_val, increment)
  ) |>
  ungroup() |>
  relocate(expected_2019, .after = observed_2019) |>
  select(-min_val, -max_val)


# Choose the variable to plot
target_name <- "Persons per Bedroom"

# Get the relevant row including ymin/ymax
row <- fig06_data |> filter(name == target_name)

# Build plot data
fig_data <- tibble(
  Category = factor(
    c("2000 Observed", "2019 Observed", "2019 Expected"),
    levels = c("2000 Observed", "2019 Observed", "2019 Expected")
  ),
  Household_Size = c(row$observed_2000, row$observed_2019, row$expected_2019),
  Type = c("Observed", "Observed", "Expected"),
  ymin = row$ymin,
  ymax = row$ymax
)

# Use computed limits from the row
fig <- ggplot(fig_data, aes(x = Category, y = Household_Size, fill = Type, linetype = Type)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.2, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
  scale_fill_manual(values = c(
    "Observed" = "steelblue", 
    "Expected" = scales::alpha("steelblue", 0.5)
  )) +
  scale_linetype_manual(values = c(
    "Observed" = "solid", 
    "Expected" = "dotted"
  )) +
  labs(
    title = target_name,
    y = NULL, x = NULL
  ) +
  coord_cartesian(ylim = c(row$ymin, row$ymax)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    plot.title = element_text(size = 13)
  )

fig

# --- 3.7: Figure 7 -  KOB decomposition bar charts
source("src/figures/fig07-kob-decomp-bars.R") # Defines functions needed for this plot
pretty_labels <- c(
  us_born = "U.S. born",
  tenure = "Tenure",
  RACE_ETH_bucket = "Race / Ethnicity",
  INCTOT_cpiu_2010_bucket = "Income",
  gender = "Sex",
  EDUC_bucket = "Education",
  cpuma = "CPUMA",
  AGE_bucket = "Age",
  Intercept = "Intercept",
  Total = "Total"
)

# Generate and print all five plots
plots <- imap(kob_outputs, ~{
  plot_data <- prepare_kob_plot_data(.x, varnames = varnames_dict, pretty_labels = pretty_labels)
  plot_kob_decomposition(plot_data, title = .y, show_total = TRUE)
})

# Optional: Display plots in RStudio
for (p in plots) print(p)



