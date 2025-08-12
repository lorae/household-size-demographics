# src/figures/figA10-refactored.R
# Modular approach for creating coefficient/endowment breakdown figures

library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

source("src/utils/kob_panels.R")
kob_output <- readRDS("throughput/kob_output.rds")

# ----- Configuration -----
xlims <- list(
  persons           = c(-0.13, 0.12),
  bedrooms          = c(-0.10, 0.40),
  ppbr              = c(-0.30, 0.15),
  rooms             = c(-0.10, 0.40),
  persons_per_room  = c(-0.15, 0.10)
)

spacer <- patchwork::plot_spacer()

# =========================
# LABEL FUNCTIONS
# =========================

label_usborn <- function(df) {
  if ("value" %in% names(df)) {
    v <- tolower(as.character(df$value))
    ifelse(v %in% c("0","false","foreign","foreign born","non-us","non‑us"),
           "Foreign Born", "U.S. Born")
  } else if ("term" %in% names(df)) {
    t <- tolower(df$term)
    ifelse(grepl("false|0|foreign|non[-_ ]?us", t),
           "Foreign Born", "U.S. Born")
  } else {
    rep(c("U.S. Born","Foreign Born"), length.out = nrow(df))
  }
}

label_raceeth <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  dplyr::recode(term_val,
                "RACE_ETH_bucketAAPI"        = "AAPI",
                "RACE_ETH_bucketAIAN"        = "AIAN",
                "RACE_ETH_bucketHispanic"    = "Hispanic", 
                "RACE_ETH_bucketBlack"       = "Black",
                "RACE_ETH_bucketWhite"       = "White",
                "RACE_ETH_bucketMultiracial" = "Multiracial",
                "RACE_ETH_bucketOther"       = "Other",
                .default = term_val)
}

label_tenure <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  dplyr::recode(term_val,
                "tenurehomeowner" = "Homeowner",
                "tenurerenter"    = "Renter",
                .default = term_val)
}

label_education <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  dplyr::recode(term_val,
                "EDUC_bucketcollege_4yr+" = "College 4yr+",
                "EDUC_bucketsome_college" = "Some College",
                "EDUC_buckeths"          = "High School",
                "EDUC_bucketless_than_hs" = "Less than HS",
                .default = term_val)
}

label_age <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  dplyr::recode(term_val,
                "AGE_bucket0-4"    = "0-4",
                "AGE_bucket5-9"    = "5-9", 
                "AGE_bucket10-14"  = "10-14",
                "AGE_bucket15-19"  = "15-19",
                "AGE_bucket20-24"  = "20-24",
                "AGE_bucket25-29"  = "25-29",
                "AGE_bucket30-34"  = "30-34",
                "AGE_bucket35-39"  = "35-39",
                "AGE_bucket40-44"  = "40-44",
                "AGE_bucket45-49"  = "45-49",
                "AGE_bucket50-54"  = "50-54",
                "AGE_bucket55-59"  = "55-59",
                "AGE_bucket60-64"  = "60-64",
                "AGE_bucket65-69"  = "65-69",
                "AGE_bucket70-74"  = "70-74",
                "AGE_bucket75-79"  = "75-79",
                "AGE_bucket80-84"  = "80-84",
                "AGE_bucket85plus" = "85+",
                .default = term_val)
}

# =========================
# GENERIC BUILDER FUNCTION
# =========================

build_decomp_column <- function(kob_df, variable_name, part_type, title,
                                x_limits, label_fun = NULL, 
                                filter_labels = NULL, label_order = NULL,
                                show_xlab = TRUE, show_ylab = TRUE, show_ytext = TRUE,
                                x_lab = "Contribution to Outcome Gap") {
  
  # Get the data
  lev <- prep_subgroup_panel(
    kob_df, variable = variable_name, part = part_type,
    level_label_fun = label_fun, level_order = NULL,
    include_total = FALSE
  )
  
  # Filter if specified
  if (!is.null(filter_labels)) {
    lev <- lev |> filter(label %in% filter_labels)
  }
  
  # Create total row
  total_label <- case_when(
    variable_name == "us_born" ~ "TOTAL: Birthplace",
    variable_name == "RACE_ETH_bucket" ~ "TOTAL: Race / Ethnicity", 
    variable_name == "tenure" ~ "TOTAL: Tenure",
    variable_name == "EDUC_bucket" ~ "TOTAL: Education",
    variable_name == "AGE_bucket" ~ "TOTAL: Age",
    TRUE ~ paste("TOTAL:", variable_name)
  )
  
  total_row <- lev |>
    summarise(
      label = total_label,
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  df <- bind_rows(total_row, lev)
  
  # Set label order
  if (is.null(label_order)) {
    label_order <- c(total_label, unique(as.character(lev$label)))
  }
  
  # Determine y-label and color
  y_lab <- ifelse(part_type == "coeff", "Coefficients", "Endowments")
  fill_color <- ifelse(part_type == "coeff", kob_cols$coeff, kob_cols$endow)
  
  plot_kob_panel(
    df = df, y_lab = y_lab, fill_hex = fill_color,
    show_xlab = show_xlab, show_ylab = show_ylab,
    x_lab = x_lab, x_limits = x_limits,
    show_ytext = show_ytext, label_order = label_order
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 2)))
}

# =========================
# FIGURE SPECIFICATIONS
# =========================

# Define all your figure configurations in one place
figure_configs <- list(
  birthplace = list(
    variable = "us_born",
    part = "coeff", 
    label_fun = label_usborn,
    filter_labels = NULL,
    label_order = c("TOTAL: Birthplace", "Foreign Born", "U.S. Born")
  ),
  
  race_ethnicity = list(
    variable = "RACE_ETH_bucket",
    part = "coeff",
    label_fun = label_raceeth, 
    filter_labels = c("White", "Hispanic", "Black"),
    label_order = c("TOTAL: Race / Ethnicity", "Black", "Hispanic", "White")
  ),
  
  tenure = list(
    variable = "tenure",
    part = "endow",
    label_fun = label_tenure,
    filter_labels = NULL,
    label_order = c("TOTAL: Tenure", "Renter", "Homeowner")
  ),
  
  race_ethnicity_endow = list(
    variable = "RACE_ETH_bucket",
    part = "endow",
    label_fun = label_raceeth, 
    filter_labels = c("White", "Hispanic", "Black"),
    label_order = c("TOTAL: Race / Ethnicity", "Black", "Hispanic", "White")
  ),
  
  education = list(
    variable = "EDUC_bucket",
    part = "endow",
    label_fun = label_education,
    filter_labels = NULL,  # Removed filtering to see if there's an NA
    label_order = c("TOTAL: Education", "Less than HS", "High School", "Some College", "College 4yr+")
  ),
  
  age = list(
    variable = "AGE_bucket",
    part = "endow",
    label_fun = label_age,
    filter_labels = NULL,
    label_order = c("TOTAL: Age", "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                    "75-79", "80-84", "85+")
  )
  
  # Easy to add more configurations here:
  # new_variable = list(
  #   variable = "your_variable_name",
  #   part = "coeff" or "endow", 
  #   label_fun = your_label_function,
  #   filter_labels = c("keep", "these", "labels") or NULL,
  #   label_order = c("TOTAL: Name", "Label1", "Label2")
  # )
)

# =========================
# FIGURE GENERATION FUNCTION
# =========================

create_figure_set <- function(config_name, config, suffix = "") {
  cfg <- config
  
  # Create all panels
  p1 <- build_decomp_column(kob_output$p, cfg$variable, cfg$part, "Number of Persons",
                            xlims$persons, cfg$label_fun, cfg$filter_labels, cfg$label_order,
                            show_xlab = FALSE, show_ylab = TRUE, show_ytext = TRUE)
  
  p2 <- build_decomp_column(kob_output$b, cfg$variable, cfg$part, "Number of Bedrooms", 
                            xlims$bedrooms, cfg$label_fun, cfg$filter_labels, cfg$label_order,
                            show_xlab = TRUE, show_ylab = FALSE, show_ytext = FALSE)
  
  p3 <- build_decomp_column(kob_output$ppbr, cfg$variable, cfg$part, "Persons per Bedroom",
                            xlims$ppbr, cfg$label_fun, cfg$filter_labels, cfg$label_order,
                            show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)
  
  p4 <- build_decomp_column(kob_output$r, cfg$variable, cfg$part, "Number of Rooms",
                            xlims$rooms, cfg$label_fun, cfg$filter_labels, cfg$label_order,
                            show_xlab = TRUE, show_ylab = FALSE, show_ytext = FALSE)
  
  p5 <- build_decomp_column(kob_output$ppr, cfg$variable, cfg$part, "Persons per Room",
                            xlims$persons_per_room, cfg$label_fun, cfg$filter_labels, cfg$label_order,
                            show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)
  
  # Create figure layouts
  fig_bedroom <- (p1 | spacer | p2 | spacer | p3) + plot_layout(widths = c(1, 0.08, 1, 0.08, 1))
  fig_room <- (p1 | spacer | p4 | spacer | p5) + plot_layout(widths = c(1, 0.08, 1, 0.08, 1))
  
  # Save figures
  height <- case_when(
    config_name == "race_ethnicity" ~ 600,
    config_name == "tenure" ~ 550,
    config_name == "education" ~ 650,
    config_name == "age" ~ 1500,  # Taller for age with many categories
    TRUE ~ 500
  )
  
  ggsave(paste0("output/figures/linear-reg/fig", suffix, "-", config_name, "-bedroom.png"),
         plot = fig_bedroom, width = 3000, height = height, units = "px", dpi = 250)
  
  ggsave(paste0("output/figures/linear-reg/fig", suffix, "-", config_name, "-room.png"), 
         plot = fig_room, width = 3000, height = height, units = "px", dpi = 250)
  
  return(list(bedroom = fig_bedroom, room = fig_room))
}

# =========================
# GENERATE ALL FIGURES
# =========================

# Generate existing figures
figA10A <- create_figure_set("birthplace", figure_configs$birthplace, "A10A")
figA10B <- create_figure_set("race_ethnicity", figure_configs$race_ethnicity, "A10B") 
figA10C <- create_figure_set("tenure", figure_configs$tenure, "A10C")
figA10D <- create_figure_set("race_ethnicity_endow", figure_configs$race_ethnicity_endow, "A10D")
figA10E <- create_figure_set("education", figure_configs$education, "A10E")
figA10F <- create_figure_set("age", figure_configs$age, "A10F")

