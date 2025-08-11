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
  persons           = c(-0.10, 0.12),
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
  height <- ifelse(config_name == "race_ethnicity", 600, 500)
  
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

# To add new figures, just add to figure_configs and call:
# figA10D <- create_figure_set("new_variable", figure_configs$new_variable, "A10D")