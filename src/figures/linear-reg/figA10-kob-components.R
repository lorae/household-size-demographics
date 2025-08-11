# src/figures/figA10-usborn-coeff-only.R
# Coefficients-only breakdowns for Birthplace and Race/Ethnicity
# Uses manual x-axis limits per outcome, hides y tick labels on middle/right panels,
# and inserts small spacers between columns for legibility.

library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

source("src/utils/kob_panels.R")
kob_output <- readRDS("throughput/kob_output.rds")

# ----- manual x-axis limits (edit these) -----
xlims <- list(
  persons           = c(-0.10, 0.12),
  bedrooms          = c(-0.10, 0.40),
  ppbr              = c(-0.30, 0.15),
  rooms             = c(-0.10, 0.40),
  persons_per_room  = c(-0.15, 0.10)
)

# =========================
# LABEL HELPERS
# =========================

# --- Birthplace: prefer VALUE over TERM ---
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
usborn_order <- c("U.S. Born","Foreign Born")

# --- Race / Ethnicity level labels ---
# Simple label_raceeth function with direct mapping
label_raceeth <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  # Direct mapping of full term names to pretty labels
  dplyr::recode(term_val,
                "RACE_ETH_bucketAAPI"        = "AAPI",
                "RACE_ETH_bucketAIAN"        = "AIAN",
                "RACE_ETH_bucketHispanic"    = "Hispanic", 
                "RACE_ETH_bucketBlack"       = "Black",
                "RACE_ETH_bucketWhite"       = "White",
                "RACE_ETH_bucketMultiracial" = "Multiracial",
                "RACE_ETH_bucketOther"       = "Other",
                .default = term_val  # If no match, keep original
  )
}
# Edit the order if you prefer a different display
race_order <- c("U.S. Born") # placeholder to avoid empty; will be replaced after we see labels
# We'll compute race_order dynamically per dataset below, so we don't
# assume which levels exist.

# =========================
# BUILDERS
# =========================

# --- Birthplace coefficients-only column ---
build_usborn_coeff_column <- function(kob_df, title,
                                      x_limits,
                                      show_xlab = TRUE,
                                      show_ylab = TRUE,   # axis title
                                      show_ytext = TRUE,  # tick labels (U.S. Born, Foreign Born)
                                      x_lab = "Contribution to Outcome Gap") {
  lev <- prep_subgroup_panel(
    kob_df, variable = "us_born", part = "coeff",
    level_label_fun = label_usborn, level_order = usborn_order,
    include_total = FALSE
  )
  
  total_row <- lev |>
    summarise(
      label = "TOTAL: Birthplace",
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  df <- bind_rows(total_row, lev)
  
  plot_kob_panel(
    df = df, y_lab = "Coefficients", fill_hex = kob_cols$coeff,
    show_xlab = show_xlab, show_ylab = show_ylab,
    x_lab = x_lab, x_limits = x_limits,
    show_ytext = show_ytext,
    label_order = c("TOTAL: Birthplace", usborn_order)
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 2)))
}

# --- Race/Ethnicity coefficients-only column ---
# --- Race/Ethnicity coefficients-only column ---
build_raceeth_coeff_column <- function(kob_df, title,
                                       x_limits,
                                       show_xlab = TRUE,
                                       show_ylab = TRUE,
                                       show_ytext = TRUE,
                                       x_lab = "Contribution to Outcome Gap") {
  lev <- prep_subgroup_panel(
    kob_df, variable = "RACE_ETH_bucket", part = "coeff",
    level_label_fun = label_raceeth, level_order = NULL,
    include_total = FALSE
  )
  
  # Filter to only keep White, Hispanic, and Black
  lev <- lev |> 
    filter(label %in% c("White", "Hispanic", "Black"))
  
  total_row <- lev |>
    summarise(
      label = "TOTAL: Race / Ethnicity",
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  df <- bind_rows(total_row, lev)
  
  # Set specific order: Total, White, Hispanic, Black
  label_order <- c("TOTAL: Race / Ethnicity", "Black", "Hispanic", "White")
  
  plot_kob_panel(
    df = df, y_lab = "Coefficients", fill_hex = kob_cols$coeff,
    show_xlab = show_xlab, show_ylab = show_ylab,
    x_lab = x_lab, x_limits = x_limits,
    show_ytext = show_ytext,
    label_order = label_order
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 2)))
}

# Small spacer to increase the gap between columns
spacer <- patchwork::plot_spacer()

# =========================
# A10A — Birthplace
# =========================

# Persons, Bedrooms, PPBR
col_p    <- build_usborn_coeff_column(kob_output$p,    "Number of Persons",
                                      x_limits = xlims$persons,
                                      show_xlab = FALSE, show_ylab = TRUE,  show_ytext = TRUE)
col_b    <- build_usborn_coeff_column(kob_output$b,    "Number of Bedrooms",
                                      x_limits = xlims$bedrooms,
                                      show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
col_ppbr <- build_usborn_coeff_column(kob_output$ppbr, "Persons per Bedroom",
                                      x_limits = xlims$ppbr,
                                      show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10A <- (col_p | spacer | col_b | spacer | col_ppbr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# Persons, Rooms, PPR
col_r    <- build_usborn_coeff_column(kob_output$r,    "Number of Rooms",
                                      x_limits = xlims$rooms,
                                      show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
col_ppr  <- build_usborn_coeff_column(kob_output$ppr,  "Persons per Room",
                                      x_limits = xlims$persons_per_room,
                                      show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10A_alt <- (col_p | spacer | col_r | spacer | col_ppr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# =========================
# A10B — Race / Ethnicity
# =========================

# Persons, Bedrooms, PPBR
re_p    <- build_raceeth_coeff_column(kob_output$p,    "Number of Persons",
                                      x_limits = xlims$persons,
                                      show_xlab = FALSE, show_ylab = TRUE,  show_ytext = TRUE)
re_b    <- build_raceeth_coeff_column(kob_output$b,    "Number of Bedrooms",
                                      x_limits = xlims$bedrooms,
                                      show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
re_ppbr <- build_raceeth_coeff_column(kob_output$ppbr, "Persons per Bedroom",
                                      x_limits = xlims$ppbr,
                                      show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10B <- (re_p | spacer | re_b | spacer | re_ppbr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# Persons, Rooms, PPR
re_r   <- build_raceeth_coeff_column(kob_output$r,   "Number of Rooms",
                                     x_limits = xlims$rooms,
                                     show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
re_ppr <- build_raceeth_coeff_column(kob_output$ppr, "Persons per Room",
                                     x_limits = xlims$persons_per_room,
                                     show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10B_alt <- (re_p | spacer | re_r | spacer | re_ppr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# =========================
# Save
# =========================
ggsave("output/figures/linear-reg/figA10A-usborn-coeffs-bedroom.png",
       plot = figA10A, width = 3000, height = 500, units = "px", dpi = 250)

ggsave("output/figures/linear-reg/figA10A-alt-usborn-coeffs-room.png",
       plot = figA10A_alt, width = 3000, height = 500, units = "px", dpi = 250)

ggsave("output/figures/linear-reg/figA10B-raceeth-coeffs-bedroom.png",
       plot = figA10B, width = 3000, height = 600, units = "px", dpi = 250)

ggsave("output/figures/linear-reg/figA10B-alt-raceeth-coeffs-room.png",
       plot = figA10B_alt, width = 3000, height = 600, units = "px", dpi = 250)

# Add this to your existing code after the race/ethnicity section
# =========================
# TENURE LABEL HELPERS
# =========================

# --- Tenure level labels ---
label_tenure <- function(df) {
  if ("term" %in% names(df)) {
    term_val <- df$term
  } else {
    term_val <- as.character(df$value)
  }
  
  # Direct mapping for tenure terms based on your actual data
  dplyr::recode(term_val,
                "tenurehomeowner" = "Homeowner",
                "tenurerenter"    = "Renter",
                .default = term_val
  )
}

# =========================
# TENURE BUILDER
# =========================

# --- Tenure endowments-only column ---
build_tenure_endow_column <- function(kob_df, title,
                                      x_limits,
                                      show_xlab = TRUE,
                                      show_ylab = TRUE,
                                      show_ytext = TRUE,
                                      x_lab = "Contribution to Outcome Gap") {
  lev <- prep_subgroup_panel(
    kob_df, variable = "tenure", part = "endow",  # Changed to "tenure"
    level_label_fun = label_tenure, level_order = NULL,
    include_total = FALSE
  )
  
  total_row <- lev |>
    summarise(
      label = "TOTAL: Tenure",
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  df <- bind_rows(total_row, lev)
  
  # Set specific order: Total, Homeowner, Renter
  label_order <- c("TOTAL: Tenure", "Renter", "Homeowner")
  
  plot_kob_panel(
    df = df, y_lab = "Endowments", fill_hex = kob_cols$endow,
    show_xlab = show_xlab, show_ylab = show_ylab,
    x_lab = x_lab, x_limits = x_limits,
    show_ytext = show_ytext,
    label_order = label_order
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 2)))
}

# =========================
# A10C — Tenure
# =========================

# Persons, Bedrooms, PPBR
ten_p    <- build_tenure_endow_column(kob_output$p,    "Number of Persons",
                                      x_limits = xlims$persons,
                                      show_xlab = FALSE, show_ylab = TRUE,  show_ytext = TRUE)
ten_b    <- build_tenure_endow_column(kob_output$b,    "Number of Bedrooms",
                                      x_limits = xlims$bedrooms,
                                      show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
ten_ppbr <- build_tenure_endow_column(kob_output$ppbr, "Persons per Bedroom",
                                      x_limits = xlims$ppbr,
                                      show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10C <- (ten_p | spacer | ten_b | spacer | ten_ppbr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# Persons, Rooms, PPR
ten_r   <- build_tenure_endow_column(kob_output$r,   "Number of Rooms",
                                     x_limits = xlims$rooms,
                                     show_xlab = TRUE,  show_ylab = FALSE, show_ytext = FALSE)
ten_ppr <- build_tenure_endow_column(kob_output$ppr, "Persons per Room",
                                     x_limits = xlims$persons_per_room,
                                     show_xlab = FALSE, show_ylab = FALSE, show_ytext = FALSE)

figA10C_alt <- (ten_p | spacer | ten_r | spacer | ten_ppr) +
  plot_layout(widths = c(1, 0.08, 1, 0.08, 1))

# =========================
# Save Tenure Plots
# =========================
ggsave("output/figures/linear-reg/figA10C-tenure-endow-bedroom.png",
       plot = figA10C, width = 3000, height = 500, units = "px", dpi = 250)

ggsave("output/figures/linear-reg/figA10C-alt-tenure-endow-room.png",
       plot = figA10C_alt, width = 3000, height = 500, units = "px", dpi = 250)