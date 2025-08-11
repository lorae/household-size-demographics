# src/figures/figA10-usborn-coeff-only.R
# Coefficients-only breakdown for US-born (solid total + dotted components)
# Uses manual x-axis limits per outcome.

library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

source("src/utils/kob_panels.R")
kob_output <- readRDS("throughput/kob_output.rds")

# ----- manual x-axis limits (edit these) -----
xlims <- list(
  persons           = c(-0.10, 0.10),
  bedrooms          = c(-0.10, 0.40),
  ppbr              = c(-0.3, 0.15),
  rooms             = c(-0.10, 0.40),
  persons_per_room  = c(-0.15, 0.10)
)

# --- label helper: prefer VALUE over TERM ---
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

# --- build one coefficients-only panel for an outcome ---
build_usborn_coeff_column <- function(kob_df, title,
                                      x_limits,
                                      show_xlab = TRUE, show_ylab = TRUE,
                                      x_lab = "Contribution to Outcome Gap") {
  
  # per-level rows (dotted)
  lev <- prep_subgroup_panel(
    kob_df, variable = "us_born", part = "coeff",
    level_label_fun = label_usborn, level_order = usborn_order,
    include_total = FALSE
  )
  
  # solid total row (sum of the two levels)
  total_row <- lev |>
    summarise(
      label = "Total Birthplace",
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
    show_ytext = TRUE,
    label_order = c("Total Birthplace", usborn_order)
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 2)))
}

# ----- figure A08: Persons, Bedrooms, PPBR -----
col_p    <- build_usborn_coeff_column(kob_output$p,    "Number of Persons",
                                      x_limits = xlims$persons,
                                      show_xlab = FALSE, show_ylab = TRUE)
col_b    <- build_usborn_coeff_column(kob_output$b,    "Number of Bedrooms",
                                      x_limits = xlims$bedrooms,
                                      show_xlab = TRUE,  show_ylab = FALSE)
col_ppbr <- build_usborn_coeff_column(kob_output$ppbr, "Persons per Bedroom",
                                      x_limits = xlims$ppbr,
                                      show_xlab = FALSE, show_ylab = FALSE)

figA10A <- (col_p | col_b | col_ppbr)

# ----- figure A09: Persons, Rooms, PPR -----
col_r    <- build_usborn_coeff_column(kob_output$r,    "Number of Rooms",
                                      x_limits = xlims$rooms,
                                      show_xlab = TRUE,  show_ylab = FALSE)
col_ppr  <- build_usborn_coeff_column(kob_output$ppr,  "Persons per Room",
                                      x_limits = xlims$persons_per_room,
                                      show_xlab = FALSE, show_ylab = FALSE)

figA10A_alt <- (col_p | col_r | col_ppr)

# ----- save -----
ggsave("output/figures/linear-reg/figA10A-usborn-coeffs-bedroom.png",
       plot = figA10A, width = 3000, height = 500, units = "px", dpi = 250)

ggsave("output/figures/linear-reg/figA10A-alt-usborn-coeffs-room.png",
       plot = figA10A_alt, width = 3000, height = 500, units = "px", dpi = 250)


