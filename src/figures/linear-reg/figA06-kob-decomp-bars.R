# #src/figures/fig07-kob-decomp-bars.R
# Produce bar charts showing the kob decomposition.
#
# Input: throughput/kob_output.rds
# Output: output/figures/fig07-kob-decomp-bars.png, output/figures/fig07a-kob-decomp-bars.png
#
# TODO: write unit tests for functions
# TODO: optionally turn off x-axis label, make the facet names like Intercept not get cutoff

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")
library("dplyr")
library("tidyr")

source("src/utils/kob_panels.R")

# build one outcome column (two stacked panels) ----------
build_outcome_column <- function(kob_df, title, varnames, pretty_labels,
                                 show_xlab = TRUE,
                                 show_ylab = TRUE,
                                 show_title = TRUE,
                                 x_lab = "Contribution to Outcome Gap") {
  
  # Prep data
  df_coeff <- prep_topline_panel(kob_df, varnames, "Coefficients", pretty_labels)
  df_endow <- prep_topline_panel(kob_df, varnames, "Endowments",  pretty_labels)
  df_int   <- prep_intercept_panel(kob_df)
  df_total <- prep_total_panel(kob_df, varnames)
  
  # Shared x-limits across all four panels
  xmin <- min(c(df_coeff$estimate - df_coeff$se,
                df_endow$estimate - df_endow$se,
                df_int$estimate   - df_int$se,
                df_total$estimate), na.rm = TRUE)
  xmax <- max(c(df_coeff$estimate + df_coeff$se,
                df_endow$estimate + df_endow$se,
                df_int$estimate   + df_int$se,
                df_total$estimate), na.rm = TRUE)
  xlim_shared <- c(xmin, xmax)
  
  p_coeff <- plot_kob_panel(df_coeff, "Coefficients", kob_cols$coeff,
                            show_xlab = FALSE, show_ylab = show_ylab,
                            x_lab = x_lab, x_limits = xlim_shared)
  
  p_endow <- plot_kob_panel(df_endow, "Endowments",  kob_cols$endow,
                            show_xlab = FALSE, show_ylab = show_ylab,
                            x_lab = x_lab, x_limits = xlim_shared)
  
  p_intercept <- plot_kob_panel(df_int, "Intercept", kob_cols$intercept,
                                show_xlab = FALSE, show_ylab = show_ylab,
                                x_lab = x_lab, x_limits = xlim_shared,
                                bar_width = 0.3, err_height = 0.10, err_size = 0.4)
  
  # TOTAL: solid gray, bold label, NO error bars
  p_total <- plot_kob_panel(df_total, "Total", kob_cols$total,
                            show_xlab = show_xlab, show_ylab = show_ylab,
                            x_lab = x_lab, x_limits = xlim_shared,
                            bar_width = 0.3, show_errorbars = FALSE,
                            y_fontface = "bold")
  
  # Stack with smaller heights for Intercept and TOTAL
  col_body <- (p_coeff / p_endow / p_intercept / p_total) +
    patchwork::plot_layout(heights = c(1, 1, 0.32, 0.32))
  
  if (!show_title) return(col_body)
  
  title_strip <- ggplot() + theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = title,
             hjust = 0.5, vjust = 0.5, size = 6, fontface = "plain") +
    theme(plot.margin = margin(b = 2, t = 0))
  
  (title_strip / col_body) + patchwork::plot_layout(heights = c(0.06, 1))
}



# existing data, labels...
# Titles on, x‑axis label only on bottom panels, show y facet labels
col_p    <- build_outcome_column(kob_output$p,    "Number of Persons",  varnames_dict, pretty_labels,
                                 show_xlab = TRUE, show_ylab = TRUE,  show_title = TRUE)
col_b    <- build_outcome_column(kob_output$b,    "Number of Bedrooms", varnames_dict, pretty_labels,
                                 show_xlab = TRUE, show_ylab = FALSE,  show_title = TRUE)
col_ppbr <- build_outcome_column(kob_output$ppbr, "Persons per Bedroom",varnames_dict, pretty_labels,
                                 show_xlab = TRUE, show_ylab = FALSE,  show_title = TRUE)


figA06 <- (col_p | col_b | col_ppbr)
figA06

ggsave("output/figures/linear-reg/figA06-kob-decomp-bars-bedroom.png",
       plot = figA06, width = 3000, height = 3000, units = "px", dpi = 200)

