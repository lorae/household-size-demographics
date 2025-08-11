# #src/figures/figA06-A07-kob-decomp-bars.R
# Produce bar charts showing the kob decomposition.
#
# Input: throughput/kob_output.rds
# Output: 
# - output/figures/linear-reg/figA06-kob-decomp-bars-bedroom.png
# - output/figures/linear-reg/figA07-kob-decomp-bars-bedroom.png
#
# TODO: write unit tests for functions
# TODO: optionally turn off x-axis label, make the facet names like Intercept not get cutoff

# ----- Step 0: Config ----- 
library("patchwork")
library("ggplot2")
library("dplyr")
library("tidyr")

source("src/utils/kob_panels.R")
kob_output <- readRDS("throughput/kob_output.rds")


# For tidying (split_term_column)
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

pretty_labels <- c(
  "us_born"                   = "U.S. Born",
  "RACE_ETH_bucket"           = "Race / Ethnicity",
  "INCTOT_cpiu_2010_bucket"   = "Income",
  "cpuma"                     = "CPUMA",
  "gender"                    = "Sex",
  "tenure"                    = "Tenure",
  "EDUC_bucket"               = "Education",
  "AGE_bucket"                = "Age"
)

# build one outcome column (two stacked panels) ----------
build_outcome_column <- function(kob_df, title, varnames, pretty_labels,
                                 show_xlab = TRUE,
                                 show_ylab = TRUE,
                                 show_title = TRUE,
                                 x_lab = "Contribution to Outcome Gap",
                                 show_bar_labels = TRUE,
                                 bar_label_order = NULL) {  
  
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
                            x_lab = x_lab, x_limits = xlim_shared,
                            show_ytext = show_bar_labels,
                            label_order = bar_label_order)
  
  p_endow <- plot_kob_panel(df_endow, "Endowments",  kob_cols$endow,
                            show_xlab = FALSE, show_ylab = show_ylab,
                            x_lab = x_lab, x_limits = xlim_shared,
                            show_ytext = show_bar_labels,
                            label_order = bar_label_order)
  
  p_intercept <- plot_kob_panel(df_int, "Intercept", kob_cols$intercept,
                                show_xlab = FALSE, show_ylab = show_ylab,
                                x_lab = x_lab, x_limits = xlim_shared,
                                bar_width = 0.3, err_height = 0.10, err_size = 0.4,
                                show_ytext = show_bar_labels)    # (order irrelevant; single bar)
  
  p_total <- plot_kob_panel(df_total, "Total", kob_cols$total,
                            show_xlab = show_xlab, show_ylab = show_ylab,
                            x_lab = x_lab, x_limits = xlim_shared,
                            bar_width = 0.3, show_errorbars = FALSE,
                            y_fontface = "bold",
                            show_ytext = show_bar_labels)        # (order irrelevant; single bar)
  
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

# Global desired order for the bars inside Coefficients/Endowments
bar_order <- c(
  "U.S. Born", 
  "Race / Ethnicity", 
  "Income", 
  "CPUMA",
  "Sex", 
  "Tenure",
  "Education",
  "Age"
  ) |> rev()

# (These must match the pretty labels produced by `prep_topline_panel`.)

col_p    <- build_outcome_column(kob_output$p,    "Number of Persons",
                                 varnames_dict, pretty_labels,
                                 show_xlab = TRUE,  show_ylab = TRUE,
                                 show_title = TRUE,
                                 show_bar_labels = TRUE,
                                 bar_label_order = bar_order)

col_b    <- build_outcome_column(kob_output$b,    "Number of Bedrooms",
                                 varnames_dict, pretty_labels,
                                 show_xlab = TRUE,  show_ylab = FALSE,
                                 show_title = TRUE,
                                 show_bar_labels = FALSE,   
                                 bar_label_order = bar_order)

col_ppbr <- build_outcome_column(kob_output$ppbr, "Persons per Bedroom",
                                 varnames_dict, pretty_labels,
                                 show_xlab = TRUE,  show_ylab = FALSE,
                                 show_title = TRUE,
                                 show_bar_labels = FALSE, 
                                 bar_label_order = bar_order)


col_r    <- build_outcome_column(kob_output$r,    "Number of Rooms",
                                 varnames_dict, pretty_labels,
                                 show_xlab = TRUE,  show_ylab = FALSE,
                                 show_title = TRUE,
                                 show_bar_labels = FALSE,   
                                 bar_label_order = bar_order)

col_ppr <- build_outcome_column(kob_output$ppr, "Persons per Room",
                                 varnames_dict, pretty_labels,
                                 show_xlab = TRUE,  show_ylab = FALSE,
                                 show_title = TRUE,
                                 show_bar_labels = FALSE, 
                                 bar_label_order = bar_order)

figA06 <- (col_p | col_b | col_ppbr)

figA07 <- (col_p | col_r | col_ppr)


ggsave("output/figures/linear-reg/figA06-kob-decomp-bars-bedroom.png",
       plot = figA06, width = 3000, height = 4000, units = "px", dpi = 200)

ggsave("output/figures/linear-reg/figA07-kob-decomp-bars-room.png",
       plot = figA07, width = 3000, height = 4000, units = "px", dpi = 200)

