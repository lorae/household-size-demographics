# src/figures/linear-reg/table-A01-export.R

library(dplyr)
library(tidyr)
kob_output <- readRDS("throughput/kob_output.rds")

# ---------- pretty labels + fixed order ----------
# Map your internal variable names -> pretty labels, in the exact order you want.
pretty_map <- c(
  "us_born"                    = "Birthplace",
  "RACE_ETH_bucket"            = "Race/ Ethnicity",
  "INCTOT_cpiu_2010_bucket"    = "Income (2010 CPI-U Adj.)",
  "gender"                     = "Sex",
  "tenure"                     = "Tenure",
  "EDUC_bucket"                = "Educational attainment",
  "AGE_bucket"                 = "Age",
  "CPUMA"                      = "CPUMA"
)
var_order <- names(pretty_map)  # preserve this order

# ---------- helpers ----------
stars <- function(est, se) {
  z <- abs(est / se)
  dplyr::case_when(
    z >= 3.290527 ~ "***",   # 99.9%
    z >= 2.575829 ~ "**",    # 99%
    z >= 1.959964 ~ "*",     # 95%
    TRUE ~ ""
  )
}

fmt_cell <- function(est, se, star) {
  paste0(sprintf("%.3f", est), star, "\n(", sprintf("%.4f", se), ")")
}

# returns ONE tidy table for an outcome (p, b, r, ppbr, ppr)
build_table <- function(df) {
  # Intercept (TOTAL CHANGE via u on intercept row)
  intercept <- df %>%
    filter(term == "(Intercept)") %>%
    transmute(
      section  = "INTERCEPT",
      label    = "INTERCEPT",
      estimate = u,
      se       = u_se
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # Coefficients by dimension (rename + order)
  coeff_by_dim <- df %>%
    filter(term != "(Intercept)") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(c, na.rm = TRUE),
      se       = sqrt(sum(c_se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    # keep only variables we know/how we want to show
    filter(variable %in% var_order) %>%
    mutate(
      label   = unname(pretty_map[variable]),
      section = "COEFFICIENTS"
    ) %>%
    # enforce desired order
    mutate(label = factor(label, levels = unname(pretty_map[var_order]))) %>%
    arrange(label) %>%
    mutate(stars = stars(estimate, se)) %>%
    select(section, label, estimate, se, stars)
  
  coeff_total <- coeff_by_dim %>%
    summarise(
      section  = "COEFFICIENTS",
      label    = "COEFFICIENTS",
      estimate = sum(estimate, na.rm = TRUE),
      se       = sqrt(sum(se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # Endowments by dimension (rename + order)
  endow_by_dim <- df %>%
    filter(term != "(Intercept)") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(e, na.rm = TRUE),
      se       = sqrt(sum(e_se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    filter(variable %in% var_order) %>%
    mutate(
      label   = unname(pretty_map[variable]),
      section = "ENDOWMENTS"
    ) %>%
    mutate(label = factor(label, levels = unname(pretty_map[var_order]))) %>%
    arrange(label) %>%
    mutate(stars = stars(estimate, se)) %>%
    select(section, label, estimate, se, stars)
  
  endow_total <- endow_by_dim %>%
    summarise(
      section  = "ENDOWMENTS",
      label    = "ENDOWMENTS",
      estimate = sum(estimate, na.rm = TRUE),
      se       = sqrt(sum(se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # ---- final ordering EXACTLY as requested ----
  out <- bind_rows(
    intercept,          # INTERCEPT single total
    coeff_total,        # COEFFICIENTS total
    coeff_by_dim,       # then coefficients by dimension (ordered)
    endow_total,        # ENDOWMENTS total
    endow_by_dim        # then endowments by dimension (ordered)
  )
  
  # Add a formatted cell for Excel-friendly 2-column export
  out %>%
    mutate(value_fmt = fmt_cell(estimate, se, stars))
}

# ---------- build tables (no files yet) ----------
tbl_p    <- build_table(kob_output$p)
tbl_b    <- build_table(kob_output$b)
tbl_r    <- build_table(kob_output$r)
tbl_ppbr <- build_table(kob_output$ppbr)
tbl_ppr  <- build_table(kob_output$ppr)

# ---------- write full (tidy) tables ----------
dir.create("output/tables/table_A01", recursive = TRUE, showWarnings = FALSE)

write.csv(tbl_p,    "output/tables/table_A01/p.csv",    row.names = FALSE)
write.csv(tbl_b,    "output/tables/table_A01/b.csv",    row.names = FALSE)
write.csv(tbl_r,    "output/tables/table_A01/r.csv",    row.names = FALSE)
write.csv(tbl_ppbr, "output/tables/table_A01/ppbr.csv", row.names = FALSE)
write.csv(tbl_ppr,  "output/tables/table_A01/ppr.csv",  row.names = FALSE)

library(writexl)  # for writing Excel

# Build all tables with outcome labels
tbl_all <- bind_rows(
  tbl_p    %>% mutate(outcome = "p"),
  tbl_b    %>% mutate(outcome = "b"),
  tbl_r    %>% mutate(outcome = "r"),
  tbl_ppbr %>% mutate(outcome = "ppbr"),
  tbl_ppr  %>% mutate(outcome = "ppr")
)

# You can choose tidy version...
write_xlsx(tbl_all, "output/tables/table_A01/all_outcomes_tidy.xlsx")

# ...or Excel-friendly version with just label/value_fmt
tbl_all_fmt <- tbl_all %>%
  select(outcome, label, value_fmt)

write_xlsx(tbl_all_fmt, "output/tables/table_A01/all_outcomes_formatted.xlsx")
