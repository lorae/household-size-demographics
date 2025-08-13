# src/figures/linear-reg/table-A01-export.R

# ---------- Libraries ----------
library(dplyr)
library(tidyr)
library(tibble)
library(glue)
library(writexl)

# ---------- Inputs ----------
kob_output <- readRDS("throughput/kob_output.rds")
aggregates <- readRDS("throughput/aggregates.rds")

# ---------- Pretty labels + fixed order ----------
pretty_map <- c(
  "us_born"                 = "Birthplace",
  "RACE_ETH_bucket"         = "Race/ Ethnicity",
  "INCTOT_cpiu_2010_bucket" = "Income (2010 CPI-U Adj.)",
  "gender"                  = "Sex",
  "tenure"                  = "Tenure",
  "EDUC_bucket"             = "Educational attainment",
  "AGE_bucket"              = "Age",
  "cpuma"                   = "CPUMA"
)
var_order <- names(pretty_map)

# ---------- Helpers ----------
stars <- function(est, se) {
  z <- abs(est / se)
  dplyr::case_when(
    z >= 3.290527 ~ "***",  # 99.9%
    z >= 2.575829 ~ "**",   # 99%
    z >= 1.959964 ~ "*",    # 95%
    TRUE ~ ""
  )
}

fmt_cell <- function(est, se, star) {
  paste0(sprintf("%.3f", est), star, "\n(", sprintf("%.4f", se), ")")
}

# percentages (100% = total change): 1 decimal for est, 2 for SEs
fmt_cell_pct <- function(est_pct, se_pct, star) {
  paste0(sprintf("%.1f%%", est_pct), star, "\n(", sprintf("%.2f%%", se_pct), ")")
}

# allow either set of SE column names in aggregates
pick_col <- function(df, prefer, alt) {
  if (prefer %in% names(df)) df[[prefer]] else df[[alt]]
}

# Build a small lookup from aggregates (supports two naming schemes for SEs)
agg_lookup <- tibble(
  abbrev_variable = aggregates$abbrev_variable,
  mean_2000       = aggregates$mean_2000,
  mean_2019       = aggregates$mean_2019,
  mean_2000_se    = if ("mean_2000_se" %in% names(aggregates)) aggregates$mean_2000_se else pick_col(aggregates, "mean_2000_se", "se_2000"),
  mean_2019_se    = if ("mean_2019_se" %in% names(aggregates)) aggregates$mean_2019_se else pick_col(aggregates, "mean_2019_se", "se_2019")
)

# ---------- Table builder ----------
build_table <- function(df, outcome_abbrev) {
  # Match aggregates row for this outcome
  agg_row <- agg_lookup %>% filter(abbrev_variable == outcome_abbrev)
  
  # Top aggregate rows
  top_rows <- tibble(
    section  = "AGGREGATE",
    label    = c("2000 Value", "2019 Value", "Change"),
    estimate = c(
      agg_row$mean_2000,
      agg_row$mean_2019,
      agg_row$mean_2019 - agg_row$mean_2000
    ),
    se       = c(
      agg_row$mean_2000_se,
      agg_row$mean_2019_se,
      sqrt(agg_row$mean_2000_se^2 + agg_row$mean_2019_se^2)
    )
  ) %>%
    mutate(stars = stars(estimate, se),
           value_fmt = fmt_cell(estimate, se, stars))
  
  # Intercept
  intercept <- df %>%
    filter(term == "(Intercept)") %>%
    transmute(
      section  = "INTERCEPT",
      label    = "INTERCEPT",
      estimate = u,
      se       = u_se
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # Coefficient contributions by dimension
  coeff_by_dim <- df %>%
    filter(term != "(Intercept)") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(c, na.rm = TRUE),
      se       = sqrt(sum(c_se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    filter(variable %in% var_order) %>%
    mutate(
      label   = unname(pretty_map[variable]),
      section = "COEFFICIENTS"
    ) %>%
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
  
  # Endowment contributions by dimension
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
  
  # Combine + format
  bind_rows(
    top_rows,
    intercept,
    coeff_total,
    coeff_by_dim,
    endow_total,
    endow_by_dim
  ) %>%
    mutate(value_fmt = fmt_cell(estimate, se, stars))
}

# ---------- Build per-outcome tables ----------
tbl_p    <- build_table(kob_output$p,    "p")
tbl_b    <- build_table(kob_output$b,    "b")
tbl_r    <- build_table(kob_output$r,    "r")
tbl_ppbr <- build_table(kob_output$ppbr, "ppbr")
tbl_ppr  <- build_table(kob_output$ppr,  "ppr")

# ---------- Output dir ----------
dir.create("output/tables/table_A01", recursive = TRUE, showWarnings = FALSE)

# ---------- Write tidy CSVs (per outcome) ----------
write.csv(tbl_p,    "output/tables/table_A01/p.csv",    row.names = FALSE)
write.csv(tbl_b,    "output/tables/table_A01/b.csv",    row.names = FALSE)
write.csv(tbl_r,    "output/tables/table_A01/r.csv",    row.names = FALSE)
write.csv(tbl_ppbr, "output/tables/table_A01/ppbr.csv", row.names = FALSE)
write.csv(tbl_ppr,  "output/tables/table_A01/ppr.csv",  row.names = FALSE)

# ---------- Stack all outcomes ----------
tbl_all <- bind_rows(
  tbl_p    %>% mutate(outcome = "p"),
  tbl_b    %>% mutate(outcome = "b"),
  tbl_r    %>% mutate(outcome = "r"),
  tbl_ppbr %>% mutate(outcome = "ppbr"),
  tbl_ppr  %>% mutate(outcome = "ppr")
)

# Level sheet: keep formatted values
tbl_all_fmt <- tbl_all %>% select(outcome, label, value_fmt)

# ---------- Validation (sum of parts == aggregate change) ----------
validate_table <- function(tbl, outcome_name, tol = 1e-8, stop_if_fail = FALSE) {
  change      <- tbl %>% filter(section == "AGGREGATE",    label == "Change")       %>% pull(estimate) %>% dplyr::first()
  intercept   <- tbl %>% filter(section == "INTERCEPT",    label == "INTERCEPT")    %>% pull(estimate) %>% dplyr::first()
  coeff_total <- tbl %>% filter(section == "COEFFICIENTS", label == "COEFFICIENTS") %>% pull(estimate) %>% dplyr::first()
  endow_total <- tbl %>% filter(section == "ENDOWMENTS",   label == "ENDOWMENTS")   %>% pull(estimate) %>% dplyr::first()
  
  sum_parts <- intercept + coeff_total + endow_total
  diff      <- sum_parts - change
  pass      <- dplyr::near(sum_parts, change, tol = tol)
  
  out <- tibble(
    outcome          = outcome_name,
    aggregate_change = change,
    sum_of_parts     = sum_parts,
    difference       = diff,
    pass             = pass
  )
  
  if (isTRUE(stop_if_fail) && !pass) {
    stop(glue("Validation failed for '{outcome_name}': sum(parts)={sum_parts}, change={change}, diff={diff}"))
  }
  out
}

val_summary <- bind_rows(
  validate_table(tbl_p,    "p"),
  validate_table(tbl_b,    "b"),
  validate_table(tbl_r,    "r"),
  validate_table(tbl_ppbr, "ppbr"),
  validate_table(tbl_ppr,  "ppr")
)

# ---------- Percentages-of-total-change sheet (100% = total change) ----------
changes <- tbl_all %>%
  filter(section == "AGGREGATE", label == "Change") %>%
  select(outcome, change = estimate)

tbl_all_prop_pct <- tbl_all %>%
  select(outcome, section, label, estimate, se, stars) %>%
  left_join(changes, by = "outcome") %>%
  mutate(
    denom_mag = ifelse(is.finite(change) & abs(change) > 1e-12, abs(change), NA_real_),
    est_pct   = (estimate / denom_mag) * 100,          # <- use abs(change)
    se_pct    = (se / denom_mag) * 100,                # SEs stay positive
    # Force the 'Change' row to be exactly +/-100 (avoids tiny rounding drift)
    est_pct   = ifelse(section == "AGGREGATE" & label == "Change" & !is.na(change),
                       sign(change) * 100, est_pct),
    value_fmt = fmt_cell_pct(est_pct, se_pct, stars)
  ) %>%
  select(outcome, label, value_fmt)

# ---------- Write Excel (two sheets) ----------
write_xlsx(
  list(
    "levels"                        = tbl_all_fmt,
    "proportions_of_total_change_%" = tbl_all_prop_pct
  ),
  "output/tables/table_A01/all_outcomes_formatted.xlsx"
)
