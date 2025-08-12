# src/figures/linear-reg/table-A01-export.R

library(dplyr)
kob_output <- readRDS("throughput/kob_output.rds")

# ---------- helpers ----------
stars <- function(est, se) {
  z <- abs(est / se)
  case_when(
    z >= 3.290527 ~ "***",   # 99.9%
    z >= 2.575829 ~ "**",    # 99%
    z >= 1.959964 ~ "*",     # 95%
    TRUE ~ ""
  )
}

# returns ONE tidy table for an outcome (p, b, r, ppbr, ppr)
build_table <- function(df) {
  # Intercept (TOTAL CHANGE via u on intercept row)
  intercept <- df %>%
    filter(term == "(Intercept)") %>%
    transmute(
      section  = "INTERCEPT",
      label    = "TOTAL",
      estimate = u,
      se       = u_se,
      stars    = stars(estimate, se)
    )
  
  # Coefficients by dimension
  coeff_by_dim <- df %>%
    filter(term != "(Intercept)") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(c, na.rm = TRUE),
      se       = sqrt(sum(c_se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    mutate(section = "COEFFICIENTS",
           label   = variable,
           stars   = stars(estimate, se)) %>%
    select(section, label, estimate, se, stars)
  
  coeff_total <- coeff_by_dim %>%
    summarise(
      section  = "COEFFICIENTS",
      label    = "TOTAL",
      estimate = sum(estimate, na.rm = TRUE),
      se       = sqrt(sum(se^2, na.rm = TRUE))
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # Endowments by dimension
  endow_by_dim <- df %>%
    filter(term != "(Intercept)") %>%
    group_by(variable) %>%
    summarise(
      estimate = sum(e, na.rm = TRUE),
      se       = sqrt(sum(e_se^2, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    mutate(section = "ENDOWMENTS",
           label   = variable,
           stars   = stars(estimate, se)) %>%
    select(section, label, estimate, se, stars)
  
  endow_total <- endow_by_dim %>%
    summarise(
      section  = "ENDOWMENTS",
      label    = "TOTAL",
      estimate = sum(estimate, na.rm = TRUE),
      se       = sqrt(sum(se^2, na.rm = TRUE))
    ) %>%
    mutate(stars = stars(estimate, se))
  
  # Final table: INTERCEPT total, COEF total + dims, ENDOW total + dims
  bind_rows(
    intercept,
    coeff_total,
    coeff_by_dim,
    endow_total,
    endow_by_dim
  )
}

# ---------- build tables (no files yet) ----------
tbl_p    <- build_table(kob_output$p)
tbl_b    <- build_table(kob_output$b)
tbl_r    <- build_table(kob_output$r)
tbl_ppbr <- build_table(kob_output$ppbr)
tbl_ppr  <- build_table(kob_output$ppr)

# Inspect in console if you want:
# dplyr::glimpse(tbl_p); dplyr::glimpse(tbl_b); ...

# ---------- now write one CSV per outcome ----------
dir.create("output/tables/table_A01", recursive = TRUE, showWarnings = FALSE)

write.csv(tbl_p,    "output/tables/table_A01/p.csv",    row.names = FALSE)
write.csv(tbl_b,    "output/tables/table_A01/b.csv",    row.names = FALSE)
write.csv(tbl_r,    "output/tables/table_A01/r.csv",    row.names = FALSE)
write.csv(tbl_ppbr, "output/tables/table_A01/ppbr.csv", row.names = FALSE)
write.csv(tbl_ppr,  "output/tables/table_A01/ppr.csv",  row.names = FALSE)
