# Colors
kob_cols <- list(
  coeff     = "#E69F00",
  endow     = "#56B4E9",
  intercept = "#805b87",
  total     = "gray50"
)

# =============== ATOMIC PANEL PLOTTER =================
# Expects columns: label, estimate, se, is_total (logical)
plot_kob_panel <- function(df, y_lab, fill_hex,
                           show_xlab = TRUE, show_ylab = TRUE,
                           x_lab = "Contribution to Outcome Gap",
                           bar_width = 0.8,
                           err_height = 0.25,
                           err_size = 0.5,
                           x_limits = NULL,
                           show_errorbars = TRUE,
                           y_fontface = "plain") {
  
  stopifnot(all(c("label","estimate","se","is_total") %in% names(df)))
  
  ord <- df |> dplyr::arrange(is_total, label) |> dplyr::pull(label) |> unique()
  df <- df |> dplyr::mutate(label = factor(label, levels = ord))
  
  g <- ggplot() +
    geom_col(
      data = dplyr::filter(df, !is_total),
      aes(x = estimate, y = label),
      width = bar_width, fill = fill_hex, alpha = 0.30,
      color = "black", linetype = "dotted"
    ) +
    geom_col(
      data = dplyr::filter(df, is_total),
      aes(x = estimate, y = label),
      width = bar_width, fill = fill_hex
    ) +
    theme_minimal(base_size = 13) +
    labs(x = if (show_xlab) x_lab else NULL,
         y = if (show_ylab) y_lab else NULL) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(face = y_fontface)
    )
  
  if (show_errorbars && any(!is.na(df$se))) {
    g <- g + geom_errorbarh(
      data = df,
      aes(y = label, xmin = estimate - se, xmax = estimate + se),
      height = err_height, size = err_size, color = "black", na.rm = TRUE
    )
  }
  if (!is.null(x_limits)) g <- g + scale_x_continuous(limits = x_limits)
  g
}

# Intercept panel data
prep_intercept_panel <- function(kob_df) {
  kob_df |>
    dplyr::filter(term == "(Intercept)") |>
    dplyr::summarise(
      label = "Intercept",
      estimate = sum(u, na.rm = TRUE),
      se = sqrt(sum(u_se^2, na.rm = TRUE)),
      is_total = TRUE,  # solid style
      .groups = "drop"
    )
}

# TOTAL panel data (sum of selected variables' E + C plus intercept U)
prep_total_panel <- function(kob_df, varnames) {
  df_vars <- kob_df |> dplyr::filter(variable %in% varnames)
  total_e <- sum(df_vars$e, na.rm = TRUE)
  total_c <- sum(df_vars$c, na.rm = TRUE)
  total_u <- kob_df |> dplyr::filter(term == "(Intercept)") |> dplyr::summarise(v = sum(u, na.rm = TRUE)) |> dplyr::pull(v)
  tibble::tibble(
    label = "TOTAL",
    estimate = total_e + total_c + total_u,
    se = NA_real_,          # no SE for now
    is_total = TRUE
  )
}

# =============== PREP: TOPLINE (per-variable collapse) ============
prep_topline_panel <- function(kob_df, varnames, component = c("Coefficients","Endowments"),
                               pretty_labels = NULL) {
  component <- match.arg(component)
  
  collapsed <- kob_df |>
    dplyr::filter(variable %in% varnames) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      e = sum(e, na.rm = TRUE),
      e_se = sqrt(sum(e_se^2, na.rm = TRUE)),
      c = sum(c, na.rm = TRUE),
      c_se = sqrt(sum(c_se^2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  df <- if (component == "Coefficients") {
    dplyr::transmute(
      collapsed,
      label = variable,
      estimate = c, se = c_se, is_total = FALSE
    )
  } else {
    dplyr::transmute(
      collapsed,
      label = variable,
      estimate = e, se = e_se, is_total = FALSE
    )
  }
  
  if (!is.null(pretty_labels))
    df <- df |> dplyr::mutate(label = dplyr::recode(label, !!!pretty_labels))
  
  total_row <- df |>
    dplyr::summarise(
      label = if (component == "Coefficients") "Total Coefficients" else "Total Endowments",
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  dplyr::bind_rows(total_row, df)
}

# =============== PREP: SUBGROUP (e.g., race levels, age bands) =====
# part = "coeff" (c/c_se) or "endow" (e/e_se)
prep_subgroup_panel <- function(kob_df, variable, part = c("coeff","endow"),
                                level_label_fun = NULL, level_order = NULL,
                                include_total = FALSE) {
  part <- match.arg(part)
  raw <- kob_df |> dplyr::filter(variable == !!variable)
  
  # derive display label for each level
  label <- if (!is.null(level_label_fun)) {
    level_label_fun(raw)
  } else if ("term" %in% names(raw)) {
    # default for bucket vars like RACE_ETH_bucketX
    sub("^.*?_", "", raw$term)
  } else raw$value
  
  df <- if (part == "coeff") {
    dplyr::transmute(raw, label = label, estimate = c, se = c_se, is_total = FALSE)
  } else {
    dplyr::transmute(raw, label = label, estimate = e, se = e_se, is_total = FALSE)
  }
  
  if (!is.null(level_order))
    df$label <- factor(df$label, levels = level_order)
  
  if (include_total) {
    total_row <- df |>
      dplyr::summarise(
        label = if (part == "coeff") "Total Coefficients" else "Total Endowments",
        estimate = sum(estimate, na.rm = TRUE),
        se = sqrt(sum(se^2, na.rm = TRUE)),
        is_total = TRUE,
        .groups = "drop"
      )
    df <- dplyr::bind_rows(total_row, df)
  }
  
  df
}
