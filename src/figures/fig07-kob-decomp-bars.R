# #src/figures/fig07-kob-decomp-bars.R
# The purpose of this script is to define functions that produce bar charts showing
# the kob decomposition.
# Input: various global vars for kob decomposition, such as kob_ppbr, defined in 
#        kob/scripts/kob-control-script.R
# Output: Functions defined below, called in kob/scripts/kob-control-script.R
#
# TODO: write a unit test or example script that does this. Also, have kob-control-script
# save the kob results before producing graphs, rather than relying on global vars.

prepare_kob_plot_data <- function(kob_output, varnames, pretty_labels = NULL) {
  # Collapse into one row per variable
  collapsed <- kob_output |>
    filter(variable %in% varnames) |> 
    group_by(variable) |>
    summarise(
      across(starts_with("coef_"), ~ NA_real_),
      across(c("u", "e", "c"), ~ sum(.x, na.rm = TRUE)),
      across(c("prop_2000_se", "prop_2019_se", "u_se", "e_se", "c_se"), ~ sqrt(sum(.x^2, na.rm = TRUE))),
      .groups = "drop"
    ) |>
    select(variable, e, e_se, c, c_se) |>
    pivot_longer(cols = c(e, c), names_to = "component", values_to = "estimate") |>
    mutate(
      se = if_else(component == "e", e_se, c_se),
      component = recode(component, e = "Endowments", c = "Coefficients"),
      component = factor(component, levels = c("Coefficients", "Endowments"))
    ) |>
    select(-e_se, -c_se)
  
  # Add intercept if present
  if ("(Intercept)" %in% kob_output$term) {
    intercept_row <- kob_output |>
      filter(term == "(Intercept)") |>
      summarise(
        variable = "Intercept",
        component = "Intercept",
        estimate = sum(u, na.rm = TRUE),
        se = sqrt(sum(u_se^2, na.rm = TRUE)),
        .groups = "drop"
      )
    collapsed <- bind_rows(collapsed, intercept_row)
  }
  
  # Add total row unconditionally
  total_row <- collapsed |>
    summarise(
      variable = "Total",
      component = "Total",
      estimate = sum(estimate, na.rm = TRUE),
      se = NA_real_,
      .groups = "drop"
    )
  collapsed <- bind_rows(collapsed, total_row)
  
  # Rename variables if a mapping is provided
  if (!is.null(pretty_labels)) {
    collapsed <- collapsed |>
      mutate(variable = recode(variable, !!!pretty_labels))
  }
  
  return(collapsed)
}


plot_kob_decomposition <- function(
    plot_data,
    title = "KOB Decomposition",
    show_total = TRUE,
    hide_facet_labels = TRUE
) {
  if (!show_total) {
    plot_data <- plot_data |> filter(component != "Total")
  }
  
  p <- ggplot(plot_data, aes(x = estimate, y = variable, fill = component)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_errorbarh(
      aes(xmin = estimate - se,
          xmax = estimate + se),
      height = 0.25, color = "black", na.rm = TRUE
    ) +
    facet_grid(
      rows = vars(component),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(values = c(
      "Endowments" = "#56B4E9",
      "Coefficients" = "#E69F00",
      "Intercept" = "gray50",
      "Total" = "black"
    )) +
    theme_minimal(base_size = 13) +
    labs(
      title = title,
      x = "Contribution to Outcome Gap",
      y = NULL
    ) +
    theme(
      panel.spacing.y = unit(1, "lines"),
      legend.position = "none"
    )
  
  # Conditionally hide facet strip labels
  if (hide_facet_labels) {
    p <- p + theme(
      strip.text.x = element_blank(),
      strip.text.y = element_blank(),
      strip.text.y.left = element_blank(),
      strip.placement = NULL
    )
  }
  
  return(p)
}
