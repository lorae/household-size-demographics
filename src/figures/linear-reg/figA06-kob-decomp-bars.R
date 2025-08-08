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

# ----- Step 1: Define functions -----
# Data preparation function
prepare_kob_plot_data <- function(kob_output, varnames, pretty_labels = NULL) {
  # Collapse into one row per variable within each component
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
      component = factor(component, levels = c("Coefficients", "Endowments")),
      is_total = FALSE
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
        is_total = FALSE,
        .groups = "drop"
      )
    collapsed <- bind_rows(collapsed, intercept_row)
  }
  
  # Add **per-component** totals that sit inside each facet
  totals_by_component <- collapsed |>
    filter(component %in% c("Coefficients", "Endowments")) |>
    group_by(component) |>
    summarise(
      variable = if_else(first(component) == "Coefficients", "Total Coefficients", "Total Endowments"),
      estimate = sum(estimate, na.rm = TRUE),
      se = sqrt(sum(se^2, na.rm = TRUE)),
      is_total = TRUE,
      .groups = "drop"
    )
  
  collapsed <- bind_rows(collapsed, totals_by_component)
  
  # Add overall Total facet row
  total_row <- collapsed |>
    summarise(
      variable = "Total",
      component = "Total",
      estimate = sum(estimate, na.rm = TRUE),
      se = NA_real_,
      is_total = FALSE,
      .groups = "drop"
    )
  collapsed <- bind_rows(collapsed, total_row)
  
  # Pretty labels
  if (!is.null(pretty_labels)) {
    collapsed <- collapsed |>
      mutate(variable = recode(variable, !!!pretty_labels))
  }
  
  # Put the "Total ..." bars at the very top of each facet by making them the
  # last factor levels globally (last level plots at the top on y).
  totals_labels <- c("Total Coefficients", "Total Endowments")
  
  order_levels <- collapsed |>
    mutate(total_flag = variable %in% totals_labels) |>
    # non-totals first, totals last; within each, keep alpha order (or change to taste)
    arrange(component, total_flag, variable) |>
    pull(variable) |>
    unique()
  
  collapsed <- collapsed |>
    mutate(variable = factor(variable, levels = order_levels))
  
  collapsed
}

# Plotting function
plot_kob_decomposition <- function(
    plot_data,
    title = "KOB Decomposition",
    show_total = TRUE,
    hide_facet_labels = FALSE,
    hide_variable_labels = FALSE
) {
  if (!show_total) {
    plot_data <- plot_data |> filter(component != "Total")
  }
  
  # Split out for layering logic
  parts_ce <- plot_data |> filter(component %in% c("Coefficients", "Endowments"), !is_total)
  totals_ce <- plot_data |> filter(component %in% c("Coefficients", "Endowments"),  is_total)
  other_facets <- plot_data |> filter(!(component %in% c("Coefficients", "Endowments")))
  
  p <- ggplot() +
    # --- light contributors (alpha .3) with dotted black outline
    geom_col(
      data = parts_ce,
      aes(x = estimate, y = variable, fill = component),
      width = 0.8, alpha = 0.3, color = "black", linetype = "dotted"
    ) +
    # --- solid totals (alpha 1)
    geom_col(
      data = totals_ce,
      aes(x = estimate, y = variable, fill = component),
      width = 0.8, alpha = 1
    ) +
    # --- other facets rendered normally (Intercept, Total)
    geom_col(
      data = other_facets,
      aes(x = estimate, y = variable, fill = component),
      width = 0.8
    ) +
    # error bars for everything that has SE
    geom_errorbarh(
      data = plot_data,
      aes(y = variable, xmin = estimate - se, xmax = estimate + se),
      height = 0.25, color = "black", na.rm = TRUE
    ) +
    facet_grid(
      rows = vars(component),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(values = c(
      "Endowments"   = "#56B4E9",
      "Coefficients" = "#E69F00",
      "Intercept"    = "gray50",
      "Total"        = "black"
    )) +
    theme_minimal(base_size = 13) +
    labs(
      title = title,
      x = "Contribution to Outcome Gap",
      y = NULL
    ) +
    theme(
      panel.spacing.y = unit(1, "lines"),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  if (hide_facet_labels) {
    p <- p + theme(
      strip.text.x = element_blank(),
      strip.text.y = element_blank(),
      strip.text.y.left = element_blank(),
      strip.placement = NULL
    )
  }
  if (hide_variable_labels) {
    p <- p + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  p
}


# Helper function to prep and plot ---
make_kob_plot <- function(data, title, show_total = TRUE, 
                          hide_facet_labels = TRUE, 
                          hide_variable_labels = TRUE) {
  plot_data <- prepare_kob_plot_data(
    data,
    varnames = varnames_dict,
    pretty_labels = pretty_labels
  )
  plot_kob_decomposition(
    plot_data,
    title = title,
    show_total = show_total,
    hide_facet_labels = hide_facet_labels,
    hide_variable_labels = hide_variable_labels
  )
}
# ----- Step 2: Read in data, define labels ----- #
kob_output <- readRDS("throughput/kob_output.rds")

pretty_labels <- c(
  us_born = "U.S. Born",
  tenure = "Tenure",
  RACE_ETH_bucket = "Race / Ethnicity",
  INCTOT_cpiu_2010_bucket = "Income",
  gender = "Sex",
  EDUC_bucket = "Education",
  cpuma = "CPUMA",
  AGE_bucket = "Age",
  Intercept = "",
  Total = ""
)

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
# ----- Step 3: Make plots ----- #
p    <- make_kob_plot(kob_output$p, "Number of Persons", hide_variable_labels = FALSE, hide_facet_labels = FALSE)
r    <- make_kob_plot(kob_output$r, "Number of Rooms")
b    <- make_kob_plot(kob_output$b, "Number of Bedrooms")
ppr  <- make_kob_plot(kob_output$ppr, "Persons per Room")
ppbr <- make_kob_plot(kob_output$ppbr, "Persons per Bedroom", hide_variable_labels = TRUE)

# Figure 7 shows # Persons, # Bedooms, Persons per Bedoom
figA06 <- (p + b + ppbr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# Figure 7A (Appendix version) shows # Persons, # Rooms, Persons per Room
figA07 <- (p + r + ppr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# ----- Step 4: Save plots ----- #
ggsave(
  "output/figures/linear-reg/figA06-kob-decomp-bars-bedroom.png", 
  plot = figA06, 
  width = 3000, height = 3000, units = "px", dpi = 200
)
ggsave(
  "output/figures/linear-reg/figA07-kob-decomp-bars-room.png", 
  plot = figA07, 
  width = 3000, height = 3000, units = "px", dpi = 200
)
