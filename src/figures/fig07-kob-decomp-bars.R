# #src/figures/fig07-kob-decomp-bars.R
# Produce bar charts showing the kob decomposition.
#
# Input: throughput/kob_output.rds
# Output: output/figures/fig07-kob-decomp-bars.png, output/figures/fig07a-kob-decomp-bars.png
#
# TODO: write unit tests for functions
# TODO: optionally turn off x-axis label, make the facet names like Intercept not get cutoff

# ----- Step 0: Define functions -----
# Data preparation function
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
  
  # Optionally hide facet labels
  if (hide_facet_labels) {
    p <- p + theme(
      strip.text.x = element_blank(),
      strip.text.y = element_blank(),
      strip.text.y.left = element_blank(),
      strip.placement = NULL
    )
  }
  
  # Optionally hide variable labels (like "U.S. Born", "Tenure", etc.)
  if (hide_variable_labels) {
    p <- p + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  return(p)
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
# ----- Step 1: Read in data, define labels ----- #
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

# ----- Step 2: Make plots ----- #
p    <- make_kob_plot(kob_numprec, "Number of Persons", hide_variable_labels = FALSE)
r    <- make_kob_plot(kob_room, "Number of Rooms")
b    <- make_kob_plot(kob_bedroom, "Number of Bedrooms")
ppr  <- make_kob_plot(kob_ppr, "Persons per Room")
ppbr <- make_kob_plot(kob_ppbr, "Persons per Bedroom", hide_variable_labels = TRUE)

# Figure 7 shows # Persons, # Bedooms, Persons per Bedoom
fig07 <- (p + b + ppbr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# Figure 7A (Appendix version) shows # Persons, # Rooms, Persons per Room
fig07a <- (p + r + ppr) +
  plot_annotation() &
  theme(plot.margin = margin(10, 10, 20, 10))  # top, right, bottom, left

# ----- Step 3: Save plots ----- #
ggsave(
  "output/figures/fig07-kob-decomp-bars.png", 
  plot = fig07, 
  width = 3000, height = 2000, units = "px", dpi = 200
)
ggsave(
  "output/figures/fig07a-kob-decomp-bars.png", 
  plot = fig07a, 
  width = 3000, height = 2000, units = "px", dpi = 200
)
