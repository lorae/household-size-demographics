# #src/figures/fig0xx-kob-decomp-subgroup.R
# Produce bar charts showing the kob decomposition.
#
# Input: throughput/kob_output.rds
# Output: todo
#

# ----- Step 0: Load packages ----- #
library("ggplot2")
library("dplyr")
library("readr")
library("forcats") 

# ----- Step 1: Load decomposition output ----- #
kob_output <- readRDS("throughput/kob_output.rds")

# ===============================
# ===== RACE / ETHNICITY ========
# ===============================

# ----- Step 2A: Extract Race/Ethnicity Coefficient Effects from Number of Persons ----- #
race_coef_df <- kob_output$p |>
  filter(variable == "RACE_ETH_bucket") |>
  mutate(
    group = gsub("^RACE_ETH_bucket", "", term),
    group = ifelse(group == "", "(Omitted)", group),
    group = factor(group),
    estimate = c,
    se = c_se
  )

# ----- Step 3A: Plot Race/Ethnicity Coefficient Contributions ----- #
fig_race_coef <- ggplot(race_coef_df, aes(x = estimate, y = group)) +
  geom_col(fill = "#E69F00", width = 0.6) +
  geom_errorbarh(
    aes(xmin = estimate - se, xmax = estimate + se),
    height = 0.25,
    color = "black"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Race / Ethnicity Contribution to Number of Persons (Coefficients Only)",
    x = "Contribution to Outcome Gap",
    y = NULL
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 12)
  )
fig_race_coef

# ----- Step 4A: Save Race Plot ----- #
ggsave(
  "output/figures/figXX-kob-race-coef-persons.png",
  plot = fig_race_coef,
  width = 2400, height = 1600, units = "px", dpi = 200
)

# ========================
# ===== NATIVITY =========
# ========================

# ----- Step 2B: Extract U.S.-Born vs Foreign-Born Coefficients ----- #
nativity_coef_df <- kob_output$p |>
  filter(variable == "us_born") |>
  mutate(
    group = value,
    estimate = c,
    se = c_se
  )

# ----- Step 3B: Plot Nativity Coefficient Contributions ----- #
fig_nativity_coef <- ggplot(nativity_coef_df, aes(x = estimate, y = group)) +
  geom_col(fill = "#E69F00", width = 0.6) +
  geom_errorbarh(
    aes(xmin = estimate - se, xmax = estimate + se),
    height = 0.25,
    color = "black"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Nativity Contribution to Number of Persons (Coefficients Only)",
    x = "Contribution to Outcome Gap",
    y = NULL
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 12)
  )
fig_nativity_coef

# ----- Step 4B: Save Nativity Plot ----- #
ggsave(
  "output/figures/figXX-kob-nativity-coef-persons.png",
  plot = fig_nativity_coef,
  width = 2400, height = 1600, units = "px", dpi = 200
)

# ========================
# ===== AGE (Endowments) =
# ========================

# ----- Step 2C: Extract Age Endowment Effects from Number of Persons ----- #
age_endow_df <- kob_output$p |>
  filter(variable == "AGE_bucket") |>
  mutate(
    group = value,
    group = ifelse(group == "85plus", "85+", group),
    group = factor(group, levels = c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
      "75-79", "80-84", "85+"
    )),
    group = fct_rev(group), 
    estimate = e,
    se = e_se
  )

# ----- Step 3C: Plot Age Endowment Contributions ----- #
fig_age_endow <- ggplot(age_endow_df, aes(x = estimate, y = group)) +
  geom_col(fill = "#56B4E9", width = 0.6) +
  geom_errorbarh(
    aes(xmin = estimate - se, xmax = estimate + se),
    height = 0.25,
    color = "black"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Age Contribution to Number of Persons (Endowments Only)",
    x = "Contribution to Outcome Gap",
    y = NULL
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 12)
  )

fig_age_endow

# ----- Step 4C: Save Age Endowment Plot ----- #
ggsave(
  "output/figures/figXX-kob-age-endow-persons.png",
  plot = fig_age_endow,
  width = 2400, height = 1600, units = "px", dpi = 200
)
