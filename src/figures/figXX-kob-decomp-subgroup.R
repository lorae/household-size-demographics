# #src/figures/fig07-kob-decomp-bars.R
# Produce bar charts showing the kob decomposition.
#
# Input: throughput/kob_output.rds
# Output: output/figures/fig07-kob-decomp-bars.png, output/figures/fig07a-kob-decomp-bars.png
#
# TODO: where is the white subgroup? track down the data

# ----- Step 0: Load packages ----- #
library("ggplot2")
library("dplyr")
library("readr")

# ----- Step 1: Load decomposition output ----- #
kob_output <- readRDS("throughput/kob_output.rds")

# ----- Step 2: Extract Race/Ethnicity Coefficient Effects from Number of Persons ----- #
race_coef_df <- kob_output$p |>
  filter(variable == "RACE_ETH_bucket") |>
  mutate(
    group = gsub("^RACE_ETH_bucket", "", term),
    group = ifelse(group == "", "(Omitted)", group),
    group = factor(group),
    estimate = c,
    se = c_se
  )

# ----- Step 3: Plot Race/Ethnicity Coefficient Contributions ----- #
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



# ----- Step 4: Save Plot ----- #
ggsave(
  "output/figures/figXX-kob-race-coef-persons.png",
  plot = fig_race_coef,
  width = 2400, height = 1600, units = "px", dpi = 200
)

