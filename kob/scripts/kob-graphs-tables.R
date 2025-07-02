# kob/scripts/kob-graphs-tables
# The purpose of this script is to be run by kob/scripts/kob-control-script.R
# see that script's header for more details.
# TODO: functionalize this code so it's not so brittle and dependent on environmental 
# variables, like, for example, "coef"
# I'm not even sure this entire script runs - hasn't been tested yet.

write.csv(coef, "results/coef2.csv")

# Filter out rows with NA c_component and create label
coef_clean <- coef |>
  filter(!is.na(c_component)) |>
  mutate(label = name)

# Plot
ggplot(coef_clean, aes(x = reorder(label, c_component), y = c_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Coefficients (c_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()

ggplot(coef_clean, aes(x = reorder(label, e_component), y = e_component)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution to Endowments (e_component)",
    x = NULL,
    y = "c_component"
  ) +
  theme_minimal()