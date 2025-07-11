# #src/figures/fig06-observed-counterfactual-bars.R
# The purpose of this script is to define functions that produce bar charts showing
# 2000 observed, 2019 observed, and 2019 and expected (counterfactual) outcomes 
# of number of people in a household, number bedrooms in a household, and bedroom 
# crowding. Also an appendix version that does the same set of 3 bars by room.
#
# Input: various global vars for kob decomposition, such as kob_ppbr, defined in 
#        kob/scripts/kob-control-script.R
# Output: Functions defined below, called in kob/scripts/kob-control-script.R
#
# TODO: write a unit test or example script that does this. Also, have kob-control-script
# save the kob results before producing graphs, rather than relying on global vars.



# Compute differences for brackets
diff_2000_2019 <- hhsize_2000_observed - hhsize_2019_observed
diff_2019_obs_exp <- hhsize_2019_observed - hhsize_2019_expected

# Adjustable settings
bracket_offset <- 0.005         # Clearance above shorter bar
difference_text_offset <- 0.1   # Horizontal offset for difference text labels

# Create the bar plot
fig03 <- ggplot(fig03_data, aes(x = Category, y = Household_Size, fill = Type)) +
  geom_bar(stat = "identity", aes(linetype = Type), color = "black", linewidth = 0.4, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
  scale_fill_manual(values = c("Observed" = "steelblue", "Counterfactual" = scales::alpha("steelblue", 0.5))) +
  scale_linetype_manual(values = c("Observed" = "solid", "Counterfactual" = "dotted")) +
  labs(y = "Household Size", x = NULL, 
       caption = "Note: Values may not sum exactly due to rounding.") +
  coord_cartesian(ylim = c(3, 3.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9) # Align caption to the left
  ) +
  
  # First I-shaped segment (2000 vs 2019 Observed)
  annotate("segment", x = 2, xend = 2, 
           y = hhsize_2019_observed + bracket_offset, 
           yend = hhsize_2000_observed, color = "black") +  # Vertical part
  annotate("segment", x = 1.9, xend = 2.1, 
           y = hhsize_2000_observed, 
           yend = hhsize_2000_observed, color = "black") +  # Top horizontal tick
  annotate("segment", x = 1.9, xend = 2.1, 
           y = hhsize_2019_observed + bracket_offset, 
           yend = hhsize_2019_observed + bracket_offset, color = "black") +
  annotate("text", x = 2 + difference_text_offset, y = (hhsize_2000_observed + hhsize_2019_observed) / 2, 
           label = sprintf("-%.3f", diff_2000_2019), size = 4, hjust = 0) +
  
  # Second I-shaped segment (2019 Observed vs 2019 Counterfactual)
  annotate("segment", x = 3, xend = 3, 
           y = hhsize_2019_expected + bracket_offset, 
           yend = hhsize_2019_observed, color = "black") +  # Vertical part
  annotate("segment", x = 2.9, xend = 3.1, 
           y = hhsize_2019_observed, 
           yend = hhsize_2019_observed, color = "black") +  # Top horizontal tick
  annotate("segment", x = 2.9, xend = 3.1, 
           y = hhsize_2019_expected + bracket_offset, 
           yend = hhsize_2019_expected + bracket_offset, color = "black") +
  annotate("text", x = 3 + difference_text_offset, y = (hhsize_2019_observed + hhsize_2019_expected) / 2, 
           label = sprintf("-%.3f", diff_2019_obs_exp), size = 4, hjust = 0)

# Display the plot
fig03

# Save the plot
ggsave("results/fig03.png", plot = fig03, width = 6.5, height = 5.5, dpi = 500)


##### Variation Fig 3a without the I-bar differences
# Create the bar plot (without I-segments or differences)
fig3a <- ggplot(fig03_data, aes(x = Category, y = Household_Size, fill = Type)) +
  geom_bar(stat = "identity", aes(linetype = Type), color = "black", linewidth = 0.4, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Household_Size)), vjust = 1.5, color = "white", size = 4) +
  scale_fill_manual(values = c("Observed" = "steelblue", "Counterfactual" = scales::alpha("steelblue", 0.5))) +
  scale_linetype_manual(values = c("Observed" = "solid", "Counterfactual" = "dotted")) +
  labs(y = "Household Size", x = NULL) +
  coord_cartesian(ylim = c(3, 3.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9) # Align caption to the left
  )

# Save the plot
ggsave("results/fig3a.png", plot = fig3a, width = 6.5, height = 5.5, dpi = 500)


#----- Step 4: Save the results to the Shiny app ----- #
# Counterfactuals
save(
  hhsize_cf, 
  bedroom_cf,
  file = "shiny-app/data/counterfactuals.rda"
)