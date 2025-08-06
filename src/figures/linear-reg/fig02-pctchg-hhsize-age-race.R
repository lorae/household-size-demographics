##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv# Figure 2
##############################################
# Define race order (ensuring correct factor levels for consistent ordering)
race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- age_bucket_summary %>%
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# Split data into two halves
left_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[1:4])  # First 4 races
right_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[5:8])  # Last 4 races

# Left plot (Facet titles on the LEFT)
left_plot <- ggplot(left_data, aes(x = subgroup, y = hhsize_pctchg_2000_2019, 
                                   fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +  # Define colors
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 10),  # Labels on the left
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Hide left-side axis labels
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    
    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),  # Right-align text
    axis.ticks.y.right = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    labels = function(x) paste0(x, "%"),  # Add percentage symbol
    sec.axis = dup_axis(name = NULL, labels = function(x) paste0(x, "%"))  # Adds tick marks with percentage on the right
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

left_plot

# Right plot (Facet titles on the RIGHT)
right_plot <- ggplot(right_data, aes(x = subgroup, 
                                     y = pmin(hhsize_pctchg_2000_2019, 14))) +  # Cap at 14% (leave room for asterisk at 14.5%)
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  
  # Add an asterisk above the capped bar at a safe y-position (14.5 instead of 16)
  geom_text(data = right_data %>% filter(RACE_ETH_bucket == "Other", subgroup == "80-84"),
            aes(x = subgroup, y = 14.5, label = "*"),
            size = 5, color = "black") +
  
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, color = "black", size = 10),  # Labels on the right
    strip.placement = "outside",  # Fixing the warning by removing strip.position
    panel.spacing = unit(0.8, "lines"),  # Slightly reduce panel spacing
    panel.spacing.y = unit(0.1, "lines"),  # Reduce vertical spacing
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 6),
    
    # Add subtle tick marks on the LEFT side without labels
    axis.text.y.left = element_blank(),  # No numeric labels
    axis.ticks.y.left = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Hide right-side axis labels
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Adjust plot margins to reduce left-side empty space
    plot.margin = margin(5, 5, 5, -10)  # Reduce left margin
  ) +
  scale_y_continuous(
    breaks = seq(-10, 10, by = 5),  # Tick marks every 5%
    limits = c(-15, 15),
    sec.axis = dup_axis(name = NULL)  # Keep tick marks without labels
  ) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

right_plot

# Combine both plots into one, side-by-side
combined_plot <- left_plot + right_plot + 
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(
    caption = "*The 80-84 age group's increase in the 'Other' category exceeds the y-axis limits of ±15%."
  )

# Save the combined plot
ggsave("results/fig02.png", plot = combined_plot, width = 6.5, height = 6.5, dpi = 500)

# Display the combined plot
print(combined_plot)
##############################################
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
##############################################

##############################################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv# Modified Figure 2 (Figure 2a, 2b)
##############################################

race_order <- c("All", "AAPI", "AIAN", "Black", "Hispanic", "White", "Multiracial", "Other")
age_bucket_summary <- age_bucket_summary |>
  mutate(RACE_ETH_bucket = factor(RACE_ETH_bucket, levels = race_order))

# # Split data into two halves
# left_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[1:4])  # First 4 races
# right_data <- age_bucket_summary %>% filter(RACE_ETH_bucket %in% race_order[5:8])  # Last 4 races

# Left plot (Facet titles on the LEFT)
fig02a <- ggplot(age_bucket_summary |> filter(RACE_ETH_bucket == "All"), aes(x = subgroup, y = hhsize_pctchg_2000_2019, 
                                                                             fill = RACE_ETH_bucket == "All")) +  # Conditional fill
  geom_bar(stat = "identity") +
  # facet_grid(rows = vars(RACE_ETH_bucket), scales = "fixed", switch = "y") +
  geom_vline(aes(xintercept = as.numeric(subgroup)), color = "grey80", linetype = "dashed", size = 0.3) +
  geom_hline(yintercept = 0, color = "black", size = 0.6) +
  scale_fill_manual(values = c("TRUE" = "grey60", "FALSE" = "steelblue"), guide = "none") +  # Define colors
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, color = "black", size = 8),  # Labels on the left
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    panel.spacing.y = unit(0.2, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 8),
    
    # Add subtle tick marks on the right side without grid lines
    axis.text.y.right = element_text(size = 8, color = "black", hjust = 1),  # Right-align text
    axis.ticks.y.right = element_line(color = "black", size = 0.3),  # Subtle tick marks
    
    # Remove major y-grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(y = NULL, x = NULL)  # Remove y-axis label

fig02a
ggsave("results/fig02a.png", plot = fig02a, width = 6.5, height = 3, dpi = 500)
