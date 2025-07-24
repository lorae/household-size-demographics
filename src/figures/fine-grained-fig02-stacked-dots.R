# src/figures/fine-grained-fig02-stacked-dots.R
# Produce dot charts where each row is a state and dots connect hhsize or headship rates
# in 2000 with those in 2019.
#
# Input: TBD
# Output: TBD

# ----- Step 0: Load required packages ----- #
library("dplyr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("forcats")
options(scipen = 999)

# ----- Step 1: Import data ----- #
headship_state <- readRDS("throughput/fine-grained-headship-diff-state.rds")
hhsize_state <- readRDS("throughput/fine-grained-hhsize-diff-state.rds")

# Prep long-format data
hhsize_state_long <- hhsize_state %>%
  select(State, observed_2000, observed_2019) %>%
  pivot_longer(cols = starts_with("observed_"),
               names_to = "Year", values_to = "Household_Size") %>%
  mutate(
    Year = recode(Year, "observed_2000" = "2000", "observed_2019" = "2019")
  )

# Sort states by observed_2000 DESCENDING
state_order <- hhsize_state %>%
  arrange(desc(observed_2000)) %>%
  pull(State)

hhsize_state_long <- hhsize_state_long %>%
  mutate(State = factor(State, levels = rev(state_order)))  # rev() puts highest at top

# Add row index for band shading
band_data <- hhsize_state_long %>%
  distinct(State) %>%
  mutate(row_id = row_number()) %>%
  filter(row_id %% 2 == 0) %>%
  mutate(ymin = row_id - 0.5, ymax = row_id + 0.5)

# Plot
ggplot() +
  # Grey background bands
  geom_rect(data = band_data,
            aes(ymin = ymin, ymax = ymax),
            xmin = -Inf, xmax = Inf, fill = "grey95") +
  
  # Line segments connecting dots
  geom_line(data = hhsize_state_long, aes(x = Household_Size, y = State, group = State), color = "gray60") +
  
  # Red & blue dots
  geom_point(data = hhsize_state_long, aes(x = Household_Size, y = State, color = Year), size = 2.5) +
  
  scale_color_manual(values = c("2000" = "red", "2019" = "blue")) +
  scale_x_continuous(name = "Average Household Size", limits = c(2.5, 4.5), expand = c(0, 0)) +
  
  labs(title = "Average Household Size by State in 2000 and 2019") +
  theme_minimal(base_size = 12) +
  theme(
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10, 20, 10, 10)
  )

# # ----- Example usage -----
# hhsize_prepped <- prepare_dotplot_data(hhsize_state)
# plot_state_dot_chart(
#   plot_data = hhsize_prepped$plot_data,
#   band_data = hhsize_prepped$bands,
#   x_label = "Average Household Size",
#   title = "Average Household Size by State in 2000 and 2019"
# )
