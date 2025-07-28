# src/figures/fine-grained-fig02-connected-dots.R
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
prep_dotplot_data <- function(data) {
  
  # Come up with order of states in chart
  state_order <- data |>
    arrange(observed_2000) |>
    pull(State) 
  
  # Make the data long
  data_long <- data |>
    select(
      State, 
      observed_2000, 
      observed_2019
    ) |>
    pivot_longer(cols = starts_with("observed_"), names_to = "year", values_to = "observed") |>
    mutate(
      year = recode(year, "observed_2000" = "2000", "observed_2019" = "2019")
    ) |>
    mutate(State = factor(State, levels = state_order))
  
  band_data <- data_long |>
    distinct(State) |>
    mutate(row_id = row_number()) |>
    filter(row_id %% 2 == 0) |>
    mutate(ymin = row_id - 0.5, ymax = row_id + 0.5)
    
  return(list(data_long = data_long, band_data = band_data))
}

make_dotplot <- function(
    dotplot_data, 
    title, 
    limits = c(2.5, 4.5),
    show_legend = TRUE
    ){
  ggplot() +
    # Grey background bands
    geom_rect(data = dotplot_data$band_data,
              aes(ymin = ymin, ymax = ymax),
              xmin = -Inf, xmax = Inf, fill = "grey95") +
    
    # Line segments connecting dots
    geom_line(data = dotplot_data$data_long, aes(x = observed, y = State, group = State), color = "gray60") +
    
    # Red & blue dots
    geom_point(
      data = dotplot_data$data_long, 
      aes(x = observed, y = State, color = year), 
      size = 2.0
    ) +
    
    scale_color_manual(values = c("2000" = "skyblue", "2019" = "forestgreen")) +
    scale_x_continuous(name = title, limits = limits, expand = c(0, 0)) +
    
    labs(title = title) +
    theme_minimal(base_size = 12) +
    theme(
      legend.title = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# ----- Step 2: Make plots -----
p <- make_dotplot(
  dotplot_data = prep_dotplot_data(hhsize_state), 
  title = "Average Household Size", 
  limits = c(2.5, 4.5),
  show_legend = FALSE
  )
p

h <- make_dotplot(
  dotplot_data = prep_dotplot_data(headship_state), 
  title = "Average Headship Rate", 
  limits = c(0.3, 0.5),
  show_legend = FALSE
)
h

fig02 <- (p + h) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# ----- Step 3: Save plots ----- #
ggsave(
  "output/figures/fine-grained/fig02-connected-dots.png", 
  plot = fig02, 
  width = 3000, height = 3000, units = "px", dpi = 300
)
