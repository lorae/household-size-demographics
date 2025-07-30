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
prep_dotplot_data <- function(data, state_order) {
  
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

get_arrow_data <- function(data, state_order) {
  arrow_data <- data |>
    mutate(State = factor(State, levels = state_order)) |>
    select(State, observed_2000, observed_2019) |>
    mutate(
      x_start = observed_2000,
      x_end = observed_2019,
      direction = case_when(
        observed_2019 > observed_2000 ~ "increase",
        observed_2019 < observed_2000 ~ "decrease",
        TRUE ~ "no_change"
      )
    )
  return(arrow_data)
}

make_dotplot <- function(
    dotplot_data, 
    x_title, 
    limits = c(2.5, 4.5),
    show_legend = TRUE,
    show_y_labels = TRUE
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
    scale_x_continuous(name = x_title, limits = limits, expand = c(0, 0)) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.title = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = if (show_y_labels) element_text(size = 9) else element_blank(),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(10, 20, 10, 10)
    )
}

make_dotplot_new <- function(
    dotplot_data, 
    x_title, 
    x_as_percent = FALSE,
    limits = c(2.5, 4.5),
    show_legend = TRUE,
    show_y_labels = TRUE,
    arrow_data = NULL
){
  
  ggplot() +
    # Grey background bands
    geom_rect(data = dotplot_data$band_data,
              aes(ymin = ymin, ymax = ymax),
              xmin = -Inf, xmax = Inf, fill = "grey95") +
    
    # Arrow layer (optional)
    {
      if (!is.null(arrow_data)) {
        geom_segment(
          data = arrow_data,
          aes(x = x_start, xend = x_end, y = State, yend = State, color = direction),
          arrow = arrow(length = unit(0.12, "cm")),
          linewidth = 0.5
        )
      } else {
        NULL
      }
    } +
    
    # Dot at starting point (2000 only)
    geom_point(
      data = dotplot_data$data_long |> 
        filter(year == "2000") |>
        left_join(arrow_data |> select(State, direction), by = "State"),
      aes(x = observed, y = State, color = direction),
      size = 1.2
    ) +
    
    scale_color_manual(
      values = c("increase" = "darkblue", "decrease" = "darkred", "no_change" = "gray"),
    ) +
    guides(
      color = guide_legend(
        override.aes = list(
          x = c(0.5, 0),         
          xend = c(0, 0.5),      
          y = c(0.5, 0.5), 
          yend = c(0.5, 0.5),
          arrow = list(arrow(length = unit(0.12, "cm"))),
          linewidth = 0.5
        ),
        title = NULL
      )
    ) +
    
    scale_x_continuous(name = x_title, limits = limits, expand = c(0, 0)) +
    scale_x_continuous(
      name = x_title, 
      limits = limits, 
      expand = c(0, 0),
      labels = if (x_as_percent) scales::label_percent(accuracy = 1) else waiver()
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.title = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = if (show_y_labels) element_text(size = 9) else element_blank(),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# ----- Step 2: Make plots -----

# ---- FIG02 OLD
# Come up with order of states in chart. We use the order of the hhsizes
# and we'll make the headship dotplot match this order
state_order <- headship_state |>
  arrange(observed_2000) |>
  pull(State) 


p <- make_dotplot(
  dotplot_data = prep_dotplot_data(hhsize_state, state_order = state_order), 
  x_title = "Average Household Size", 
  limits = c(2.5, 4.5),
  show_legend = FALSE
)

h <- make_dotplot(
  dotplot_data = prep_dotplot_data(headship_state, state_order = state_order), 
  x_title = "Average Headship Rate", 
  limits = c(0.3, 0.5),
  show_legend = FALSE,
  show_y_labels = FALSE
)

fig02 <- (p + h) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# --- FIG02 NEW
# Create arrow data
arrow_hhsize <- get_arrow_data(hhsize_state, state_order)
arrow_headship <- get_arrow_data(headship_state, state_order)

# Arrow legend: make it manually

legend_arrow_df <- tibble::tibble(
  direction = c("decrease", "increase"),         # Decrease first
  x_start   = c(1.2, 1.6),
  x_end     = c(1.1, 1.7),                       # Left arrow for decrease, right arrow for increase
  y         = c(1.0, 1.0),
  label     = c("decrease", "increase"),
  color     = c("darkred", "darkblue")
)

# Manually build the arrow legend
arrow_legend_plot <- ggplot(legend_arrow_df) +
  # Arrows
  geom_segment(
    aes(x = x_start, xend = x_end, y = y, yend = y, color = direction),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.6
  ) +
  # Starting dots (representing 2000)
  geom_point(
    aes(x = x_start, y = y, color = direction),
    size = 1.2
  ) +
  # Text labels placed to the right of arrow tips
  geom_text(
    aes(x = x_end + ifelse(direction == "increase", 0.05, 0.35), 
        y = y, 
        label = label),
    hjust = ifelse(legend_arrow_df$direction == "increase", 0, 1),
    size = 3.5
  ) +
  scale_color_manual(values = c("increase" = "darkblue", "decrease" = "darkred")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = c(0.7, 2.4))

p_new <- make_dotplot_new(
  dotplot_data = prep_dotplot_data(hhsize_state, state_order),
  x_title = "Average Household Size",
  limits = c(2.5, 4.5),
  arrow_data = arrow_hhsize,
  show_legend = FALSE
)
h_new <- make_dotplot_new(
  dotplot_data = prep_dotplot_data(headship_state, state_order), 
  x_title = "Average Headship Rate", 
  x_as_percent = TRUE,
  limits = c(0.3, 0.5),
  arrow_data = arrow_headship,
  show_legend = FALSE,
  show_y_labels = FALSE
)

fig02_new <- (p_new + h_new) / arrow_legend_plot  +
  plot_layout(heights = c(1, 0.1))
fig02_new

# ----- Step 3: Save plots ----- #
ggsave(
  "output/figures/fine-grained/fig02-connected-dots.png", 
  plot = fig02, 
  width = 3000, height = 3000, units = "px", dpi = 300
)
