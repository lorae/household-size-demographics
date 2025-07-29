# #src/figures/fig02-hhsize-race-hist.R
#
# Produce bar charts showing hhsize by race as a histogram
#
# Input: data/db/ipums.duckdb
# Output: output/figures/fig02-hhsize-race-hist.png
#         output/figures/fig02-data.rds 
#
# ----- Step 0: Config ----- #
library("ggplot2")
library("duckdb")
library("dplyr")
library("tidyr")
library("glue")
library("patchwork")
library("grid")

devtools::load_all("../dataduck") # The crosstab_count() function used here is defined in dataduck

# ----- Step 1: Define functions ----- #
# Plotting function for one histogram
plot_hhsize_histogram <- function(data = fig02_data,
                                  main_color = "steelblue",
                                  title = NULL,
                                  xtitle = TRUE,
                                  ytitle = TRUE,
                                  ymax = NULL) {
  # Input validation: Only one race group in data
  race_group <- unique(data$RACE_ETH_bucket)
  if (length(race_group) != 1) {
    stop("Data must be filtered to a single RACE_ETH_bucket.")
  }
  
  # Input validation: Frequencies sum to one
  freq_sum <- sum(data$freq)
  if (abs(freq_sum - 1) > 1e-6) {
    stop(glue("Frequencies must sum to one within 1e-6 tolerance. Frequencies sum to {freq_sum}"))
  }
  
  # Input validation: ymax >= largest freq value
  largest_freq <- max(data$freq)
  if (!is.null(ymax) && ymax < largest_freq) {
    stop(glue("Largest frequency provided of {largest_freq} exceeds `ymax` specified of {ymax}. 
              Please increase `ymax` setting."))
  } 
  
  # Build base plot
  p <- ggplot(data, aes(x = factor(NUMPREC), y = freq)) +
    geom_bar(
      stat = "identity",
      width = 1,
      fill = scales::alpha(main_color, 0.5),
      color = main_color,
      size = 0.3
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  # Add y-axis limit if specified
  if (!is.null(ymax)) {
    p <- p + ylim(0, ymax)
  }
  
  # Title
  if (!is.null(title)) {
    p <- p + labs(title = title)
  } else {
    p <- p
  }
  
  # Axis labels
  p <- p + labs(
    x = if (xtitle) "Number of Persons in Household" else NULL,
    y = if (ytitle) "Frequency" else NULL
  )
  
  return(p)
}

# Plotting function for two overlaid histograms
plot_hhsize_histogram_double <- function(
    data,
    per1 = 2000,
    per2 = 2019,
    bar_fills = list(
      per1 = list(color = "skyblue", alpha = 0.4, line_type = "dashed"), # 2000
      per2 = list(color = "forestgreen", alpha = 0.8, line_type = "solid") # 2019
    ),
    title = NULL,
    xtitle = TRUE,
    ytitle = TRUE,
    ymax = NULL,
    add_legend = FALSE
) {
  # Input validation
  race_group <- unique(data$RACE_ETH_bucket)
  if (length(race_group) != 1) {
    stop("Data must be filtered to a single RACE_ETH_bucket.")
  }
  
  years_present <- sort(unique(data$year))
  if (!all(c(per1, per2) %in% years_present)) {
    stop(glue::glue("Data must include both per1 ({per1}) and per2 ({per2}) years. Found: {paste(years_present, collapse=', ')}"))
  }
  
  # Explicit factor levels for year labels
  d1 <- data |> filter(year == per1) |> mutate(year_label = factor("2000", levels = c("2000", "2019")))
  d2 <- data |> filter(year == per2) |> mutate(year_label = factor("2019", levels = c("2000", "2019")))
  all_data <- bind_rows(d1, d2)
  
  # y-axis limit
  if (is.null(ymax)) {
    ymax <- max(all_data$freq) * 1.05
  }
  
  ggplot(mapping = aes(x = factor(NUMPREC), y = freq)) +
    geom_bar(
      data = d1,
      aes(fill = year_label, color = year_label, linetype = year_label),
      stat = "identity",
      alpha = bar_fills$per1$alpha,
      width = 0.9,
      size = 0.3,
      show.legend = add_legend
    ) +
    geom_bar(
      data = d2,
      aes(fill = year_label, color = year_label, linetype = year_label),
      stat = "identity",
      alpha = bar_fills$per2$alpha,
      width = 0.6,
      size = 0.3,
      show.legend = add_legend
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_fill_manual(
      values = c("2000" = bar_fills$per1$color, "2019" = bar_fills$per2$color),
      guide = if (add_legend) "legend" else "none"
    ) +
    scale_color_manual(
      values = c("2000" = bar_fills$per1$color, "2019" = bar_fills$per2$color),
      guide = if (add_legend) "legend" else "none"
    ) +
    scale_linetype_manual(
      values = c("2000" = bar_fills$per1$line_type, "2019" = bar_fills$per2$line_type),
      guide = if (add_legend) "legend" else "none"
    ) +
    labs(
      title = title,
      x = if (xtitle) "Number of Persons in Household" else NULL,
      y = if (ytitle) "Frequency" else NULL,
      fill = NULL,
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = if (add_legend) "bottom" else "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# ----- Step 2: Import and wrangle data ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# Estimate of non-group-quarters population in household of X size and Y race/ethnicity
hhsize_race_2019 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2019)

hhsize_race_2000 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "RACE_ETH_bucket"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2000)

hhsize_allraces_allyears <- crosstab_count(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "YEAR"),
  every_combo = TRUE
) |> collect() |> rename(year = YEAR) |> mutate(RACE_ETH_bucket = "All")

hhsize_race_year <- bind_rows(hhsize_race_2000, hhsize_race_2019, hhsize_allraces_allyears)

# ----- Step 3: Make plots ----- #
# Choose a max NUMPREC (household size) to display in histogram
topcode_hhsize <- 8

# Topcode the table
fig02_data <- hhsize_race_year |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, topcode_hhsize, NUMPREC)) |>
  group_by(RACE_ETH_bucket, NUMPREC, year) |>
  summarize(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  # Add frequencies for each NUMPREC value within RACE_ETH_bucket subpopulations
  group_by(RACE_ETH_bucket, year) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup() |> 
  arrange(year, RACE_ETH_bucket, NUMPREC)

# Configure plot settings
theme_2019_all = list(
  color = "darkgrey",
  alpha = 0.4,
  line_type = "solid"
)
theme_2000_all <- list(
  color = "lightgrey",
  alpha = 0.4,
  line_type = "dashed"
)
theme_2019 = list(
  color = "forestgreen",
  alpha = 0.4,
  line_type = "solid"
)
theme_2000 <- list(
  color = "skyblue",
  alpha = 0.4,
  line_type = "dashed"
)
ymax <- 0.35

# ... Figure 2 ... (default format)
all <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "All"),
  title = "All",
  ymax = ymax,
  ytitle = TRUE,
  xtitle = FALSE,
  bar_fills = list(
    per1 = list(color = theme_2000_all$color, alpha = theme_2000_all$alpha, line_color = theme_2000_all$color, line_type = theme_2000_all$line_type),
    per2 = list(color = theme_2019_all$color, alpha = theme_2019_all$alpha, line_color = theme_2019_all$color, line_type = theme_2019_all$line_type)
  ),
)

black <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black"),
  title = "Black",
  ymax = ymax,
  ytitle = FALSE,
  xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
  add_legend = TRUE
)

hispanic <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic"),
  title = "Hispanic",
  ymax = ymax,
  ytitle = TRUE,
  xtitle = FALSE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
  add_legend = FALSE
)

white <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "White"),
  title = "White",
  ymax = ymax,
  ytitle = FALSE,
  xtitle = FALSE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
)

aapi <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "AAPI"),
  title = "AAPI",
  ymax = ymax,
  ytitle = TRUE,
  xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
)

aian <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "AIAN"),
  title = "AIAN",
  ymax = ymax,
  ytitle = FALSE,
  xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
)

multiracial <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Multiracial"),
  title = "Multiracial",
  ymax = ymax,
  ytitle = TRUE,
  xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
)

other <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(RACE_ETH_bucket == "Other"),
  title = "Other",
  ymax = ymax,
  ytitle = FALSE,
  xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_color = theme_2000$color, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_color = theme_2019$color, line_type = theme_2019$line_type)
  ),
)

# Combine
fig02 <-  white / hispanic / black
fig02c <- (all + black) / (hispanic + white) / (aapi + aian) + (multiracial + other)
fig02d <- (all + black) / (hispanic + white) 


# ... Figure 2B ... (alternate format)
# 2019
black_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black" & year == 2019),
  main_color = theme_2019$color,
  title = "Black",
  xtitle = FALSE,
  ytitle = TRUE,
  ymax = ymax
)

hispanic_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic" & year == 2019),
  main_color = theme_2019$color,
  title = "Hispanic",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

white_2019 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "White" & year == 2019),
  main_color = theme_2019$color,
  title = "White",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

# 2000
black_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Black" & year == 2000),
  main_color = theme_2000$color,
  title = "Black",
  xtitle = FALSE,
  ytitle = TRUE,
  ymax = ymax
)

hispanic_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "Hispanic" & year == 2000),
  main_color = theme_2000$color,
  title = "Hispanic",
  xtitle = TRUE,
  ytitle = FALSE,
  ymax = ymax
)

white_2000 <- plot_hhsize_histogram(
  data = fig02_data |> filter(RACE_ETH_bucket == "White" & year == 2000),
  main_color = theme_2000$color,
  title = "White",
  xtitle = FALSE,
  ytitle = FALSE,
  ymax = ymax
)

# Combine
# Function to create a styled year label
make_year_label <- function(text) {
  ggplot() +
    annotate("text", x = 1, y = 1, label = text, hjust = 1, size = 4.5, fontface = "bold") +
    theme_void() +
    theme(
      plot.margin = margin(5, 10, 5, 10),  # spacing around text
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3)
    ) +
    coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
}

# Create year labels
title_2019 <- make_year_label("2019")
title_2000 <- make_year_label("2000")

# Combine all plots
fig02b <- (
  title_2019 / (black_2019 + hispanic_2019 + white_2019) /
    title_2000 / (black_2000 + hispanic_2000 + white_2000)
) + plot_layout(heights = c(0.11, 1, 0.11, 1))


# ----- Step 4: Save plots & data ----- #
ggsave(
  "output/figures/fig02-hhsize-race-hist.png",
  plot = fig02,
  width = 2000, height = 5000, units = "px", dpi = 400
)

ggsave(
  "output/figures/fig02b-hhsize-race-hist.png",
  plot = fig02b,
  width = 3000, height = 2400, units = "px", dpi = 300
)

ggsave(
  "output/figures/fig02c-hhsize-race-hist.png",
  plot = fig02c,
  width = 3000, height = 6000, units = "px", dpi = 300
)

ggsave(
  "output/figures/fig02d-hhsize-race-hist.png",
  plot = fig02d,
  width = 3000, height = 3000, units = "px", dpi = 300
)

saveRDS(fig02_data, "output/figures/fig02-data.rds")


