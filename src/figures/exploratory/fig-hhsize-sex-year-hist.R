# #src/figures/fig02-hhsize-race-hist.R (fig02-only)
# Produce figure 2: overlaid household-size histograms (2000 vs 2019) for
# All, Black, Hispanic, White — combined as white / hispanic / black

# ----- Libraries -----
library(ggplot2)
library(duckdb)
library(dplyr)
library(tidyr)
library(patchwork)
devtools::load_all("../dataduck")  # for crosstab_count()

# ----- Plot function (double overlay) -----
plot_hhsize_histogram_double <- function(
    data,
    per1 = 2000,
    per2 = 2019,
    bar_fills = list(
      per1 = list(color = "skyblue",  alpha = 0.4, line_type = "dashed"), # 2000
      per2 = list(color = "forestgreen", alpha = 0.8, line_type = "solid") # 2019
    ),
    title = NULL,
    xtitle = TRUE,
    ytitle = TRUE,
    ymax = NULL,
    add_legend = FALSE
) {
  # Expect a single group and both years
  if (length(unique(data$gender)) != 1) stop("Filter to one gender.")
  yrs <- sort(unique(data$year))
  if (!all(c(per1, per2) %in% yrs)) stop("Must contain both per1 and per2 years.")
  
  d1 <- data |> filter(year == per1) |> mutate(year_label = factor("2000", levels = c("2000", "2019")))
  d2 <- data |> filter(year == per2) |> mutate(year_label = factor("2019", levels = c("2000", "2019")))
  all_data <- bind_rows(d1, d2)
  if (is.null(ymax)) ymax <- max(all_data$freq) * 1.05
  
  ggplot(mapping = aes(x = factor(NUMPREC), y = freq)) +
    geom_bar(
      data = d1,
      aes(fill = year_label, color = year_label, linetype = year_label),
      stat = "identity", alpha = bar_fills$per1$alpha, width = 0.9, size = 0.3,
      show.legend = add_legend
    ) +
    geom_bar(
      data = d2,
      aes(fill = year_label, color = year_label, linetype = year_label),
      stat = "identity", alpha = bar_fills$per2$alpha, width = 0.6, size = 0.3,
      show.legend = add_legend
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_fill_manual(values  = c("2000" = bar_fills$per1$color, "2019" = bar_fills$per2$color),
                      guide = if (add_legend) "legend" else "none") +
    scale_color_manual(values = c("2000" = bar_fills$per1$color, "2019" = bar_fills$per2$color),
                       guide = if (add_legend) "legend" else "none") +
    scale_linetype_manual(values = c("2000" = bar_fills$per1$line_type, "2019" = bar_fills$per2$line_type),
                          guide = if (add_legend) "legend" else "none") +
    labs(
      title = title,
      x = if (xtitle) "Number of Persons in Household" else NULL,
      y = if (ytitle) "Frequency" else NULL,
      fill = NULL, color = NULL, linetype = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = if (add_legend) "bottom" else "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# ----- Data (DuckDB -> tidy) -----
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

hhsize_sex_2019 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2019 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "gender"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2019)

hhsize_sex_2000 <- crosstab_count(
  data = ipums_db |> filter(YEAR == 2000 & GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "gender"),
  every_combo = TRUE
) |> collect() |> mutate(year = 2000)

hhsize_all_yrs <- crosstab_count(
  data = ipums_db |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("NUMPREC", "YEAR"),
  every_combo = TRUE
) |> collect() |> rename(year = YEAR) |> mutate(gender = "All")

hhsize_sex_year <- bind_rows(hhsize_sex_2000, hhsize_sex_2019, hhsize_all_yrs)

topcode_hhsize <- 8
fig02_data <- hhsize_sex_year |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, topcode_hhsize, NUMPREC)) |>
  group_by(gender, NUMPREC, year) |>
  summarize(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  group_by(gender, year) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup() |>
  arrange(year, gender, NUMPREC)

# ----- Figure-specific styling -----
theme_2019_all <- list(color = "darkgrey",  alpha = 0.4, line_type = "solid")
theme_2000_all <- list(color = "lightgrey", alpha = 0.4, line_type = "dashed")
theme_2019      <- list(color = "forestgreen", alpha = 0.4, line_type = "solid")
theme_2000      <- list(color = "skyblue",     alpha = 0.4, line_type = "dashed")
ymax <- 0.35

# ----- Build panels -----
all <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(gender == "All"),
  title = "All", ymax = ymax, ytitle = TRUE, xtitle = FALSE,
  bar_fills = list(
    per1 = list(color = theme_2000_all$color, alpha = theme_2000_all$alpha, line_type = theme_2000_all$line_type),
    per2 = list(color = theme_2019_all$color, alpha = theme_2019_all$alpha, line_type = theme_2019_all$line_type)
  )
)

female <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(gender == "female"),
  title = "Female", ymax = ymax, ytitle = FALSE, xtitle = TRUE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_type = theme_2019$line_type)
  ),
  add_legend = TRUE
)

male <- plot_hhsize_histogram_double(
  data = fig02_data |> filter(gender == "male"),
  title = "Male", ymax = ymax, ytitle = TRUE, xtitle = FALSE,
  bar_fills = list(
    per1 = list(color = theme_2000$color, alpha = theme_2000$alpha, line_type = theme_2000$line_type),
    per2 = list(color = theme_2019$color, alpha = theme_2019$alpha, line_type = theme_2019$line_type)
  )
)


# ----- Combine (fig02) -----
fig02 <- female / male  # (matches your current layout)

# ----- Save -----
ggsave(
  "output/figures/fig02-hhsize-race-hist.png",
  plot = fig02,
  width = 2000, height = 5000, units = "px", dpi = 400
)
