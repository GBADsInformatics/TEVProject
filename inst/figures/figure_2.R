#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
# nolint start
# Name: inst/figures/generate-figures.R
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Generates  Figure 2 for the livestock value manuscript
#
# For detail on descriptions of each figure
# See:  output/figures/README.md#figure-descriptions
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications
# nolint end 
# -------------------------------------------------------------------------


# Project Library ---------------------------------------------------------
renv::activate(project = ".")

# Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(grid)
  library(gtable)
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(ggthemes)
  library(hrbrthemes)
  library(tidyr)
  library(LivestockValueGBADS)
})
# -------------------------------------------------------------------------



# Import output files -----------------------------------------------------
config <- config::get()


# Have to remove this
theme_set(
  panel_plot_theme()
)


# Preparation functions ---------------------------------------------------
config <- config::get()

# Function to prepare data for initial plots
prep_data <- function() {

  # List of data frames
  df_list <- purrr::keep(
    config$data$output,
    ~ grepl("values", .x, ignore.case = TRUE)
  )

  # List to store outputs
  data <- list()

  # Livestock Value
  livestock_value <- df_list$livestock_values |>
    arrow::read_parquet() |>
    dplyr::mutate(
      gross_production_value_constant_2014_2016_usd = gross_production_value_constant_2014_2016_thousand_us * 1e3 # nolint
    )

  # Livestock Asset Value
  data$livestock_asset <- get_livestock_asset_output_value(livestock_value)


  # Livestock Output Value
  data$livestock_output <- get_livestock_asset_output_value(
    livestock_value,
    gross_production_value_constant_2014_2016_usd
  )

  # Livestock Aquaculture value
  data$aquaculture_value <- df_list$aquaculture_values |>
    arrow::read_parquet() |>
    get_aquaculture_value()


  data$livestock_output <- data$livestock_output |>
    bind_rows(data$aquaculture_value) |>
    dplyr::group_by(
      year
    ) |>
    dplyr::mutate(
      percentage = value / sum(value)
    ) |>
    dplyr::mutate(
      category = forcats::fct_reorder(category, value, mean, .desc = FALSE)
    )


  # Crop Output value
  data$crop_value <- df_list$crop_values |>
    arrow::read_parquet() |>
    dplyr::mutate(
      gross_production_value_constant_2014_2016_usd = gross_production_value_constant_2014_2016_thousand_us * 1e3 # nolint
    ) |>
    get_crop_value()


  # Human Population
  data$population <- LivestockValueGBADS::world_bank_population |>
    dplyr::filter(
      year %in% seq(1998, 2018, by = 5),
      country_code %in%
        toupper(unique(livestock_value$iso3_code))
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      population = sum(population, na.rm = TRUE)
    )

  data
}

# -------------------------------------------------------------------------

# Get data for initial plots
data <- prep_data()


# -------------------------------------------------------------------------

# Years to be plotted
years <- seq(1998, 2018, by = 5)


# Create a list of data frames of the totals for all plots shown in column 2
# Scaling function
weight_scale <- scales::comma_format(scale = 1e-6, suffix = " M", accuracy = 1)



# Function to format percentages
pct_format <- function(x, accuracy = 1) {
  scales::percent(
    x,
    accuracy = accuracy
  )
}


# Function to add segments to % labels
add_segments <- function(p) {
  pvars <- ggplot_build(p)


  segment_df <- pvars$data[[2]] |>
    dplyr::filter(grepl("[%]", label)) |>
    dplyr::select(
      x = x, y = y, yend = y
    ) |>
    dplyr::mutate(
      xend = x + 1.3,
    ) |>
    dplyr::mutate(
      x = x + 0.6
    )

  p + geom_segment(
    data = segment_df,
    aes(x = x, xend = xend, y = y, yend = yend), inherit.aes = FALSE,
    size = 0.4,
    alpha = 1
  )
}

# Function to get the manuals scale fill values
# Adds crops at the end so that they will be part of the
# common legend
get_fill_values <- function(fct) {
  if (!is.factor(fct)) {
    fct <- as.factor(fct)
  }
  f_vals <- rev(nature_color_scheme()[levels(fct)])
  unused_levels <- setdiff(names(nature_color_scheme()), names(f_vals))
  if (length(unused_levels) == 0) {
    return(f_vals)
  } else {
    return(
      c(f_vals, nature_color_scheme()[unused_levels])
    )
  }
}

# Plotting function

# Left Column
plot_panel_col_1 <- function(df,
                             value_col,
                             fill_values,
                             total_offset = 0.05,
                             pct_text_size = 3,
                             total_text_size = 3,
                             pct_lower_bound = 0.05,
                             scale_fun = scale_trill,
                             y_axis_title = NULL) {
  years <- sort(unique(df$year))

  p <- df |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      percentage = {{ value_col }} / sum({{ value_col }})
    ) |>
    ggplot(
      aes(
        x = year,
        y = {{ value_col }},
        fill = category,
        group = category,
        label = case_when(
          (percentage > pct_lower_bound) & (percentage < 1) ~ scales::percent(percentage, accuracy = 1),
          TRUE ~ ""
        )
      )
    ) +
    geom_bar(
      position = "stack",
      stat = "identity",
      color = "white",
      width = 1.5,
      size = 0
    ) +
    geom_text(
      aes(x = year - 1.5),
      fontface = "italic",
      size = pct_text_size,
      position = position_stack(0.5)
    ) +
    geom_text(
      data = df |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          value = sum({{ value_col }}, na.rm = TRUE)
        ),
      aes(x = year, y = value, label = scale_trill(value)),
      fontface = "italic",
      size = total_text_size,
      position = position_stack(1 + total_offset),
      inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(labels = scale_fun) +
    scale_fill_manual(
      values = fill_values
    )

  p <- p +
    labs(
      x = NULL,
      y = y_axis_title
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    panel_plot_theme() +
    theme(
      legend.title = element_blank()
    )

  p |>
    add_segments()
}


# Right Column
plot_panel_col_2 <- function(df,
                             value_col,
                             fill_values,
                             population_df = data$population,
                             y1_axis_title = NULL,
                             y2_axis_title = "Human Population",
                             coef = 1,
                             total_offset = 0.05,
                             total_text_size = 3,
                             pct_text_size = 3,
                             pct_lower_bound = 0.05,
                             scale_fun = scale_trill) {
  years <- sort(unique(df$year))
  p <- df |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      percentage = {{ value_col }} / sum({{ value_col }}, na.rm = TRUE)
    ) |>
    ggplot(
      aes(
        x = year,
        y = {{ value_col }},
        fill = category,
        group = category,
        label = case_when(
          (percentage > pct_lower_bound) & (percentage < 1) ~ scales::percent(percentage, accuracy = 1),
          TRUE ~ ""
        )
      )
    ) +
    geom_bar(
      position = "stack",
      stat = "identity",
      color = "white",
      size = 0,
      width = 1.5
    ) +
    geom_text(
      aes(x = year - 1.5),
      fontface = "italic",
      size = pct_text_size,
      position = position_stack(0.5)
    ) +
    geom_text(
      data = df |>
        dplyr::group_by(year) |>
        dplyr::summarize(value = sum({{ value_col }})),
      aes(
        x = year,
        y = value,
        label = scale_fun(value)
      ),
      position = position_stack(vjust = 1 + total_offset),
      inherit.aes = FALSE,
      size = total_text_size,
      fontface = "italic"
    ) +
    geom_label(
      data = population_df,
      aes(
        x = year,
        y = (population / coef) * (1 + 2 * total_offset),
        label = scale_fun(population)
      ),
      fill = "black",
      color = "white",
      size = 3,
      inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = years) +
    scale_y_continuous(
      labels = scale_fun,
      name = y1_axis_title,
      sec.axis = sec_axis(~ . * coef,
        name = y2_axis_title,
        labels = scale_fun
      )
    ) +
    geom_point(
      data = population_df,
      aes(x = year, y = population / coef),
      size = 1,
      shape = 4,
      show.legend = FALSE,
      color = "black",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = fill_values
    )




  p <- p + guides(fill = guide_legend(nrow = 1)) +
    labs(
      x = NULL
    ) +
    panel_plot_theme() +
    theme(
      legend.title = element_blank()
    )


  # Retrieve the ggplot axis ranges/limits
  # Inspired by the following github gist
  # https://gist.github.com/tomhopper/9076152
  x_range <- ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
  y_range <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range

# Add on the custom Population Legend Annotation in the Top Left Corner
  p <- p + 
    annotation_custom(
      grob = grid::gTree(
        children = gList(
          grid::rectGrob(width = 1.2, height = 0.8),
          grid::textGrob(
          label = "Human Population - x",
          gp = gpar(fontface = "italic", fontsize = 10))
        )
      ),
      xmin = min(x_range),
      xmax = years[2] - 1,
      ymin = 0.9 * max(y_range),
      ymax = max(y_range)
    )

  p |>
    add_segments()
}


# Value -------------------------------------------------------------------


p11 <- plot_panel_col_1(
  df = data$livestock_asset,
  value_col = value,
  fill_values = get_fill_values(data$livestock_asset$category),
  y_axis_title = "Market value in constant USD (Trillion)"
)

p21 <- plot_panel_col_1(
  df = data$livestock_output,
  value_col = value,
  fill_values = get_fill_values(data$livestock_output$category),
  y_axis_title = "Market value in constant USD (Trillion)",
)

p31 <- plot_panel_col_1(
  df = data$crop_value,
  value_col = value,
  fill_values = get_fill_values(data$crop_value$category),
  y_axis_title = "Market value in constant USD (Trillion)"
)



# Weight ------------------------------------------------------------------
p12 <- plot_panel_col_2(
  df = data$livestock_asset,
  value_col = tonnes,
  fill_values = get_fill_values(data$livestock_asset$category),
  coef = 10,
  scale_fun = scales::comma_format(scale = 1e-9, accuracy = .01, suffix = " B"),
  y1_axis_title = "Live animals (Tonnes)",
  pct_lower_bound = 0.05
)

p22 <- plot_panel_col_2(
  df = data$livestock_output,
  value_col = tonnes,
  fill_values = get_fill_values(data$livestock_output$category),
  scale_fun = scales::comma_format(scale = 1e-9, accuracy = .01, suffix = " B"),
  coef = 5,
  y1_axis_title = "Animal outputs (Tonnes)",
  pct_lower_bound = 0.06
)

p32 <- plot_panel_col_2(
  df = data$crop_value,
  value_col = tonnes,
  fill_values = get_fill_values(data$crop_value$category),
  coef = .7,
  scale_fun = scales::comma_format(scale = 1e-9, accuracy = .01, suffix = " B"),
  y1_axis_title = "Crop outputs (Tonnes)"
)


# Use GGarange to create the Figure ---------------------------------------

fig2 <- ggpubr::ggarrange(
  p11, p12, p21, p22, p31, p32,
  nrow = 3, ncol = 2,
  legend = "bottom",
  common.legend = TRUE,
  hjust = c(-0.2, -0.2, -0.2, -0.2, -0.2, -0.2),
  font.label = list(
    size = 14,
    color = "black",
    face = "italic",
    family = "sans"
  ),
  labels = c(
    "A - Total global farmed animal asset value",
    "B - Total global farmed animal liveweight mass",
    "C - Total global farmed animal output value",
    "D - Total global farmed animal output mass",
    "E - Total global crop output value",
    "F - Total global crop output mass"
  ),
  align = "hv"
)


# Save Output -------------------------------------------------------------

# PNG 
ggsave(
  plot = fig2,
  filename = "output/figures/figure_2.png",
  width = 16,
  height = 10,
  dpi = 300,
  device = "png"
)

# Tiff
ggsave(
  plot = fig2,
  filename = "output/figures/figure_2.tiff",
  width = 16,
  height = 10,
  dpi = 300,
  device = "tiff"
)
