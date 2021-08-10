#!/usr/bin/Rscript --vanilla
# ------------------------------------------------------------------------------
#
# Name: inst/figures/figure_5.R # nolint
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Generates  Figure 4 for the livestock value manuscript
#
#' Figure 4 - World Maps of Asset and Output Values
#' where each' Global World map showing the
#' total value of livestock assets + outputs
#' in each country. Use PPP to adjust for countries.
#'
#' Currently the map is split into 4  panes,
# ` with each pane showing
# `
#' A - Total value (PPP Adjusted Int ($))
#' B - Value Per capita (PPP Adjusted Int ($))
#'
# For detail on descriptions of each figure
# See:  output/figures/README.md#figure-descriptions # nolint
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications # nolint
# -------------------------------------------------------------------------


# Project -----------------------------------------------------------------

renv::activate(project = here::here("."))



# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggthemes)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(LivestockValueGBADS)
})



# Config ------------------------------------------------------------------

config <- config::get()


# Constants ---------------------------------------------------------------
date <- 2018


# Prep Data ---------------------------------------------------------------

# Function to prep and return data
# Just so it doesn't clog up the global environment
prep_data <- function() {


  # Output Datasets
  df_list <- config$data$output[
    c("crop_values", "aquaculture_values", "livestock_values", "ppp_conversion")
  ]

  # Load in the data
  data <- purrr::map(df_list, ~ arrow::read_parquet(.x, as_data_frame = TRUE) |>
    dplyr::filter(year == date))


  # Summarise by Country and produce a total value

  # Outputs
  data$livestock_outputs <- data$livestock_values |>
    filter(
      gross_production_value_current_thousand_slc > 0,
      tonnes > 0,
      item != "stock",
    ) |>
    drop_na(gross_production_value_current_thousand_slc) |>
    group_by(iso3_code) |>
    summarise(
      value = sum(gross_production_value_current_thousand_slc,
        na.rm = TRUE
      ) * 1000,
      .groups = "drop"
    )

  # Assets
  data$livestock_assets <- data$livestock_values |>
    filter(
      head > 0,
      stock_value_slc > 0,
      item == "stock"
    ) |>
    group_by(iso3_code) |>
    summarise(
      value = sum(stock_value_slc, na.rm = TRUE)
    )

  # Aquaculture
  data$aquaculture_outputs <- data$aquaculture_values |>
    filter(
      lcu_value > 0,
      tonnes > 0
    ) |>
    group_by(iso3_code) |>
    summarise(
      value = sum(lcu_value,
        na.rm = TRUE
      )
    )


  # World Bank Population File
  data$population <- LivestockValueGBADS::world_bank_population |>
    dplyr::filter(year == date) |>
    dplyr::rename(iso3_code = country_code)


  # Joint aquaculture and livestock outputs
  data$livestock_outputs <- data$livestock_outputs |>
    dplyr::bind_rows(data$aquaculture_outputs) |>
    dplyr::group_by(iso3_code) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
    dplyr::left_join(
      data$ppp_conversion,
      by = c("iso3_code"), suffix = c("", "_ppp")
    ) |>
    dplyr::mutate(
      value = value / value_ppp
    ) |>
    dplyr::left_join(data$population, by = c("iso3_code", "year")) |>
    dplyr::mutate(
      value_percap = value / population,
      .after = value_ppp
    )

  data$livestock_assets <- data$livestock_assets |>
    dplyr::group_by(iso3_code) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
    dplyr::left_join(
      data$ppp_conversion,
      by = c("iso3_code"), suffix = c("", "_ppp")
    ) |>
    dplyr::mutate(
      value = value / value_ppp
    ) |>
    dplyr::left_join(data$population, by = c("iso3_code", "year")) |>
    dplyr::mutate(
      value_percap = value / population,
      .after = value_ppp
    )

  data
}




# Helper Functions --------------------------------------------------------


# Function to cut values into bins and return as a factor
value_bins <- function(x, breaks, labels) {
  x <- cut(x, breaks, labels,
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
  )
  levels(x) <- levels(addNA(x))
  x
}

# Plots -------------------------------------------------------------------


# data
data <- prep_data()

# World data:
#
# Imports simple features for all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename(iso3_code = iso_a3)



# Summarise data

# Output data
output_map_data <- data$livestock_outputs %>%
  filter(iso3_code %in% world$iso3_code)


# asset data
asset_map_data <- data$livestock_assets %>%
  filter(iso3_code %in% world$iso3_code)


# Value data
world_output_value <- left_join(world, output_map_data, by = c("iso3_code"))
world_asset_value <- left_join(world, asset_map_data, by = c("iso3_code"))


# Breaks and labels
break_labels <- c("&lt;10B", "10B-50B", "50B-100B", "&gt; 100B")
break_vals <- c(0, 10e9, 50e9, 100e9, 100e20)
break_label_na <- c(break_labels, "NA")
cpal <- setNames(
  rev(c("#808080", "#225EA8", "#41B6C4", "#A1DAB4", "#FFFFCC")), break_label_na
)

# Per capita
break_labels_percap <- c("&lt;150", "150-500", "500-1000", "&gt; 1000")
break_vals_percap <- c(0, 150, 500, 1000, 1e9)
break_label_na_percap <- c(break_labels_percap, "NA")
cpal_percap <- setNames(
  rev(c("#808080", "#225EA8", "#41B6C4", "#A1DAB4", "#FFFFCC")),
  break_label_na_percap
)


plot_data <- function(df, value_col, fill_title, break_vals, break_labels, cpallette) {
  df <- df |>
    dplyr::select(name, iso3_code, {{ value_col }}, geometry) %>%
    dplyr::mutate(
      value_bins = value_bins({{ value_col }}, breaks = break_vals, labels = break_labels)
    )
  p <- ggplot2::ggplot(data = world) +
    ggplot2::geom_sf(fill = "#808080", color = "#D5E4EB") +
    ggplot2::geom_sf(
      data = df,
      aes(fill = value_bins), color = "#D5E4EB"
    ) +
    ggplot2::coord_sf(
      ylim = c(-55, 78),
      xlim = c(-180, 180)
    ) +
    ggplot2::scale_fill_manual(
      values = cpallette
    ) +
    ggplot2::labs(
      title = "",
      fill = fill_title,
      subtitle = "",
      caption = ""
    ) +
    ggplot2::guides(fill = guide_legend(nrow = 1)) +
    world_map_theme() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB")
    )
  p
}


# Total Values
p11 <- plot_data(world_asset_value,
  value,
  "Int.  $",
  break_vals,
  break_labels,
  cpal)
p12 <- plot_data(world_output_value,
  value,
  "Int.   $",
  break_vals,
  break_labels,
  cpal)

# Per capita
p21 <- plot_data(world_asset_value,
  value_percap,
  "Int. $  per   capita",
  break_vals_percap,
  break_labels_percap,
  cpal_percap)

p22 <- plot_data(world_output_value,
  value_percap,
  "Int.  $  per   capita",
  break_vals_percap,
  break_labels_percap,
  cpal_percap)



# Arrange -----------------------------------------------------------------
fig5 <- ggpubr::ggarrange(
  p11,
  p12,
  p21,
  p22,
  ncol = 2,
  nrow = 2,
  align = "hv",
  hjust = c(-0.4, -0.4, -0.3, -0.3),
  vjust = 4,
  legend = "bottom",
  labels = c(
    "A - Global Asset Value (2018) (PPP Int. $)",
    "B - Global Output Value (2018) (PPP Int. $)",
    "C - Global Asset Value (2018) (PPP Int. $ per capita)",
    "D - Global Output Value (2018) (PPP Int. $ per capita)"
  ),
  font.label = list(
    size = 14,
    color = "black",
    face = "italic",
    family = "sans"
  )
) +
  theme(
    panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
    plot.margin = margin(0, 0, 0, 0)
  )




# Save the png ------------------------------------------------------------

ggsave(
  plot = fig5,
  filename = "output/figures/figure_5.png",
  width = 16,
  height = 10,
  dpi = 300
)

# Tiff
ggsave(
  plot = fig5,
  filename = "output/figures/figure_5.tiff",
  width = 16,
  height = 10,
  dpi = 300,
  device  = "tiff"
)
