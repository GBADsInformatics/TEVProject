#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
#
# Name: inst/figures/generate-figures.R
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Generates different figures for the livestock value manuscript
#
# For detail on descriptions of each figure
# See:  output/figures/README.md#figure-descriptions
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications
#
# Figure themes are added based on the figure specifications
#
# -------------------------------------------------------------------------


# Project Library ---------------------------------------------------------
renv::activate(project = ".")

# Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(ggthemes)
  library(hrbrthemes)
  library(logging)
  library(tidyr)
  library(rnaturalearth)
  library(sf)
  library(ggtext)
})
# -------------------------------------------------------------------------



# Import output files -----------------------------------------------------
config <- config::get()

# Nature Food Figure Specification
figure_spec <- config$figure_specification$nature_food


# Have to remove this
theme_set(
  panel_plot_theme()
)


# Preparation functions ---------------------------------------------------
config <- config::get()








# ISVEE Plots  ------------------------------------------------------------



#' figure_asset_map
#'
#' Generates a map a global map of Asset/Output Values
#'
#' FIXME: this needs to be refactored
#'
#' @param df_file aggregated dataframe with columns "iso3_code", "year", "value
#' @param date integer year to plot (defaults to 2018)
#' @param use_pdf whether to use a pdf or not
#'
figure_year_map <- function(df,
                            .title,
                            .fig_name,
                            date = 2018,
                            use_pdf = TRUE) {



  # Imports simple features for all countries in the world
  world <-
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::rename(iso3_code = iso_a3)


  world_value <- world |>
    dplyr::left_join(df, by = "iso3_code") |>
    tidyr::drop_na(value)



  # Bin the data into appropriate bins
  cut_labels <- c(
    "NA" = "#808080",
    "&lt;500M" = "#FFFFCC",
    "500M-2.5B" = "#A1DAB4",
    "2.5B-10B" = "#41B6C4",
    "10B-40B" = "#225EA8",
    "&gt;40B" = "#00008b"
  )

  world_value <- dplyr::select(world_value, name, iso3_code, value, geometry) %>%
    dplyr::mutate(
      value_bins = cut(value,
        breaks = c(0, 5e8, 2.5e9, 10e9, 40e9, Inf),
        labels = names(cut_labels)[2:length(cut_labels)],
        ordered_result = TRUE
      )
    )

  p <- ggplot(data = world) +
    geom_sf(fill = "#808080", color = "#D5E4EB", size = 0.1) +
    geom_sf(data = world_value, aes(fill = value_bins), color = "#D5E4EB", size = 0.1) +
    coord_sf(ylim = c(-55, 78)) +
    scale_fill_manual(
      values = cut_labels
    ) +
    labs(
      title = .title,
      fill = "USD ($)",
      subtitle = "",
      caption = "All values in constant 2014-2016 USD ($)"
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    world_map_theme()



  device <- ifelse(use_pdf, "pdf", "png")
  output_file_name <- here::here("output", "figures", glue::glue("{.fig_name}.{device}"))

  ggsave(
    plot = p,
    filename = output_file_name,
    width = figure_spec$two_column_width,
    height = figure_spec$two_column_width * (1 / 1.6),
    units = figure_spec$units,
    dpi = figure_spec$dpi,
    device = device
  )
}


config$data$output$livestock_value |>
  arrow::read_parquet() |>
  dplyr::filter(item == "stock", year == 2018) |>
  filter(
    !is.nan(stock_value_constant_2014_2016_usd),
    !is.na(stock_value_constant_2014_2016_usd)
  ) |>
  group_by(iso3_code) |>
  summarise(
    value = sum(stock_value_constant_2014_2016_usd, rm.na = TRUE)
  ) -> asset_df

config$data$output$livestock_value |>
  arrow::read_parquet() |>
  dplyr::filter(item != "stock", year == 2018) |>
  group_by(iso3_code) |>
  summarise(
    value = sum(gross_production_value_constant_2014_2016_thousand_us, na.rm = TRUE) * 1e3
  ) |>
  filter(value > 0) -> output_df


config$data$output$aquaculture_values |>
  arrow::read_parquet() |>
  filter(year == 2018) |>
  group_by(iso3_code) |>
  summarise(value = sum(constant_2014_2016_usd_value, na.rm = TRUE)) -> aqua_df

output_aqua_df <- rbind(output_df, aqua_df) |>
  group_by(iso3_code) |>
  summarise(value = sum(value, na.rm = TRUE))



# Asset parameters
asset_map_params <- list(
  df = asset_df,
  .title = "Global Value of Livestock Assets (2018)",
  .fig_name = "figure_a_asset_map_2018",
  date = 2018,
  use_pdf = FALSE
)

do.call(figure_year_map, asset_map_params)

# Asset parameters
output_map_params <- list(
  df = output_df,
  .title = "Global Value of Livestock Outputs (2018)",
  .fig_name = "figure_a_output_map_no_aquaculture_2018",
  date = 2018,
  use_pdf = FALSE
)

do.call(figure_year_map, output_map_params)

output_aqua_map_params <- list(
  df = output_aqua_df,
  .title = "Global Value of Livestock and Aquaculture Outputs (2018)",
  .fig_name = "figure_a_output_map_2018",
  date = 2018,
  use_pdf = FALSE
)

do.call(figure_year_map, output_aqua_map_params)
