#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
# nolint start
# Name: "inst/figures/figure_6.R"
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
# nolint end 
# -------------------------------------------------------------------------


# Project  ----------------------------------------------------------------
renv::activate(project = ".")

# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggthemes)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(ggtext)
  library(LivestockValueGBADS)
})


# Config ------------------------------------------------------------------

config <- config::get()




# Asset Values ------------------------------------------------------------

local({
  # Local environment due to combining multiple figures

  # Constants ---------------------------------------------------------------
  year_range <- 2005:2018
  df_file <- config$data$output$livestock_values


  # Data Prep ---------------------------------------------------------------

  # Load in the livestock value data
  data <- arrow::read_parquet(df_file, as_data_frame = TRUE) |>
    dplyr::filter(year %in% year_range) |>
    dplyr::mutate(iso3_code = toupper(iso3_code))


  # Load the world bank poulation dataset
  population <- LivestockValueGBADS::world_bank_population |>
    dplyr::filter(year %in% year_range) |>
    dplyr::rename(iso3_code = country_code)



  # Livestock Assets
  pct_vec <- c(
    "&lt; -2.5%",
    "-2.5% - 0%",
    "0% - 1%",
    "1% - 2.5%",
    "2.5% - 5%",
    "5% - 10%",
    "10%&lt;")

  assets <- data |>
    dplyr::filter(
      head > 0,
      stock_value_constant_2014_2016_usd > 0,
      item == "stock"
    ) |>
    dplyr::mutate(value = stock_value_constant_2014_2016_usd) |>
    dplyr::group_by(iso3_code, year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(population, by = c("iso3_code", "year")) |>
    dplyr::group_by(iso3_code) |>
    dplyr::arrange(year) %>%
    dplyr::mutate(
      value_percap = value / population
    ) |>
    dplyr::mutate(
      value = 100 * (value - lag(value)) / (lag(value)) / (year - lag(year))
    ) %>%
    dplyr::mutate(
      value_percap = 100 * (value_percap - lag(value_percap)) / (lag(value_percap)) / (year - lag(year))
    ) %>%
    dplyr::summarise(
      avg_change = mean(value, na.rm = TRUE),
      avg_change_percap = mean(value_percap, na.rm = TRUE), .groups = "drop"
    ) %>%
    dplyr::mutate(
      change_vals = as.factor(
        cut(avg_change,
          breaks = c(-Inf, -2.5, 0, 1, 2.5, 5, 10, Inf),
          labels = pct_vec
        )
      ),
      change_vals_percap = as.factor(
        cut(avg_change_percap,
          breaks = c(-Inf, -2.5, 0, 1, 2.5, 5, 10, Inf),
          labels = pct_vec
        )
      )
    )





  # Plot Countries ----------------------------------------------------------
  cmap <- setNames(
    c(
      "#808080", "#E64B35FF", "#F39B7FFF", "#F7FCF5",
      "#74C476", "#41AB5D", "#238B45", "#005A32"
    ),
    c("NA", pct_vec)
  )

  # World data:
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::rename(iso3_code = iso_a3) |>
    left_join(assets, by = "iso3_code")


  # Per Capita Plot ---------------------------------------------------------
  p11 <<- ggplot(world) +
    geom_sf(fill = "#808080", color = "#D5E4EB") +
    geom_sf(aes(fill = change_vals), color = "#D5E4EB") +
    coord_sf(ylim = c(-55, 78)) +
    scale_fill_manual(
      values = cmap
    ) +
    labs(
      title = "",
      fill = "",
      subtitle = ""
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    world_map_theme() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB")
    )


  # Per Capita Plot ---------------------------------------------------------


  p21 <<- ggplot(world) +
    geom_sf(fill = "#808080", color = "#D5E4EB") +
    geom_sf(aes(fill = change_vals_percap), color = "#D5E4EB") +
    coord_sf(ylim = c(-55, 78)) +
    scale_fill_manual(
      values = cmap
    ) +
    labs(
      title = "",
      fill = "",
      subtitle = ""
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    world_map_theme() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB")
    )
})


# Output Values  ----------------------------------------------------------

local({

  # Constants ---------------------------------------------------------------
  year_range <- 2005:2018

  df_list <- config$data$output[c(
    "aquaculture_values",
    "livestock_values"
  )]


  # Prep Data ---------------------------------------------------------------

  # Load in the data
  data <- purrr::map(
    df_list,
    ~ arrow::read_parquet(.x, as_data_frame = TRUE) |>
      dplyr::filter(year %in% year_range) |>
      dplyr::mutate(iso3_code = toupper(iso3_code))
  )

  # Summarise by Country and produce a total value
  data$livestock_outputs <- data$livestock_values |>
    dplyr::filter(
      gross_production_value_constant_2014_2016_thousand_us > 0,
      tonnes > 0
    ) |>
    tidyr::drop_na(gross_production_value_constant_2014_2016_thousand_us) |>
    dplyr::group_by(iso3_code, year) |>
    dplyr::summarise(
      value = sum(gross_production_value_constant_2014_2016_thousand_us,
        na.rm = TRUE
      ) * 1000,
      .groups = "drop"
    ) |> 
      dplyr::mutate(is_aqua = FALSE)

  data$aquaculture_outputs <- data$aquaculture_values |>
    dplyr::filter(
      constant_2014_2016_usd_value > 0,
      tonnes > 0
    ) |>
    dplyr::group_by(iso3_code, year) |>
    dplyr::summarise(
      value = sum(constant_2014_2016_usd_value,
        na.rm = TRUE
      )
    ) |> 
      dplyr::mutate(is_aqua = TRUE)

  # Load the world bank poulation dataset
  population <- LivestockValueGBADS::world_bank_population |>
    dplyr::filter(year %in% year_range) |>
    dplyr::rename(iso3_code = country_code)



  # Compute Outputs ---------------------------------------------------------
  pct_vec <- c(
    "&lt; -2.5%",
    "-2.5% - 0%",
    "0% - 1%",
    "1% - 2.5%",
    "2.5% - 5%",
    "5% - 10%", "10%&lt;")

   outputs <- 
      bind_rows(data$livestock_outputs, data$aquaculture_outputs) |>
    dplyr::group_by(iso3_code, year) |>
       filter(!all(is_aqua)) |> 
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(population, by = c("iso3_code", "year")) |>
    dplyr::group_by(iso3_code) |>
    dplyr::arrange(year) %>%
    dplyr::filter(value > 0) |> 
    dplyr::mutate(
      value_percap = value / population
    ) |>
    dplyr::mutate(
      value_pct_change = 100 * (value - lag(value)) / (lag(value)) / (year - lag(year))
    ) |> 
    dplyr::mutate(
      value_percap = 100 * (value_percap - lag(value_percap)) / (lag(value_percap)) / (year - lag(year))
    ) |> dplyr::group_by(iso3_code) |> 
     dplyr::mutate(
      max_change = max(abs(value_pct_change), na.rm = TRUE), 
     )  |> 
    dplyr::summarise(
      avg_change = mean(value_pct_change, na.rm = TRUE),
      avg_change_percap = mean(value_percap, na.rm = TRUE), .groups = "drop"
    ) |> 
    dplyr::mutate(
      change_vals = as.factor(
        cut(avg_change,
          breaks = c(-Inf, -2.5, 0, 1, 2.5, 5, 10, Inf),
          labels = pct_vec
        )
      ),
      change_vals_percap = as.factor(
        cut(avg_change_percap,
          breaks = c(-Inf, -2.5, 0, 1, 2.5, 5, 10, Inf),
          labels = pct_vec
        )
      )
    )


  
  # Plots -------------------------------------------------------------------

  # World data:
  # Imports simple features for all countries in the world
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::rename(iso3_code = iso_a3) |>
    dplyr::left_join(outputs, by = "iso3_code")

  # Color map
  cmap <- setNames(
    c(
      "#808080", "#E64B35FF", "#F39B7FFF", "#F7FCF5",
      "#74C476", "#41AB5D", "#238B45", "#005A32"
    ),
    c("NA", pct_vec)
  )



  p12 <<- ggplot(world) +
    geom_sf(fill = "#808080", color = "#D5E4EB") +
    geom_sf(aes(fill = change_vals), color = "#D5E4EB") +
    coord_sf(ylim = c(-55, 78)) +
    scale_fill_manual(
      values = cmap
    ) +
    labs(
      title = "",
      fill = "",
      subtitle = ""
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    world_map_theme() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB")
    )

  p22 <<- ggplot(world) +
    geom_sf(fill = "#808080", color = "#D5E4EB") +
    geom_sf(aes(fill = change_vals_percap), color = "#D5E4EB") +
    coord_sf(ylim = c(-55, 78)) +
    theme_economist() +
    scale_fill_manual(
      values = cmap
    ) +
    labs(
      title = "",
      fill = "",
      subtitle = ""
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    world_map_theme() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB")
    )
})


# Figure 6 ----------------------------------------------------------------
# Combined Plot

fig6 <- ggpubr::ggarrange(
  p11,
  p12,
  p21,
  p22,
  ncol = 2,
  nrow = 2,
  legend = "bottom",
  labels = c(
    "A – Average asset value change (%) per year (2005-2018)",
    "B – Average output value change (%) per year (2005-2018)",
    "C – Average asset value change per capita (%) per year (2005-2018)",
    "D – Average output value change per capita (%) per year (2005-2018)"
  ),
  hjust = c(-0.4, -0.4, -0.3, -0.3),
  align = "hv",
  font.label = list(
    size = 14,
    color = "black",
    face = "italic",
    family = "sans"
  ),
  common.legend = TRUE
) +
  theme(
    panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
    plot.margin = margin(0, 0, 0, 0)
  )


# Save  -------------------------------------------------------------------
ggsave(
  plot = fig6,
  filename = "output/figures/figure_6.png",
  width = 18,
  height = 12,
  dpi = 300
)


ggsave(
  plot = fig6,
  filename = "docs/figure_6_update.png",
  width = 18,
  height = 12,
  dpi = 300
)
# Tiff
ggsave(
  plot = fig6,
  filename = "output/figures/figure_6.tiff",
  width = 18,
  height = 12,
  dpi = 300, 
  device = "tiff"
)
