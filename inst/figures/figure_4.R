#!/usr/bin/Rscript --vanilla
# ------------------------------------------------------------------------------
#
# Name: inst/figures/figure_5.R
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Generates  Figure 4 for the livestock value manuscript
#
#' Figure 4 - World Maps of Asset + Output Values
#'
#' Global World map showing the total value of livestock assets + outputs
#' in each country. Use PPP to adjust for countries.
#'
#' Currently the map is split into two panes,
#' A - Total value (PPP Adjusted Int ($))
#' B - Value Per capita (PPP Adjusted Int ($))
# For detail on descriptions of each figure
# See:  output/figures/README.md#figure-descriptions
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications
# -------------------------------------------------------------------------


# Project -----------------------------------------------------------------

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
  library(glue)
  library(LivestockValueGBADS)
})



# Config ------------------------------------------------------------------

config <- config::get()
df_list <- config$data$output[c(
  "crop_values", "aquaculture_values",
  "livestock_values", "ppp_conversion"
)]


# Constants ---------------------------------------------------------------
date <- 2018

# Prep Data ---------------------------------------------------------------


# Load in the data
data <- purrr::map(
  df_list,
  ~ arrow::read_parquet(.x, as_data_frame = TRUE) |>
    dplyr::filter(year == date)
)


# Summarise by Country and produce a total value

# Outputs
data$livestock_outputs <- data$livestock_values |>
  filter(
    gross_production_value_current_thousand_slc > 0,
    tonnes > 0,
    item != "stock"
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



# Join the data and ensure the iso3_code matches before summarising
# and convert to PPP adjusted dollars
data_tev <- bind_rows(
  data$livestock_outputs,
  data$livestock_assets,
  data$aquaculture_outputs
) |>
  mutate(
    iso3_code = toupper(iso3_code)
  ) |>
  group_by(iso3_code) |>
  summarise(
    value = sum(value, na.rm = TRUE)
  ) |>
  left_join(data$ppp_conversion, by = c("iso3_code"), suffix = c("", "_ppp")) |>
  dplyr::mutate(
    value = value / value_ppp
  )


# World Bank Population File
population <- LivestockValueGBADS::world_bank_population |>
  dplyr::filter(year == date) |>
  dplyr::rename(iso3_code = country_code)



# Plots -------------------------------------------------------------------

# World data:
# Imports simple features for all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename(iso3_code = iso_a3)

# Summarise data
map_data <- data_tev %>%
  filter(iso3_code %in% world$iso3_code)


world_value <- left_join(world, map_data, by = c("iso3_code"))


# Bin the data into appropriate bins
world_value <- select(world_value, name, iso3_code, value, geometry) %>%
  mutate(
    value_bins = factor(case_when(
      value < 10e9 ~ "&lt;10B",
      value < 50e9 ~ "10-50B",
      value < 100e9 ~ "50-100B",
      value >= 100e9 ~ "&gt; 100B",
      TRUE ~ "NA"
    ),
    levels = rev(c(
      "NA", "&lt;10B",
      "10-50B",
      "50-100B",
      "&gt; 100B"
    ))
    )
  )

p1 <- ggplot2::ggplot(data = world) +
  ggplot2::geom_sf(fill = "#808080", color = "#D5E4EB") +
  ggplot2::geom_sf(data = world_value, aes(fill = value_bins), color = "#D5E4EB") +
  ggplot2::coord_sf(ylim = c(-55, 78), xlim = c(-180, 180)) +
  ggplot2::scale_fill_manual(
    values = rev(list(
      "NA" = "#808080",
      "&gt; 100B" = "#225EA8",
      "50-100B" = "#41B6C4",
      "10-50B" = "#A1DAB4",
      "&lt;10B" = "#FFFFCC"
    ))
  ) +
  ggplot2::labs(
    title = "",
    fill = "Int ($)",
    subtitle = "",
    caption = ""
  ) +
  ggplot2::guides(fill = guide_legend(nrow = 1)) +
  world_map_theme()



# World data:
# Imports simple features for all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename(iso3_code = iso_a3)

# Summarise data
map_data <- data_tev %>%
  filter(iso3_code %in% world$iso3_code) |>
  left_join(population, by = c("iso3_code", "year")) |>
  dplyr::mutate(
    value = value / population
  )
world_value <- left_join(world, map_data, by = c("iso3_code"))


# Bin the data into appropriate bins
world_value <- select(world_value, name, iso3_code, value, geometry) %>%
  mutate(
    value_bins = factor(case_when(
      value < 150 ~ "&lt;150",
      value < 500 ~ "150-500",
      value < 1000 ~ "500-1000",
      value >= 1000 ~ "&gt;1000",
      TRUE ~ "NA"
    ),
    levels = rev(c(
      "NA", "&lt;150",
      "150-500",
      "500-1000",
      "&gt;1000"
    ))
    )
  )

p2 <- ggplot(data = world) +
  geom_sf(fill = "#808080", color = "#D5E4EB") +
  geom_sf(
    data = world_value,
    aes(fill = value_bins),
    color = "#D5E4EB"
  ) +
  coord_sf(ylim = c(-55, 78)) +
  scale_fill_manual(
    values = rev(list(
      "NA" = "#808080",
      "&gt;1000" = "#225EA8",
      "500-1000" = "#41B6C4",
      "150-500" = "#A1DAB4",
      "&lt;150" = "#FFFFCC"
    ))
  ) +
  labs(
    title = "",
    fill = "Int ($) per capita",
    subtitle = "",
    caption = ""
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  world_map_theme()

p <- ggpubr::ggarrange(
  p1,
  p2,
  ncol = 1,
  nrow = 2,
  legend = "bottom",
  labels = c(
    "A - Global Value of Livestock and Aquaculture (2018) (PPP Int ($))",
    paste0(
      "B - Global Value of Livestock and Aquaculture (2018)",
      " (PPP Int ($) per capita)"
      )
  ),
  hjust = c(-0.7, -0.6),
  font.label = list(
    size = 14,
    color = "black",
    face = "italic",
    family = "sans"
  )
)


# Save the png ------------------------------------------------------------
ggsave(
  plot = p,
  filename = "output/figures/figure_4.png",
  width = 16,
  height = 12,
  dpi = 300,
  device = "png"
)

# Tiff 
ggsave(
  plot = p,
  filename = "output/figures/figure_4.tiff",
  width = 16,
  height = 12,
  dpi = 300,
  device = "tiff"
)
