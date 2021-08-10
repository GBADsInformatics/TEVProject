#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
#
# Name: inst/figures/figure_A1.R #nolint
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Generates  Figure A1 for the livestock value manuscript
#
# Shows the
# Figure A.1: Global spatial distribution of the economic asset value by livestock type in 2018 # nolint
# in 2018 PPP adjusted Int. $
#
# For detail on descriptions of each figure
# See: output/figures/README.md#figure-descriptions #nolint
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications #nolint
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
  library(tidyr)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(LivestockValueGBADS)
})
# -------------------------------------------------------------------------

# Import output files -----------------------------------------------------
config <- config::get()

# -------------------------------------------------------------------------

#  Livestock Data file
df_file <- config$data$output$livestock_values

# Aquaculture File
aqua_file <- config$data$output$aquaculture_values

# Constants

# Year to filter for
date <- 2018


# Animal categories to use,
# with the addition of "Other Livestock"
animals <- c("Cattle", "Sheep", "Goat", "Chicken", "Pig")


# -------------------------------------------------------------------------

# Load in the data

# Select all items which have LCU/SLC stock values for the year in question
# group by animals, and aggregate the selected item
data <- arrow::read_parquet(df_file, as_data_frame = TRUE) |>
  dplyr::mutate(
    category = case_when(
      animal %in% tolower(animals) ~ stringr::str_to_title(animal),
      TRUE ~ "Other Livestock"
    )
  ) %>%
  dplyr::filter(
    year == date,
    item == "stock",
     # Using slc because in FAOSTAT SLC is equal to LCU for the conversion
     (stock_value_slc > 0)
  ) %>%
  dplyr::group_by(iso3_code, category) %>%
  dplyr::summarise(
    value_slc = sum(stock_value_slc, na.rm = TRUE),
    .groups = "drop"
  )

# PPP Conversion Tables
# Contains LCU to Int. $ Conversion ratios
ppp_conversion <- arrow::read_parquet(config$data$output$ppp_conversion) |>
    dplyr::filter(year == date) |>
    dplyr::select(
        iso3_code,
        ppp_conversion = value
    )


# Convert the SLC stock/Asset values to 2018 PPP Int. $
data <- data |>
    dplyr::left_join(
        ppp_conversion, by = "iso3_code"
    ) |>
    dplyr::mutate(
        value = value_slc / ppp_conversion
    )

# -------------------------------------------------------------------------


# World data:
# Imports simple features for all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename(iso3_code = iso_a3)



world_value <- world |>
  dplyr::left_join(data, by = "iso3_code") |>
  tidyr::drop_na(value)



# Bin the data into appropriate bins
cut_labels <- c(
  "NA" = "#808080",
  "&lt;50M" = "#FFFFCC",
  "50M-500M" = "#A1DAB4",
  "500M-10B" = "#41B6C4",
  "&gt;10B" = "#225EA8"
)


# Bin into values
world_value <- dplyr::select(
  world_value,
  name,
  iso3_code,
  category,
  value,
  geometry
) %>%
  dplyr::mutate(
    value_bins = cut(value,
      breaks = c(0, 50e6, 500e6, 10e9, Inf),
      labels = names(cut_labels)[2:length(cut_labels)],
      ordered_result = TRUE
    ),
    category = forcats::fct_reorder(category, value, sum, .desc = TRUE)
  )



# Figure A1 Plot ----------------------------------------------------------
#https://stackoverflow.com/questions/34805506/adjust-title-vertically-to-inside-the-plot-vjust-not-working # nolint

fig_a1 <- ggplot(data = world) +
  geom_sf(fill = "#808080", color = "#D5E4EB", size = 0.1) +
  geom_sf(
      data = world_value,
      aes(fill = value_bins),
      color = "#D5E4EB",
      size = 0.1
   ) +
  coord_sf(
      ylim = c(-55, 78),
      xlim = c(-180, 180)
  ) +
  scale_fill_manual(
    values = cut_labels
  ) +
  facet_wrap(~category, nrow = 2) +
  labs(
    title = "Global spatial distribution of the economic asset value by livestock type in 2018",
    fill = "Int. $",
    subtitle = "",
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  world_map_theme() +
  theme(
    plot.margin = margin(t  = 50, r = 25, l = 25, b = 25),
    strip.text.x = element_text(
        size = 15, face = "italic", margin = margin(b = 20)),
    plot.title = ggtext::element_markdown(
      face = "italic", margin = margin(t = 10, b = 30))
  )



# Save  -------------------------------------------------------------------
ggsave(
  plot = fig_a1,
  filename = "output/figures/figure_A1.png",
  width = 20,
  height = 8,
  dpi = 300, 
  device = "png"
)

ggsave(
  plot = fig_a1,
  filename = "output/figures/figure_A1.tiff",
  width = 20,
  height = 8,
  dpi = 300, 
  device = "tiff"
)
