#!/usr/bin/Rscript --vanilla

# ------------------------------------------------------------------------------
#
# Name: "inst/figures/figure_A2.R" #nolint
# Project: GBADS
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Figure A.2: Global spatial distribution of the economic value of direct farmed animal outputs (e.g., meat, milk, eggs, fish) in 2018
#
# This  figure contains a world map plot showing the  a comparison of
# of the value  of livestock output types in 2018 in Int. $.
#
# For detail on descriptions of each figure
# See:  output/figures/README.md#figure-descriptions # nolint
#
# For details on the figure specifications used here
# See: output/figures/README.md#figure-specifications # nolint
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



# Constants ---------------------------------------------------------------

# Year that will be used in the output map
date <- 2018


# Animal categories to use,
# with the addition of "Other Livestock"
animals <- c("Cattle", "Sheep", "Goat", "Chicken", "Pig", "Other Livestock")





# Prep data ---------------------------------------------------------------


prep_data <- function(data = date) {

  # Load in the PPP Conversion Values from the config files
  ppp_conversion <- arrow::read_parquet(
    config$data$output$ppp_conversion
  ) %>%
    dplyr::filter(year == date) # nolint

  # Load in the livestock output data
  output_value <- arrow::read_parquet(
    config$data$output$livestock_values,
    as_data_frame = TRUE
  ) |>
  dplyr::mutate(
    category = case_when(
      animal %in% tolower(animals) ~ stringr::str_to_title(animal),
      TRUE ~ "Other Livestock"
    )
  ) %>%
    dplyr::filter(
      year == date,
      item != "stock",
      gross_production_value_current_thousand_slc > 0
    ) |>
    dplyr::mutate(iso3_code = toupper(iso3_code)) %>%
    dplyr::select(
      iso3_code,
      item,
      category,
      gross_production_value_current_thousand_slc
    ) %>%
    dplyr::mutate(
      current_slc_value = gross_production_value_current_thousand_slc * 1e3
    ) %>%
    dplyr::group_by(iso3_code, category) %>%
    dplyr::summarise(
      current_slc_value = sum(current_slc_value, na.rm = TRUE),
      .groups = "drop"
    )  %>%
    tidyr::drop_na(current_slc_value)


  # Load the Aquaculture data
  aquaculture_value <- arrow::read_parquet(
    config$data$output$aquaculture_values
  ) |>
    dplyr::filter(
      year == date,
      lcu_value > 0
    ) |>
    dplyr::group_by(iso3_code) |>
    dplyr::summarise(
      # Technically lcu, but there will be no different
      current_slc_value = sum(lcu_value, na.rm = TRUE)
    ) |>
    dplyr::mutate(category = "Aquaculture")




  # Convert the data to ppp adjusted values
  output_value_ppp <- output_value  %>%
    bind_rows(aquaculture_value) |>
     dplyr::left_join(
      ppp_conversion, by = "iso3_code"
     )  %>%
     dplyr::mutate(
      value_int = current_slc_value/value
     )   %>%
     dplyr::select(
      iso3_code, category,  value_int
     )  %>%
     dplyr::filter(value_int > 0)

}


# Load data ---------------------------------------------------------------

data <- prep_data()


# World data:
# Imports simple features for all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::rename(iso3_code = iso_a3)



world_value <- world |>
  dplyr::left_join(data, by = "iso3_code") |>
  tidyr::drop_na(value_int)



# Bin the data into appropriate bins
cut_labels <- c(
  "NA" = "#808080",
  "&lt;50M" = "#FFFFCC",
  "50M-500M" = "#A1DAB4",
  "500M-10B" = "#41B6C4",
  "&gt;10B" = "#225EA8"
)


world_value <- dplyr::select(
  world_value,
  name,
  iso3_code,
  category, value_int, geometry
) %>%
  dplyr::mutate(
    value_bins = cut(value_int,
      breaks = c(0, 50e6, 500e6, 10e9, Inf),
      labels = names(cut_labels)[2:length(cut_labels)],
      ordered_result = TRUE
    ),
    category = forcats::fct_reorder(category, value_int, sum, .desc = TRUE)
  )


# Figure A2 ---------------------------------------------------------------

fig_a2  <- ggplot(data = world) +
  geom_sf(fill = "#808080", color = "#D5E4EB", size = 0.1) +
  geom_sf(
    data = world_value,
    aes(fill = value_bins), color = "#D5E4EB", size = 0.1
  ) +
  coord_sf(
    ylim = c(-55, 78),
    xlim = c(-180, 180)) +
  scale_fill_manual(
    values = cut_labels
  ) +
  facet_wrap(~category) +
  labs(
    title = paste0(
    "Global spatial distribution of the economic value", 
    " of direct farmed animal outputs (e.g., meat, milk, eggs, fish) in 2018"
  ),
    fill = "Int. $",
    subtitle = "",
  ) +
  guides(fill = guide_legend(nrow = 1, title.position = "left")) +
  world_map_theme() +
  theme(
    legend.position = c(0.6, 0.2),
    plot.margin = margin(50, 25, 25, 50),
    strip.text.x = element_text(size = 15, face = "italic", margin = margin(b = 20)),
    plot.title = ggtext::element_markdown(face = "italic", margin = margin(t = 10, b = 30))
  )




# Save  -------------------------------------------------------------------
ggplot2::ggsave(
  plot = fig_a2,
  filename = "output/figures/figure_A2.png",
  width = 18,
  height = 11,
  dpi = 300
)

ggplot2::ggsave(
  plot = fig_a2,
  filename = "output/figures/figure_A2.tiff",
  width = 18,
  height = 11,
  dpi = 300, 
  device = "tiff"
)
