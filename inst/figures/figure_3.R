##!/usr/bin/Rscript --vanilla

## ------------------------------------------------------------------------------
##
## Name: "inst/figures/generate-figures.R" # nolint
## Project: GBADS
## Author: Gabriel Dennis <gabriel.dennis@csiro.au>
##
## Generates  Figure 2 for the livestock value manuscript
##
## For detail on descriptions of each figure
## See:  output/figures/README.md#figure-descriptions # nolint
##
## For details on the figure specifications used here
## See: output/figures/README.md#figure-specifications # nolint
## -------------------------------------------------------------------------


### Project Library ---------------------------------------------------------
renv::activate(project = ".")

## Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
    library(dplyr)
    library(magrittr)
    library(ggplot2)
    library(ggthemes)
    library(hrbrthemes)
    library(tidyr)
    library(LivestockValueGBADS)
})
## -------------------------------------------------------------------------



## Import output files -----------------------------------------------------
config <- config::get()


## Have to remove this
theme_set(
    panel_plot_theme()
)


## Preparation functions ---------------------------------------------------
config <- config::get()

## Function to prepare data for initial plots
prep_data <- function() {
    df_list <- purrr::keep(
        config$data$output,
        ~ grepl("values", .x, ignore.case = TRUE)
    )
    data <- list()
    livestock_value <- df_list$livestock_values |>
        arrow::read_parquet() |>
        dplyr::mutate(
            gross_production_value_constant_2014_2016_usd = gross_production_value_constant_2014_2016_thousand_us * 1e3 # nolint
        )

    livestock_value <- df_list$livestock_values |>
        arrow::read_parquet() |>
        dplyr::mutate(
            gross_production_value_constant_2014_2016_usd = gross_production_value_constant_2014_2016_thousand_us * 1e3 # nolint
        )

    data$livestock_asset <- get_livestock_asset_output_value(livestock_value)

    data$livestock_output <- get_livestock_asset_output_value(
        livestock_value,
        gross_production_value_constant_2014_2016_usd # nolint
    )

    data$aquaculture_value <- df_list$aquaculture_values |>
        arrow::read_parquet() |>
        get_aquaculture_value()


    data$crop_value <- df_list$crop_values |>
        arrow::read_parquet() |>
        dplyr::mutate(
            gross_production_value_constant_2014_2016_usd = gross_production_value_constant_2014_2016_thousand_us * 1e3 # nolint
        ) |>
        get_crop_value()


    data
}

## -------------------------------------------------------------------------

## Get data for initial plots
data <- prep_data()



## Helper Functions  -------------------------------------------------------

## Function to get the total value
get_total_df <- function(df, value_col) {
    df |>
        dplyr::group_by(year) |> # nolint
        dplyr::summarise(
            value = sum({{ value_col }}, na.rm = TRUE),
            .groups = "drop"
        )
}



## Data Prep ---------------------------------------------------------------

df <- bind_rows(
    data$livestock_asset,
    data$livestock_output,
    data$aquaculture_value 
) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
        value = sum(value, na.rm = TRUE)
    ) |>
    dplyr::mutate(
        type = "direct+market"
    )

df <- df |>
    dplyr::bind_rows(
        data$livestock_asset |>
            dplyr::group_by(year) |>
            dplyr::summarise(
                value = sum(value, na.rm = TRUE)
            ) |>
            dplyr::mutate(type = "market")
    )



# Livestock and Aquaculture
df_outputs <- get_total_df(
    bind_rows(data$livestock_output, data$aquaculture_value),
    value)

df_crops <- data$crop_value


## Generate the arrow location plot
arrow_locations <- df |>
    group_by(year) |>
    mutate(
        x = year,
        xend = year,
        y = min(value) + 1.5e11,
        yend = max(value) - 1.5e11
    ) |>
    ungroup() |>
    filter(year != 2008)



##

## Text location
text_locations <- df |>
  left_join(df_outputs, by = "year", suffix = c("", "_out"))  %>%  
    mutate(
        x = year,
        y = ifelse(type == "market" & value > value_out, value + 0.8e11, value - 0.8e11),
        label = scales::dollar(
            value,
            accuracy = 0.01,
            scale = 1e-12,
            suffix = "T",
            prefix = "$"
        )
    )   %>% 
    select(-value_out)


## Colors for both
nat_pal <- setNames(c("black", "black"), c("direct+market", "market"))
csiro_blue <- "#0989B2"
annotate_size <- 4
years <- seq(1998, 2018, by =  5)

## Geom ribbon
df_ribbon <- df |>
    spread(type, value)


### Figure 3 ----------------------------------------------------------------
fig3 <- df |>
    ggplot(aes(x = year, y = value, color = type)) +
geom_ribbon(
    data = df_ribbon,
    aes(x = year, ymin = `market`, ymax = `direct+market`), inherit.aes = FALSE,
    alpha = 0.1,
    color = csiro_blue
) +
geom_point(size = 2) +
geom_line(size = 1.5) +
scale_color_manual(values = nat_pal) +
scale_y_continuous(
    labels = scales::dollar_format(
        scale = 1e-12,
        suffix = "T",
        accuracy = 1,
    ),
    limits = c(0, 3.5e12)
) +
scale_x_continuous(breaks = years) +
geom_text(
    data = text_locations,
    aes(x = x, y = y, label = label),
    size = annotate_size,
    fontface = "bold.italic",
    inherit.aes = FALSE
) +
geom_point(
    data = df_outputs,
    aes(x = year, y = value),
    inherit.aes = FALSE,
    size = 2,
    color = nature_color_scheme()["Cattle"]
) +
geom_line(
    data = df_outputs,
    aes(x = year, y = value),
    inherit.aes = FALSE,
    linetype = "dashed",
    size = 1.5,
    color = nature_color_scheme()["Cattle"]
) +
geom_point(
    data = df_crops,
    size = 2,
    aes(x = year, y = value),
    inherit.aes = FALSE,
    color = nature_color_scheme()["Sheep"]
) +
geom_line(
    data = df_crops,
    size = 1.5,
    aes(x = year, y = value),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = nature_color_scheme()["Sheep"]
) +
annotate(
    "text",
    x = 2008,
    y = df_crops$value[df_crops$year == 2008] + 0.7e11,
    label = "atop(bold('Crop Output Value'))",
    parse = TRUE,
    size = annotate_size,
    color = nature_color_scheme()["Sheep"]
) +
annotate(
    "text",
    x = 2008,
    y = df_outputs$value[df_outputs$year == 2008] - 1.8e11,
    label = "atop(bold('Animal Output Value'))",
    parse = TRUE,
    size = annotate_size,
    color = nature_color_scheme()["Cattle"]
) +
annotate("text",
    x = 2008,
    y = 1.55e12,
    label = "atop(bold('Animal Asset Value'))",
    parse = TRUE,
    size = annotate_size
) +
annotate("text",
    x = 2008,
    y = 2.85e12,
    label = "atop(bold('Animal Asset Value +\nAnimal Output Value'))",
    size = annotate_size,
    parse = TRUE
) +
labs(
    x = "Year",
    y = "Market Value in constant USD (Trillion)",
    fill = " ",
    title = ""
) +
panel_plot_theme() +
theme(
    legend.position = "none",
    plot.margin = margin(50, 50, 50, 50),
    plot.title = element_text(face = "italic"),
    axis.title.x.bottom = element_text(
        size = 14,
        face = "italic",
        vjust = -1.5
    ),
  axis.title.y = element_text(
        size = 14,
        face = "italic",
        vjust = 3
    ),
     axis.text.y = ggplot2::element_text(
         family = "Helvetica",
         size = 13
     ),
     axis.text.x = ggplot2::element_text(
         family = "Helvetica",
         size =  13
     )
)

if (interactive()) {
    fig3
}

## Save Output -------------------------------------------------------------
ggsave(
    plot = fig3,
    filename = "output/figures/figure_3.png",
    width = 16,
    height = 10,
    dpi = 300,
    device = "png"
)

## Tiff file
ggsave(
    plot = fig3,
    filename = "output/figures/figure_3.tiff",
    width = 16,
    height = 10,
    dpi = 300,
    device = "tiff"
)
