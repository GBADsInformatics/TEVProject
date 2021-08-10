#' scale_trill
#'
#' Scales axis to trillions of dollars
#'
#' @param x the numeric vector to be scaled
#' @param accuracy  what accuracy to display the results at
#'
#' @return
#' @export
#'
#' @examples
scale_trill <- function(x, accuracy = 0.01) {
  scales::dollar(x,
    accuracy = accuracy,
    scale = 1e-12,
    suffix = " T"
  )
}




#' nature_color_scheme
#' Obtained from \code{ggsci::pal_npg()(7)}
#' Visualise through \code{scales::show_col(ggsci::pal_npg()(7))}
#'
#' @return nature colorscheme
#' @export
#'
nature_color_scheme <- function() {
  c(
    "Crops" = "#91D1C2FF",
    "Cattle" = "#E64B35FF",
    "Chicken" = "#F39B7FFF",
    "Pig" = "#4DBBD5FF",
    "Aquaculture" = "#3C5488FF",
    "Sheep" = "#00A087FF",
    "Other Livestock" = "#8491B4FF"
  )
}


#' panel_plot_theme_nature_food
#'
#' @return
#' @export
#'
#' @examples
panel_plot_theme <- function() {
  ggthemes::theme_clean() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        family = "Helvetica",
        size = 10
      ),
      legend.text = ggplot2::element_text(
        family = "Helvetica",
        size = 10
      ),
      plot.title = ggplot2::element_text(
        family = "Helvetica",
        face = "bold.italic",
        size = 15
      ),
      plot.subtitle = ggplot2::element_text(
        face = "italic",
        family = "Helvetica",
        size = 12
      ),
      axis.text.y = ggplot2::element_text(
        family = "Helvetica",
        size = 10
      ),
      axis.title = ggplot2::element_text(
        family = "Helvetica",
        face = "italic",
        size = 10
      ),
      axis.text.x = ggplot2::element_text(
        family = "Helvetica",
        size = 10
      ),
      axis.title.x = ggplot2::element_text(
        face = "italic",
        family = "Helvetica",
        size = 10
      ),
      panel.border = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x.bottom = ggplot2::element_blank(),
      plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
    )
}

#' world_map_theme
#'
#' @return
#' @export
#'
#' @examples
world_map_theme <- function() {
  figure_spec <- config::get()$figure_specification$nature_food
  ggthemes::theme_economist() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.text = ggtext::element_markdown(
        size = 12
      ),
      legend.title = ggtext::element_markdown(
        size = 12
      ),
      panel.grid.major = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      plot.subtitle = ggtext::element_markdown(
        size = figure_spec$font_size,
        family = figure_spec$family
      ),
      plot.title.position = "plot",
      plot.title = ggtext::element_markdown(
        size = 18,
        family = "sans",
        face = "italic"
      ),
      plot.caption = ggtext::element_markdown(
        size = figure_spec$font_size,
        family = figure_spec$family, hjust = 0
      ),
      plot.margin = margin(75, 0, 0, 75)
    )
}



# Function to Save Plots  -------------------------------------------------

#' save_panel_plot
#'
#' @param p
#' @param filename
#' @param figure_spec
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_panel_plot <- function(p, filename, ...) {
  if (!(tools::file_ext(filename) %in% c(".png", ".pdf"))) {
    stop("Incorrect file extension!!")
  }

  ggplot2::ggsave(
    plot = p,
    filename = filename,
    ...
  )
}


# Functions to prepare the data  ------------------------------------------

#' get_livestock_asset_output_value
#'
#' @param livestock_value
#' @param value_col
#' @param years
#' @param in_cat
#' @param out_cat
#'
#' @return
#' @export
#'
#' @examples
get_livestock_asset_output_value <- function(livestock_value,
                                             value_col = stock_value_constant_2014_2016_usd,
                                             years = c(1998, 2003, 2008, 2013, 2018),
                                             in_cat = c("cattle", "chicken", "pig", "sheep"),
                                             out_cat = "Other Livestock") {
  livestock_value |>
    dplyr::rename(value = {{ value_col }}) |>
    dplyr::filter(
      (head > 0 | tonnes > 0),
      value > 0,
      year %in% years
    ) |>
    dplyr::mutate(
      category = case_when(
        !(animal %in% in_cat) ~ out_cat,
        TRUE ~ tools::toTitleCase(animal)
      )
    ) |>
    dplyr::group_by(year, category) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      tonnes = sum(tonnes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      percentage = value / sum(value, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      category = forcats::fct_reorder(category, value, mean, .desc = FALSE)
    ) |>
    assertr::verify(
      assertr::not_na(value)
    ) |>
    assertr::verify(
      value < 2e12 & value >= 1e9 # Less than 2 trillion
    ) |>
    assertr::verify(
      assertr::has_all_names(
        "year", "category", "value", "tonnes", "percentage"
      )
    )
}

#' get_livestock_output_value
#'
#' @inheritParams get_livestock_asset_value
#' @param out_cat
#' @return
#' @export
#'
#' @examples
get_livestock_output_value <- function(livestock_value,
                                       value_col = gross_production_value_constant_2014_2016_usd,
                                       years = c(1998, 2003, 2008, 2013, 2018),
                                       in_cat = c("cattle", "chicken", "pig", "sheep"),
                                       out_cat = "Other Livestock") {
  livestock_value |>
    dplyr::rename(value = {{ value_col }}) |>
    dplyr::filter(value > 0, year %in% years) |>
    mutate(
      category = case_when(
        !(animal %in% in_cat) ~ out_cat,
        TRUE ~ tools::toTitleCase(animal)
      )
    ) |>
    dplyr::group_by(year, category) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      tonnes = sum(tonnes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      percentage = value / sum(value, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    assertr::verify(
      assertr::not_na(value)
    ) |>
    assertr::verify(
      value > 0 & value < 2e12
    ) |>
    assertr::verify(
      assertr::has_all_names(
        "year", "category", "value", "tonnes"
      )
    )
}

#' get_aquaculture_value
#'
#' @param aquaculture_value
#' @param value_col
#' @param years
#'
#' @return
#' @export
#'
#' @examples
get_aquaculture_value <- function(aquaculture_value,
                                  value_col = constant_2014_2016_usd_value,
                                  years = c(1998, 2003, 2008, 2013, 2018)) {
  aquaculture_value |>
    dplyr::rename(value = {{ value_col }}) |>
    dplyr::filter(value > 0, year %in% years) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      tonnes = sum(tonnes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      category = "Aquaculture"
    ) |>
    assertr::verify(
      assertr::not_na(value) & assertr::not_na(tonnes)
    ) |>
    assertr::verify(
      value > 0 & value < 5e11
    ) |>
    assertr::verify(
      tonnes > 0
    ) |>
    assertr::verify(
      assertr::has_all_names(
        "year", "category", "value", "tonnes"
      )
    )
}


#' get_crop_value
#'
#' Returns the crop values for the specified years
#'
#' @param crop_value a dataframe containing the crop values
#' @param value_col
#' @param years
#'
#' @return
#' @export
#'
#' @examples
get_crop_value <- function(crop_value,
                           value_col = gross_production_value_constant_2014_2016_usd,
                           years = c(1998, 2003, 2008, 2013, 2018)) {
  crop_value |>
    dplyr::rename(value = {{ value_col }}) |>
    dplyr::filter(value > 0, year %in% years) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      tonnes = sum(tonnes, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      category = "Crops"
    ) |>
    assertr::verify(
      assertr::has_all_names(
        "year", "category", "value", "tonnes"
      )
    )
}
