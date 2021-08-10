################################################################
#
# Project: GBADS: Global Burden of Animal Disease
#
# Author: Gabriel Dennis
#
# Position: Research Technician, CSIRO
#
# Email: gabriel.dennis@csiro.au
#
# CSIRO ID: den173
#
# GitHub ID: denn173
#
# Date Created:  20220214
#
# Description:  This file contains a series of functions which generate
# different  types of output tables.
#
####################################

require(kableExtra)
require(magrittr)
require(dplyr)



# KableExtra Tables -------------------------------------------------------
#' generate_kbl
#'
#' Generates a kable using the kable extra package and saves the output
#' pdf
#'
#' @param df the tables data
#' @param col_names  column names
#' @param caption table header
#' @param header_spec_fun a function which will be applied to the column header
#' @param footnotes a character vector of footnotes to be added to the table
#' @param output_name the output location to save the kable
#'
generate_kbl <- function(df, col_names, caption, header_spec_fun,
                         footnotes, output_name = NULL) {
  table <- df %>%
    kableExtra::kbl(
      col.names = col_names,
      booktabs = TRUE,
      align = c("l", rep("c", length(col_names) - 1)),
      caption = caption,
      midrule = TRUE,
      bottomrule = TRUE
    ) %>%
    kableExtra::kable_classic(
      font_size = 13, bootstrap_options = "striped",
      full_width = F
    ) %>%
    kableExtra::column_spec(1:ncol(df), border_left = TRUE, border_right = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    kableExtra::row_spec(0, italic = TRUE, align = "c") %>%
    kableExtra::row_spec(nrow(df), extra_css = "border-bottom: 1px solid;") %>%
    header_spec_fun() %>%
    kableExtra::add_footnote(footnotes,
      notation = "number"
    )
  if (!is.null(output_name)) {
    kableExtra::save_kable(table, output_name)
  } else {
    return(table)
  }
}
