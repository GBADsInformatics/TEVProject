#!/usr/bin/env Rscript --vanilla

#-------------------------------------------------------------------------------
#
# project: gbads
# name: inst/values/get-info-data.R
#
# author: gabriel dennis
# email: gabriel.dennis@csiro.com
# id: gdenn173
#
#
# description:
# This script takes the output tev values files and aggregates them to
# a simpler format for the informatics dashboard.
#
# Note: Dependent on further revisions of the current manuscript,
# this output data/format may change.
#
# inputs:
# - parquet files, defined in the config file located at conf/config.yml
#   under the key config.data.output
#
# outputs:
#
# - tidy parquet file located at
#   data/output/informatics/20220603_informatics_tev-data_1998-2018.parquet
#   this is recorded in the config file at config.data.output.informatics
#-------------------------------------------------------------------------------



# Activate Project --------------------------------------------------------

renv::activate(".")



# Config ------------------------------------------------------------------

config <- config::get()



# Libraries ---------------------------------------------------------------

library(dplyr)


# Read in data ------------------------------------------------------------

# Read in
# Livestock
# Cropping
# Aquaculture Output files
output_value_files <- grep(
  "values.parquet$",
  unlist(config$data$output),
  value = TRUE,
  ignore.case = TRUE
)


# Import output files into dfs
# Select for the appropriate years

data <- purrr::map(
  output_value_files,
  ~ arrow::read_parquet(.x)
)



# Summarise and aggregate -------------------------------------------------

# Livestock Assets
data$livestock_asset <- data$livestock_values |>
  dplyr::filter(
    (head > 0 | tonnes > 0),
    stock_value_constant_2014_2016_usd > 0
  ) |>
  dplyr::mutate(value = stock_value_constant_2014_2016_usd) |>
  dplyr::mutate(category = case_when(
    !(animal %in% c(
      "cattle",
      "sheep",
      "chicken",
      "pig"
    )) ~ "Other Livestock",
    TRUE ~ tools::toTitleCase(animal)
  )) |>
  dplyr::group_by(year, iso3_code, category) |>
  dplyr::summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    type = "Asset",
    unit = "2014-2016 Constant USD ($)"
  )


# Livestock Outputs
data$livestock_output <- data$livestock_values |>
  dplyr::filter(gross_production_value_constant_2014_2016_thousand_us > 0) |>
  dplyr::mutate(value = gross_production_value_constant_2014_2016_thousand_us * 1000) |>
  dplyr::mutate(category = case_when(
    !(animal %in% c(
      "cattle", "sheep",
      "chicken", "pig"
    )) ~ "Other Livestock",
    TRUE ~ tools::toTitleCase(animal)
  )) |>
  dplyr::group_by(year, iso3_code, category) |>
  dplyr::summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    type = "Output",
    unit = "2014-2016 Constant USD ($)"
  )


# Aquaculture Outputs
data$aquaculture_outputs <- data$aquaculture_values |>
  dplyr::filter(constant_2014_2016_usd_value > 0) |>
  dplyr::mutate(value = constant_2014_2016_usd_value) |>
  dplyr::group_by(year, iso3_code) |>
  dplyr::summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(category = "Aquaculture") |>
  dplyr::mutate(
    type = "Output",
    unit = "2014-2016 Constant USD ($)"
  )


# Crop Outputs
data$crop_outputs <- data$crop_values |>
  dplyr::filter(gross_production_value_constant_2014_2016_thousand_us > 0) |>
  dplyr::mutate(value = gross_production_value_constant_2014_2016_thousand_us * 1e3) |>
  dplyr::group_by(year, iso3_code) |>
  dplyr::summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(category = "Crops") |>
  dplyr::mutate(
    type = "Crops",
    unit = "2014-2016 Constant USD ($)"
  )



# Write to file -----------------------------------------------------------


# Bind rows to form output data
output_df <- dplyr::bind_rows(
  data$livestock_asset,
  data$livestock_output,
  data$aquaculture_output,
  data$crop_outputs
)

# Quick check:
aggregate(value ~ year + type, data = output_df, FUN = summary)

# Write to file

output_df |>
  arrow::write_parquet(config$data$output$informatics)
