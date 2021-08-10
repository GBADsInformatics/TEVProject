#!/usr/bin/env Rscript --vanilla

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
# Date Created:  20220201
#
# Description:  Generates Crop values
# parquet file for analysis.
#
#
#
#
####################################

renv::activate(project = ".")


# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(janitor)
  library(stringr)
  library(logging)
  library(LivestockValueGBADS)
})


# Logging configuration
basicConfig()


# Config ------------------------------------------------------------------

# Read in the project configuration file
config <- config::get()



# Load Data ---------------------------------------------------------------
get_data <- function(files, use_years = 1994:2018) {
  purrr::map(
    files,
    ~ arrow::read_parquet(.x) |>
      janitor::clean_names() |>
      dplyr::filter(year %in% use_years) |>
      clean_countries() |>
      sanitize_columns()
  )
}




# Data Import -------------------------------------------------------------

# Crop codes which are used
loginfo("Importing Crop Codes")

# Import the Crop codes which will be used
crop_codes <- config$data$codes$faostat$crops |>
  readRDS()


output_results_dir <- here::here(
  "output", "tables",
  paste0(
    "crop_values_",
    format(Sys.Date(), format = "%Y%m%d")
  )
)

dir.create(output_results_dir, recursive = TRUE)

readr::write_csv(crop_codes |>
  dplyr::select(item, item_code, group),
file = here::here(output_results_dir, "faostat_crop_groupings.csv")
)

logging::loginfo("Zipping results directory %s", output_results_dir)
zip(
  zipfile = paste0(output_results_dir, ".zip"),
  files = list.files(output_results_dir, full.names = TRUE),
  flags = "-j"
)

# Remove the temporary output directory
unlink(output_results_dir, recursive = TRUE)


logging::loginfo(
  paste0(
    "Generating summary files and tables",
    " - outputs will be zipped into %s"
  ),
  output_results_dir
)



# Read in processed source data -------------------------------------------

# Input files
processed_files <- config$data$processed$tables

# Import files for crops
loginfo("Importing FAOSTAT data files")

crops <- get_data(processed_files[grep("faostat", processed_files)])


# Subset for crop crop codes
loginfo("Subsetting for required crop data")

crops <- purrr::map(
  crops,
  ~ filter(.x, item_code %in% unique(crop_codes$item_code)) %>%
    select(-year_code) %>%
    left_join(crop_codes) %>%
    relocate(group, .after = item)
)

# Filter  for just production element

crops$production <- filter(
  crops$crops_and_livestock_products,
  element == "production"
)

crops$prices <- filter(crops$producer_prices, months_code == 7021)


# Spread each table
crops$value_of_production <- crops$value_of_production %>%
  select(-element_code, -unit, -flag) %>%
  spread(element, value)

crops$production <- crops$production %>%
  select(-element_code, -flag, -element, -unit) %>%
  rename(tonnes = value)

crops$prices <- crops$prices %>%
  select(-element_code, -months_code, -months, -unit, -flag) %>%
  spread(element, value)

crop_df <- purrr::reduce(
  crops[c("production", "prices", "value_of_production")],
  left_join,
  by = c("iso3_code", "year", "item_code", "item", "group")
) |>
  janitor::clean_names() |>
  dplyr::filter(tonnes > 0) # Filter for years with records





# Metadata ----------------------------------------------------------------

# Convert to a Arrow table
crop_table <- arrow::Table$create(crop_df)


crop_table$metadata <- list(
  iso3_code = "ISO 3166-1 alpha-3",
  faost_code = "FAOSTAT Area Code",
  area = "FAOSTAT Area Name",
  year = "Year in YYYY format",
  tonnes = "Metric Tonnes",
  producer_price_index_2014_2016_100 = "An FAOSTAT Items producer price index, for a certain item calculated to average 100 between 2014 and 2016",
  producer_price_lcu_tonne = "Producer price of item in local currency per tonne",
  producer_price_slc_tonne = "Producer price of item in national currency per tonne",
  producer_price_usd_tonne = "Producer price of item in current USD per tonne",
  gross_production_value_constant_2014_2016_thousand_i = "Gross production value of item in constant thousand 2014 2016 international dollars, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_constant_2014_2016_thousand_slc = "Gross production value of item in constant 2014 2016 in thousand standard local currency units, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_constant_2014_2016_thousand_us = "Gross production value of item in constant thousand 2014 2016 US dollars,  for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_current_thousand_slc = "Gross production value of item in current thousand   standard local currency units, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_current_thousand_us = "Gross production value of item in current thousand US dollars",
  date = iso_date(),
  contributor = "Gabriel Dennis CSIRO, gabriel.dennis@csiro.au",
  format = "Arrow Table",
  language = "English",
  source = paste("[FAO.] Crops and livestock products.[Accessed 2022-01-28.] https://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_Livestock_E_All_Data_(Normalized).zip",
    "[FAO.] Value of Agricultural Production.[Accessed 2022-01-28.]https://fenixservices.fao.org/faostat/static/bulkdownloads/Value_of_Production_E_All_Data_(Normalized).zip",
    "[FAO.] Prices: Producer Prices.[Accessed 2022-01-28.] http://fenixservices.fao.org/faostat/static/bulkdownloads/Prices_E_All_Data_(Normalized).zip",
    sep = " -  - "
  ),
  frequency = "Yearly"
)


## 5 - Write to file ------------------------------#
if (!dir.exists(dirname(config$data$output$crop_values))) {
  dir.create(dirname(config$data$output$crop_values))
}

loginfo("Writing to parquet file")
arrow::write_parquet(
  crop_table,
  config$data$output$crop_values
)
