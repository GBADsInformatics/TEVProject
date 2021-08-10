# Info ----------------------------------------------
#
# Name: world_bank_population.R
# Project: GBADS
# Author: Gabriel Dennis
# Email: Gabriel.Dennis@csiro.au
#
# Date Created: 20221031
#
# Description: Creates a package dataset
# for the World Bank Population Data
#
# Note: to instructions to download this dataset are included in the
# makefile
#
# TODO: Migrate those download instructions to here
#  ---------------------------------------------------


# Load Libraries ----------------------------------------------------------

suppressPackageStartupMessages({
  library(assertr)
})

# Load the Location of the dataset ---------------------------------------

# Projection configuration
config <- config::get()

# World Bank Population Configuration
world_bank_population_config <- config$data$source$tables$population

# World Bank Population File
world_bank_population_file <- config$data$processed$tables$population


# Read in Metadata Files --------------------------------------------------

metadata_filenames <- list.files(world_bank_population_config$metadata_files,
  pattern = "^metadata",
  ignore.case = TRUE,
  full.names = TRUE
)

metadata <- purrr::map(
  metadata_filenames,
  ~ readr::read_csv(.x) |>
    janitor::clean_names()
) |>
  dplyr::bind_cols()

# Read in and manipulate the data -----------------------------------------
world_bank_population <- arrow::read_parquet(world_bank_population_file) |>
  dplyr::select(country_code, dplyr::contains("x")) |>
  tidyr::gather(key = "year", value = "population", -country_code) |>
  tidyr::drop_na(population) |>
  dplyr::mutate(
    year = as.numeric(gsub("^x", "", year)),
    url = world_bank_population_config$url,
    source_dir = world_bank_population_config$dir,
    source_type = world_bank_population_config$type,
    last_download = world_bank_population_config$last_download,
  ) |>
  dplyr::left_join(metadata, by = c("country_code")) |>
  dplyr::select(-dplyr::contains("x")) |>
  assertr::verify(
    has_all_names(
      "country_code", "year", "population", "url", "source_dir",
      "source_type", "last_download", "region", "income_group", "special_notes",
      "table_name", "indicator_code", "indicator_name", "source_note",
      "source_organization"
    )
  )




# Write to data file  -----------------------------------------------------
usethis::use_data(world_bank_population, overwrite = TRUE)
