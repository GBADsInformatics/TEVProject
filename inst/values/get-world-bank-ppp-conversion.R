#!/usr/bin/Rscript --vanilla

###################################################
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
# Date Created:  20220408
#
# Description:  This file takes in the purchasing power parity (PPP)
# data from the world bank (PA.NUS.PPP) which has already been
# downloaded and stored in
#  - data/processed/world_bank/ppp_conversion.parquet
#
# This script simply reads this file, subsets it
# for countries/regions which are to be included in the final
# analysis.
#
# The output location of this script will be in
# - data/output/world_bank/ppp_conversion.parquet
#
# These locations are currently available in the
# project config file stored in conf/config.yml
#
####################################


# Activate Project --------------------------------------------------------

renv::activate(project = ".")



# Import Configuration Files ----------------------------------------------

config <- config::get( )


# Import PPP Conversion Data ----------------------------------------------

# Subset for the regions which will be used in the remainder of this
# analysis
ppp_conversion_file <- config$data$processed$tables$ppp_conversion
output_ppp_conversion_file <- config$data$output$ppp_conversion

# Location of country codes
country_codes <- config$data$codes$faostat$country_codes$output_codes |>
  arrow::read_parquet()


logging::loginfo("Importing and transforming world bank PPP Conversion Data")

arrow::read_parquet(ppp_conversion_file) |>
  dplyr::rename(
    iso3_code = country_code,
    country = country_name
  ) |>
  tidyr::gather(
    key = "year",
    value = "value",
    -iso3_code,
    -country,
    -indicator_name,
    -indicator_code
  ) |>
  tidyr::drop_na(value) |>
  dplyr::filter(
    iso3_code %in% unique(country_codes$iso3_code)
  ) |>
  dplyr::mutate(
    year = as.numeric(substr(year, 2, nchar(year)))
  ) |>
  arrow::write_parquet(
    sink = here::here(output_ppp_conversion_file)
  )

logging::loginfo("Exit")
