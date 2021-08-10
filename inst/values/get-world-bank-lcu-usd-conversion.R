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
# Description: This script takes in the World Banks weighted average
# USD to LCU conversion rate (PA.NUS.ATLS) which is stored in
#
#  - data/processed/world_bank/lcu_conversion.parquet
#
# For any gaps which are present for the years 1994-2020 in the
# regions assessed in this project, these are filled using the
# data series of official exchange rates (PA.NUS.FCRF) which is stored in
#
# - data/processd/world_bank/lcu_conversion_official.parquet
#
# This script simply reads these files, subsets it
# for countries/regions which are to be included in the final
# analysis.
#
# The output location of this script will be in
# - data/output/world_bank/lcu_conversion.parquet
#
# These locations are currently available in the
# project config file stored in conf/config.yml
# and the commands to rerun each of these files and reproduce the output
# are given in the project makefile.
####################################


# Activate Project --------------------------------------------------------

renv::activate(project = '.')



# Import Configuration Files ----------------------------------------------

config <- config::get(file = file.path('conf', 'config.yml'))


# Import LCU USD Conversion Data ----------------------------------------------

# Subset for the regions which will be used in the remainder of this
# analysis
config_processed_tables <- config$data$processed$tables
lcu_conversion_file <- config_processed_tables$lcu_conversion
lcu_official_conversion_file <- config_processed_tables$lcu_conversion_official

output_lcu_conversion_file <- config$data$output$lcu_conversion

# Location of country codes
country_codes <- config$data$codes$faostat$country_codes$output_codes |>
  arrow::read_parquet()


logging::loginfo(paste0("Importing and transforming world bank LCU to USD Exchange Rate",
                        "Conversion Data: PA.NUS.ATLS"))
lcu_conversion <-
    arrow::read_parquet(lcu_conversion_file) |>
    dplyr::rename(
        iso3_code = country_code,
        country = country_name
    ) |>
    tidyr::gather(key = "year",
                  value = "value",
                  -iso3_code,
                  -country,
                  -indicator_name,
                  -indicator_code) |>
    tidyr::drop_na(value) |>
    dplyr::filter(value > 0) |>
    assertr::verify(value > 0) |>
    dplyr::filter(
        iso3_code %in% unique(country_codes$iso3_code)
    ) |>
    dplyr::mutate(
        year = as.numeric(substr(year, 2, nchar(year)))
    )

logging::loginfo(paste0("Importing and transforming world bank LCU to USD Exchange Rate",
                        "Conversion Data: PA.NUS.FCRF"))

lcu_official_conversion <-
    arrow::read_parquet(lcu_official_conversion_file) |>
    dplyr::rename(
        iso3_code = country_code,
        country = country_name
    ) |>
    tidyr::gather(key = "year",
                  value = "value",
                  -iso3_code,
                  -country,
                  -indicator_name,
                  -indicator_code) |>
    tidyr::drop_na(value) |>
    dplyr::filter(value > 0) |>
    assertr::verify(value > 0) |>
    dplyr::filter(
        iso3_code %in% unique(country_codes$iso3_code)
    ) |>
    dplyr::mutate(
        year = as.numeric(substr(year, 2, nchar(year)))
    )



# Input the Official Exchange Rate where the Area Weighted exchange is missing-

# Cheap trick to get the NAs per group
lcu_conversion <-
    lcu_conversion |>
    tidyr::pivot_wider(
        id_cols = c("iso3_code", "country", "indicator_name", "indicator_code"),
        names_from = year,
        values_from = value
    ) |>
    tidyr::gather(key = "year", value = "value",
                  -iso3_code, -country, -indicator_name, -indicator_code)

lcu_official_conversion <- lcu_official_conversion  |>
    tidyr::pivot_wider(
        id_cols = c("iso3_code", "country", "indicator_name", "indicator_code"),
        names_from = year,
        values_from = value
    ) |>
    tidyr::gather(key = "year", value = "value",
                  -iso3_code, -country, -indicator_name, -indicator_code)

lcu_conversion <- dplyr::full_join(
    lcu_conversion,
    lcu_official_conversion,
    by = c("iso3_code", "country", "year")
) |>
    dplyr::mutate(
        value = ifelse(is.na(value.x), value.y, value.x),
        indicator_name = ifelse(is.na(value.x), indicator_name.y, indicator_name.x),
        indicator_code = ifelse(is.na(value.x), indicator_code.y, indicator_code.x),
        year = as.numeric(year)
    ) |>
    dplyr::filter( year > 1992) |>
    tidyr::drop_na(value) |>
    dplyr::select(iso3_code,
                  country, indicator_name, indicator_code,year, value)



# Fix missing values ------------------------------------------------------
# American Samoa ASM uses USD -> Set the rate to 1
# Guadelope GLP uses Euros -> Set rate to France's rate
# GUF       French Guiana  -> Set to EURO after 2002
# Reunion REU -> uses EURO  since 1999
#




# Save to output file -----------------------------------------------------
lcu_conversion |>
  arrow::write_parquet(
    sink = here::here(output_lcu_conversion_file)
  )


# Exit the Script ---------------------------------------------------------
logging::loginfo("Exit")

