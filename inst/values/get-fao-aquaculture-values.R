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
# Description:  This script calculates output values in PPP converted
# dollars from the FAO global Aquaculture production database.
#
# The Global Aquaculture production database data is located at
# - data/source/faostat/global_aquaculture_production
#
# Note (From Metadata): This dataset covers annual series of aquaculture production from 1950
# for the quantities and from 1984 for the values.
# Aquaculture is the farming of aquatic organisms including fish, molluscs,
# crustaceans and aquatic plants.
# Farming implies some form of intervention in the rearing process to enhance
# production, such as regular stocking, feeding, protection from predators, etc.
# Farming also implies individual or corporate ownership of the stock being
# cultivated. For statistical purposes, aquatic organisms which are harvested
# by an individual or corporate body which has owned them throughout their
# rearing period contribute to aquaculture while aquatic organisms which are
# exploitable by the public as a common property resource, with or without
# appropriate licenses, are the harvest of fisheries.
# Production of fish, crustaceans and molluscs is expressed in live weight,
# that is the nominal weight of the aquatic organisms at the time of capture.
# The harvest of aquatic plants is given in wet weight.
# Quantities are given in tonnes (=1000 kg).
# The value of aquaculture, converted from local currencies,
# is reported in thousands US dollars using appropriate exchange rates.
#
# Estimated output values in 1000 USD($) are located in the
#
# - AQUACULTURe_VALUE.csv file
#
# Estimated output values for tonnes of liveweight are located in
#
# - AQUACULTURE_QUANTITY.csv
#
#
# The main unit that is used to subset the data is the custom grouping
#  - "Fish, crustaceans and molluscs, etc."
# which is what is used in the FAOSTAT yearbook.
#  This subsets the species
# used to the main commercial products and excluded production figures for
#  aquatic plants, pearls and mother-of-pearl.
# This subsetting is outlined in the Global Aquaculture Production database
# metadata at `Aqua_E.html`, other notes on the data are also given in
# `NOTES_AQUACULTURE_COUNTRY_EN.html`.
#
# Note: The annual period used is the calendar year (1 January - 31 December),
# with the exceptions of data for some countries for which a split-year is used.
# Split-year data are shown under the calendar year in which the split-year ends.
#
# Note: due to how the Global Aquaculture Production dataset is reported
# there may be a systematic undervaluing of some products due to the species
# groupings. As some countries report using wide and varied categories.
#
#
# As the data is currently in current USD ($) values,
# it is converted to PPP adjusted values by first converting back to LCU ($)
# using a combination of the World Bank indicators
#
# - PA.NUS.ATLS
# - PA.NUS.FCRF
#
# Which are both located at
#
# - data/output/fao/fao_aquaculture_values.parquet
#
# The location of all the input and output files should be specified in
# - conf/config.yml
# and the process for recreating each output file is given in the Makefile.
#
# - $ make data/output/fao/fao_aquaculture_values.parquet
#
# Several auxilary files are also produced when this script is run.
# These are stored in output/tables/*
#
#
# Note: If we go back to using constant dollars,
# use the quantity data that we dropped last time.
####################################

renv::activate(project = ".")

## 0 - Libraries  ------------------------------#
suppressPackageStartupMessages({
  library(here)
  library(tidyr)
  library(readr)
  library(dplyr)
  library(magrittr)
  library(janitor)
  library(logging)
  library(assertr)
  library(LivestockValueGBADS)
})


# Setup logging
basicConfig()



# Config ------------------------------------------------------------------
loginfo("Parsing configurations")
config <- config::get()


## 1 - Parameters ------------------------------#
data_dir <- list.dirs(config$data$source$tables$global_aquaculture_production$dir)[2]

# Locations of each file inside the global aquaculture production directory
params <- list(
  data_dir = data_dir,
  value_file = "AQUACULTURE_VALUE.csv",
  quantity_file = "AQUACULTURE_QUANTITY.csv",
  cpc_group_en_file = "CL_FI_SPECIES_GROUPS.csv",
  iso3_codes_files = "CL_FI_COUNTRY_GROUPS.csv",
  use_years = 1994:2019
)


loginfo("File Directory: %s", data_dir)


# Read in files and Convert  ------------------------------#

aqua <- list()

# Read in Codes and Value Files
#
# ISO3 codes for country mappings
# these are the FAOSTAT IS0 3 codes which
# are included in the data original download
loginfo("Importing ISO3 Codes")

aqua$iso3codes <- readr::read_csv(
  file.path(params$data_dir, params$iso3_codes_files),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  dplyr::select(un_code, iso3_code, name_en) |>
  tidyr::drop_na(iso3_code) |>
  assertr::verify(toupper(iso3_code) == iso3_code)


# Import Project ISO3 codes -----------------------------------------------

# Use the set of ISO3 codes that will be used across this project
#
# These are derived from the FAOSTAT list - large areas which will not be
# included
logging::loginfo("Importing project ISO3 codes")

project_iso3_codes <-
  config$data$codes$faostat$country_codes$output_codes |>
  arrow::read_parquet()

# Subset for the project ISO3 codes
aqua$iso3codes <- aqua$iso3codes |>
  dplyr::filter(iso3_code %in% project_iso3_codes$iso3_code)



# Import Aquaculture Values (1000USD) -------------------------------------
loginfo(
  "Importing Aquaculture Values from %s/%s",
  params$data_dir, params$value_file
)

#
# Aquaculture production values are reported in current 1000 USD for each year
#
# - Subset for years which report positive values
# - Subset for years which are used in this analysis
#
# Note: of the available data, 32382 observations are official estimates
# with 12017 being FAO estimates.
#
aqua$fisheries_values <-
  readr::read_csv(
    file.path(params$data_dir, params$value_file),
    show_col_types = FALSE
  ) %>%
  janitor::clean_names() %>%
  assertr::verify(value >= 0) |>
  dplyr::filter(
    period %in% params$use_years,
    value > 0 # Subsets out the negligible values (status = N)
  ) %>%
  dplyr::group_by(
    country_un_code,
    species_alpha_3_code,
    area_code,
    environment_alpha_2_code,
    measure,
    period
  ) %>%
  dplyr::summarise(
    value_1000_usd = sum(value, na.rm = TRUE), .groups = "drop"
  ) |>
  dplyr::distinct()


# Import Aquaculture Production Quantities (tonnes wet weight) ------------

#
# - Subset for years which report positive values
# - Subset for years which are used in this analysis
#


logging::loginfo(
  "Importing Aquaculture quantities from %s/%s",
  params$data_dir, params$quantity_file
)

aqua$fisheries_quantity <- readr::read_csv(
  file.path(params$data_dir, params$quantity_file),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  assertr::verify(value >= 0) |>
  dplyr::filter(
    period %in% params$use_years,
    value > 0 # Subset out negligible values (Status = N)
  ) %>%
  dplyr::group_by(
    country_un_code,
    area_code,
    environment_alpha_2_code,
    species_alpha_3_code,
    measure,
    period
  ) %>%
  dplyr::summarise(
    tonnes = sum(value, na.rm = TRUE), .groups = "drop"
  ) %>%
  dplyr::distinct()




# Convert to price/tonne to allow easier conversions ----------------------
#
# Get the USD price for every reported quantity
# and filter for the correct project ISO3 codes
#
# Note: Can keep Hong Kong and Taiwan as they are not double counted.
#
# Note: once subset for negligable values, their are equal observations
# from both datasets.

fao_fisheries <- dplyr::full_join(aqua$fisheries_quantity,
  aqua$fisheries_values,
  by = c(
    "country_un_code",
    "area_code",
    "environment_alpha_2_code",
    "species_alpha_3_code",
    "period"
  )
) %>%
  dplyr::mutate(usd_price = if_else(tonnes > 0,
    (value_1000_usd / tonnes) * 1000, 0
  )) %>%
  dplyr::filter(usd_price > 0) %>%
  dplyr::rename(un_code = country_un_code) %>%
  dplyr::left_join(aqua$iso3codes, by = c("un_code")) %>%
  dplyr::rename(species_code = species_alpha_3_code, year = period) %>%
  dplyr::select(
    iso3_code,
    name_en,
    species_code,
    area_code,
    environment_alpha_2_code,
    year,
    tonnes,
    value_1000_usd,
    usd_price
  ) %>%
  dplyr::filter(iso3_code %in% project_iso3_codes$iso3_code) |>
  dplyr::rename(country = name_en) |>
  tidyr::drop_na(country) |>
  dplyr::ungroup()

# Check that there are no double ups in years
# Belgium starts at 1994
# Serbia, Montenegro, South Sudan, and Sudan all start after
# their splits
# fao_fisheries |>
#    dplyr::filter(iso3_code %in% c("BEL", "LUX", "SSD", "SDN", "MNE", "SRB")) |>
#    View()


# Check for outlier price per tonnes --------------------------------------
#
# These are objects which have a reported value of > $100,000 per tonne
#
# To determine if these are true outliers it must be first determined
# the species in question and then a judgement will be made from there.
#
# The list of ASFIS species that are provided with the Global Aquaculture
# production database is in
#  - cpc_groups_en_file
#

# Import the CPC groupings for every species
# CPC codes are 3 letters and there are over 12,000
# uniqe codes which are reported in this database
loginfo("Importing CPC code groupings from %s", params$cpc_group_en_file)

aqua_cpc_groups <-
  readr::read_csv(file.path(params$data_dir, params$cpc_group_en_file),
    show_col_types = FALSE
  ) |>
  janitor::clean_names() |>
  dplyr::distinct() |>
  dplyr::rename(
    species_code = x3a_code,
    name = name_en
  )


##
# From the Global Aquaculture Production Notes.txt file
# > NB: When viewing time-series data in FISHSTAT, in order to obtain the
# > aggregates presented in the
# > Summary Tables of the "FAO Yearbook of Fishery Statistics -
# > Aquaculture Production" which exclude production figures
# > for aquatic plants, pearls and mother-of-pearl, data should be filtered
# > using a Custom Group:
# >     "Fish, crustaceans and molluscs, etc."(1801).
# > The species of this group are shown in CL_FI_SPECIES_GROUPS.csv file
# > in the column Yearbook_Group.
# See also Aqua_H.html which is included in the Dataset
# > Fish, crustaceans, molluscs and all other aquatic organisms included in
# > the dataset have been classified according to approximately 630 commercial
# > species items, further arranged within the 50 groups of species
# > constituting the nine divisions of the FAO International Standard
# > Statistical Classification of Aquatic Animals and Plants (ISSCAAP).
# > The taxonomic code descriptors are taken from the " ASFIS list of species
# > for fishery statistics purposes".
#
# Therefore, this aggregated values should be confined to the values of
# Aquaculture which are reported on a commercial level.
##
fao_fisheries <- fao_fisheries |>
  dplyr::left_join(aqua_cpc_groups,
    by = "species_code"
  ) |>
  dplyr::filter(
    # To make the groupings match with the FAO Yearbook groupings for
    # Aquaculture Production
    yearbook_group_en == "Fish, crustaceans and molluscs, etc."
  )

##
# This produces 460 NA values for the cpc_group_en
# However, these appear to all be for either turtles or Frogs
# which are valid means of Aquaculture production
# in South America and South East Asia
##

#
# Check for outliers
#
# fao_fisheries |>
#     slice_max(usd_price, n = 200) |>
#     dplyr::arrange(desc(usd_price)) |>
#     View()

# No obvious outliers,
# the highest values are mostly for red abalone
# and sturgeons, which will be cavier eggs, which
# can have very large prices per tonne.
# Note: nei - "Not Elsewhere Included"



# Convert values Local Currency Units  -----------------------------

#
#
# To do this, note that currently all values and usd_prices
# are presented in current USD. Therefore to convert these values to
# current LCU the following process will be completed
#
#    1. Convert these values back into the standard local currently for the
#              prevailing region that these figures are reported in.
#              This conversion will be done using the World Bank series
#              from the IFS/IMF (PA.NUS.ATLS) which uses the weighted average,
#              exchange rate per area, in the case where a conversion
#              is missing, the series (PA.NUS.FCRF) will be used which uses
#              the monthly average official exchange rate.
#
#
##


logging::loginfo(paste0(
  "Importing PA.NUS.ATLS Weighted exchange rates",
  "imputed with PA.NUS.FRCF"
))

# Import PU.NUS.ATLS Exchange Rates
# Missing values have been imputed using the PU.NUS.FCRF
# exchange rates.
lcu_conversion <-
  config$data$output$lcu_conversion |>
  arrow::read_parquet() |>
  dplyr::filter(year %in% params$use_years)



#
# Check how many exchange rates are missing from the World Bank data
#
#  - Check what value will be lost due the the loss of this data
#
#
# fao_fisheries |>
#   dplyr::group_by(iso3_code, year, country) |>
#   dplyr::summarise(
#     value = 1e3 * sum(value_1000_usd, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   dplyr::anti_join(lcu_conversion, by = c("year", "iso3_code")) |>
#   dplyr::group_by(iso3_code, country, year) |>
#   dplyr::summarise(value = scales::dollar(sum(value))) |>
#   count(country) |>
#   View()

##
# The countries which are missing conversion rates are as follows
# Groups:   iso3_code, country [14]
# iso3_code country                      n
# <chr>     <chr>                    <int>
# 1 ASM       American Samoa               8 -> USD
# 2 GLP       Guadeloupe                  26 -> EURO
# 3 GUF       French Guiana               23 -> EURO
# 4 GUM       Guam                         8 -> USD
# 5 PRK       Korea, Dem. People's Rep    26 -> Remove - Data will be poor
# 6 REU       Réunion                     26 -> EURO
# 7 SCG       Serbia and Montenegro       12 -> Remove due to exchange rates
# 8 TWN       Taiwan Province of China    26 -> Do not have official Exchange rates
# 9 VEN       Venezuela, Boliv Rep of      2 -> Keep out due to exchange rates
#



# Convert to LCU ($) -----------------------
fao_fisheries <- fao_fisheries |>
  dplyr::left_join(lcu_conversion,
    by = c("year", "iso3_code"),
    suffix = c("", "_lcu")
  ) |>
  dplyr::rename(lcu_conversion = value) |>
  dplyr::relocate(lcu_conversion, .after = value_1000_usd) |>
  dplyr::mutate(
    lcu_value = value_1000_usd * lcu_conversion * 1e3 # LCU Conversion
  )



# Convert to Constant 2014-2016 US Dollars --------------------------------
logging::loginfo("Converting prices to constant 2014-2016 values")

# The process to convert to a constant time series is as follows
#
# Calculate the average lcu/tonne for each item in each region
# for the base period (2014-2016)
# then simply multiply the average lcu/tonne by the reported output
# tonne quantities, then convert back to USD using the average exchange rate
#
# In both cases, a weighted mean is used based on the total production
# for each year

# Base period that is used to perform the calculations
constant_year_base_period <- 2014:2016


# Constant USD price for the base period
constant_period_values <- fao_fisheries |>
  dplyr::filter(year %in% constant_year_base_period) |>
  dplyr::group_by(
    iso3_code,
    country,
    species_code,
    environment_alpha_2_code,
    area_code
  ) |>
  dplyr::summarise(
    constant_2014_2016_lcu_price = weighted.mean(lcu_value / tonnes, tonnes, na.rm = TRUE),
    constant_2014_2016_usd_exchange_rate = 1 / weighted.mean(lcu_conversion, tonnes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    constant_2014_2016_usd_price = constant_2014_2016_lcu_price * constant_2014_2016_usd_exchange_rate
  )

#
fao_fisheries <- fao_fisheries |>
  dplyr::left_join(
    constant_period_values,
    by = c(
      "iso3_code",
      "country",
      "species_code",
      "environment_alpha_2_code",
      "area_code"
    )
  ) |>
  dplyr::mutate(
    constant_2014_2016_lcu_value = tonnes * constant_2014_2016_lcu_price,
    constant_2014_2016_usd_value = tonnes * constant_2014_2016_usd_price,
    usd_value = 1e3 * value_1000_usd
  )


#
# Select out the relevant columns
#
# Includes all conversion factors and final values
#
fao_fisheries <- fao_fisheries |>
  dplyr::select(
    iso3_code,
    country,
    year,
    species_code,
    environment_alpha_2_code,
    area_code,
    name,
    cpc_group_en,
    tonnes,
    usd_value,
    usd_price,
    lcu_conversion,
    lcu_value,
    indicator_code,
    indicator_name,
    constant_2014_2016_lcu_price,
    constant_2014_2016_usd_price,
    constant_2014_2016_lcu_value,
    constant_2014_2016_usd_value
  )



# Output summary values and tables ----------------------------------------
# Automatically generate summary files and tables
# from this dataset
# the raw outputs from these are zipped
source(here::here("R", "table-functions.R"))
output_results_dir <- here::here(
  "output", "tables",
  paste0(
    "aquaculture_values_",
    format(Sys.Date(), format = "%Y%m%d")
  )
)

dir.create(output_results_dir, recursive = TRUE)



logging::loginfo(
  paste0(
    "Generating summary files and tables",
    " - outputs will be zipped into %s"
  ),
  output_results_dir
)


# Generate a summary file by country and year
summary_per_country <- aggregate(
  cbind(
    tonnes,
    usd_value,
    lcu_value,
    constant_2014_2016_lcu_value,
    constant_2014_2016_usd_value
  ) ~ iso3_code + country + year,
  data = fao_fisheries,
  FUN = sum, na.rm = TRUE
)

write.csv(summary_per_country,
  file = file.path(output_results_dir, "summary_per_country.csv"),
  row.names = FALSE
)

# Summary per year
summary_per_year <- aggregate(
  cbind(
    tonnes,
    usd_value,
    constant_2014_2016_usd_value
  ) ~ year,
  data = summary_per_country,
  FUN = sum, na.rm = TRUE
)

write.csv(summary_per_year,
  file = file.path(output_results_dir, "summary_per_year.csv"),
  row.names = FALSE
)


# Format the summaries by year and country
tbl_df <- summary_per_year |>
  dplyr::group_by(year) |>
  dplyr::summarise_at(
    vars(tonnes:constant_2014_2016_usd_value),
    ~ scales::comma(.x, scale = 1e-6, accuracy = 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(year)

# Summary table
generate_kbl(
  df = tbl_df,
  col_names = c(
    "year", "Tonnes (m)", "Million USD ($)",
    "Million 2014-2016 USD"
  ),
  caption = "Estimated Global Aquaculture Value and Tonnes",
  header_spec_fun = function(x) x,
  footnotes = c(
    "Aquaculture values selected using the FAOSTAT Yearbook  grouping 'Fish, crustaceans and molluscs, etc.'",
    paste0(
      "Source:", "FAO.GLOBAL AQUACULTURE PRODUCTION. License: CC BY–NC–SA 3.0 IGO.",
      "Extracted from:  https://www.fao.org/fishery/statistics-query/en/aquaculture.",
      "Date of Access: 2022-02-01."
    )
  ),
  output_name = file.path(output_results_dir, "summary_table.pdf")
)

# Create a summary csv file containing all the species contained
species_groups <- file.path(params$data_dir, params$cpc_group_en_file) |>
  readr::read_csv() |>
  janitor::clean_names() |>
  dplyr::filter(x3a_code %in% unique(fao_fisheries$species_code)) |>
  dplyr::rename(
    species_code = x3a_code
  )

generate_kbl(species_groups |>
  count(isscaap_group_en),
col_names = c("ISSCAAP Group", "Number"),
caption = "Aquatic Species By ISSCAAP Groupings",
header_spec_fun = function(x) x,
footnotes = "Grouping names obtained from: aquatic_fao_species_subset_used.csv",
output_name = here::here(output_results_dir, "aquatic_isscaap_groupings.pdf")
)

species_groups |>
  readr::write_csv(here::here(output_results_dir, "aquatic_fao_species_subset_used.csv"))

logging::loginfo("Zipping results directory %s", output_results_dir)
zip(
  zipfile = paste0(output_results_dir, ".zip"),
  files = list.files(output_results_dir, full.names = TRUE),
  flags = "-j"
)

# Remove the temporary output directory
unlink(output_results_dir, recursive = TRUE)


##  - Save the data  ------------------------------#
aqua_table <- arrow::Table$create(fao_fisheries)

logging::loginfo("Attaching metadata")

# Add initial metadata
aqua_table$metadata <- list(
  iso3_code = "ISO 3166-1 alpha-3",
  country = "Country name",
  year = "Year in %Y format",
  species_code = "ASFIS Species Code",
  environment_alpha_2_code = "IN=Freshwater,BW=Brackishwater,MA=Marine,AL=All",
  area_code = "Location (1-99), specified in CL_FI_WATERAREA_GROUPS.csv",
  name = "Species name",
  tonnes = "Metric Tonnes of live/wet weight",
  usd_value = "Current value in USD",
  usd_price = "USD/tonne",
  lcu_conversion = "LCU per USD (PA.NUS.ATLS and PU.NUS.FCRF)",
  lcu_value = "Current value in LCU (Conerted from USD)",
  indicator_code = "Code of the World Bank indicator used to convert USD to LCU",
  indicator_name = "Name of the World Bank indicator used to convert USD to LCU",
  constant_2014_2016_lcu_price = "Items constant 2014-2016 LCU Price",
  constant_2014_2016_usd_price = "Items constant 2014-2016 USD Price",
  constant_2014_2016_lcu_value = "Items constant 2014-2016 LCU Value",
  constant_2014_2016_usd_value = "Items constant 2014-2016 USD Value",
  date = iso_date(),
  contributor = "Gabriel Dennis CSIRO, gabriel.dennis@csiro.au",
  format = "Arrow Table",
  language = "English",
  source = paste0(
    "FAO.GLOBAL AQUACULTURE PRODUCTION. License: CC BY–NC–SA 3.0 IGO.",
    "Extracted from:  https://www.fao.org/fishery/statistics-query/en/aquaculture.",
    "Date of Access: 2022-02-01."
  )
)


# Write to File -----------------------------------------------------------
if (!dir.exists(dirname(config$data$output$crop_values))) {
  dir.create(dirname(config$data$output$crop_values))
}

logging::loginfo("Writing to parquet file: %s", config$data$output$aquaculture_values)
arrow::write_parquet(
  aqua_table,
  config$data$output$aquaculture_values
)

logging::loginfo("Exiting")
