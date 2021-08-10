#!/usr/bin/env Rscript  --vanilla


# Details -----------------------------------------------------------------
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
# Date Created:  20220204
#
# Description:  Computes an estimate of the  total economic value
# for Livestock
#
#
# Inputs:
#
# Livestock Value of Production is sourced from the
# FAOSTAT Gross Value of Production (QVL) table.
#
# Livestock Heads are sourced from the FAOSTAT
# Crop and Livestock Production Table (QCL)
#
# Livestock Output tonnes are source from the FAOSTAT
# Crop and Livestock Production Table (QCL)
#
# Livestock slaughtered are sourced from the FAOSTAT
# Crop and Livestock Production Table (QCL)
#
# Livestock Carcass Percentages are sourced from the FAO
# Technical Conversion Factors for Agricultural Commodities (TCF)
#
# Locations of these tables can be found in the project configuration file
## -conf/config.yml
#
# Outputs:
#      The output table from this file is located in
#      -data/output/ with the exact location specified via conf/config.yml
#      The output file is a parquet file which has attached metadata describing
#      the table schema. The point of placing the data into such a wide table
#      was to allow for minimal changes to be made to the source data initially,
#      with the final estimations mostly coming in any postprocessing tables
#      or figures
#
# Conversions to constant 2014-2016 USD is done using the World Bank
# Exchange rate Conversion series PU.NUS.ATLS (Weighted) and PU.NUS.FCRF
# (Official)
# Constant series are obtained for live animals by using a Stock Weighted mean
# to determine the live weight price for each animal type and the 2014-2016
# SLC to USD Exchange rate.
# From there, values are converted using the ratio of these two quantities.
#
#
#
# Conversions to Carcass (%) and Live Body Weights are done using
# Carcass percentages and live weights  from the FAO technical conversion factors
# These factors are imputed using the regional median values
# first at a local regional level (region23) then
# at a continent wide level, and finally at a global level.
#
# Activate Project --------------------------------------------------------

renv::activate(project = ".")


# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(logging)
  library(LivestockValueGBADS)
})



# Logging -----------------------------------------------------------------
basicConfig()


# Config ------------------------------------------------------------------

logging::loginfo("Loading configuration file")
config <- config::get()



# Load Data ---------------------------------------------------------------
get_data <- function(files, codes, use_years = 1994:2018) {
  data <- purrr::map(
    files,
    ~ arrow::read_parquet(.) |>
      janitor::clean_names() |>
      dplyr::filter(
        year %in% use_years,
        item_code %in% codes
      ) |>
      clean_countries() |>
      sanitize_columns()
  )
}

# Extract the data, and match it to the correct livestock codes
livestock_codes <- get_livestock_codes()

loginfo("Importing and sanatizing processed tables")

data_tables <- config$data$processed$tables

livestock <- get_data(
  list(
    production = data_tables$crops_and_livestock_products,
    value_of_production = data_tables$value_of_production,
    producer_prices = data_tables$producer_prices
  ),
  livestock_codes$codes
)



# Sanitize item strings into animal types ---------------------------------

#
# Remove unwanted characters from the
# from the beginning of the item column's
#
#  In this case, the value of production items that
#  can be removed are
#  "whole_fresh" - which are milk items
#
#   In the production (QCL) table these values
#   can also be sanitized, along with "edible"
#   which is used to describe fats and offals
#
# This is done so that each item string will now be of the form
# {output_type}_animal_name
#

# Regex to match these patterns
str_regex <- c(
  "[_]{0,1}whole_fresh[_]{0,1}",
  "[_]{0,1}edible[_]{0,1}"
)

# Regex to remove any underscores which remain at the start
# and end of any item string
line_regex <- "^[_]{0,}|[_]{0,}$"

str_regex <- paste0(str_regex, collapse = "|")

# Remove these patterns from each item string
livestock$value_of_production$item <- gsub(str_regex, "_",
  livestock$value_of_production$item,
  perl = TRUE
)
# Remove start and ends
livestock$value_of_production$item <- gsub(line_regex, "",
  livestock$value_of_production$item,
  perl = TRUE
)

livestock$production$item <- gsub(str_regex, "_",
  livestock$production$item,
  perl = TRUE
)
# Remove start and ends
livestock$production$item <- gsub(line_regex, "",
  livestock$production$item,
  perl = TRUE
)


# Price Series Intervals --------------------------------------------------

#
# Select Annual Values: FAOSTAT Months Code = 7021
# to retrieve the annual price series
# Monthly series are only available for a selection of
# commodities since 2010
#
# Note: About half of these annual series are imputed
# values
#
livestock$prices <- dplyr::filter(
  livestock$producer_prices,
  months_code == 7021
)



# Livestock Yield ---------------------------------------------------------
#
#
# Calculate the yearly livestock carcass weight/yield
# for each country for each animal type
#
# This is in dressing percentage and will need to be converted to
# Live Weight Equivalent
#
# This is different across countries and animals in how it is reported,
# however, we will assume that this is the true carcass yield for all animals
# due to the inconsistency in how the FAO collects data it is reported
# to the FAO, see
# https://fenixservices.fao.org/faostat/static/documents/QCL/QCL_methodology_e.pdf
# In most cases data is reported in dressed carcass weights.
#
#


loginfo("Computing Livestock Yield")

livestock$yield <- livestock$production |>
  dplyr::filter(
    element %in% c("producing_animals_slaughtered", "production"),
    item_code %in%
      unique(livestock_codes$code_list$livestock_meat_item_codes$item_code)
  ) |>
  dplyr::mutate(
    value = dplyr::case_when(
      stringr::str_detect(unit, "1000") ~ value * 1000,
      TRUE ~ value
    ),
    unit = dplyr::case_when(
      stringr::str_detect(unit, "1000") ~ "head",
      TRUE ~ unit,
    ),
    iso3_code = toupper(iso3_code)
  ) |>
  dplyr::select(-element, -element_code, -flag, -year_code) |>
  tidyr::spread(unit, value) |>
  dplyr::filter(head > 0) |> # Only get entries in which some values were recorded
  dplyr::mutate(
    yield_kg = round(tonnes * 1000 / head, 1)
  ) |>
  tidyr::drop_na(yield_kg) |>
  dplyr::filter(yield_kg > 0) |>
  assertr::verify((yield_kg > 0) & (yield_kg < 1000))


# Summary of Yields per animal
# tapply(livestock$yield$yield_kg, livestock$yield$item, FUN = summary)



# Technical Conversion Factors --------------------------------------------

#
# Carcass percentages and live weights  from the FAO technical conversion factors
# These factors are imputed using the regional median values
# first at a local regional level (region23) then
# at a continent wide level, and finally at a global level.
#


carcass_pct <- config$data$output$technical_conversion_factors_imputed |>
  arrow::read_parquet() |>
  dplyr::select(iso3_code, animal, carcass_pct_imp, live_weight_kg_imp) |>
  dplyr::rename(carcass_pct = carcass_pct_imp) |>
  dplyr::mutate(
    carcass_pct = carcass_pct / 100
  ) |>
  assertr::verify((carcass_pct > 0) & (carcass_pct < 1))



tapply(carcass_pct$carcass_pct, carcass_pct$animal, FUN = summary)


loginfo(paste(
  "Converting the FAO yields to live body weights using the",
  "FAO technical conversion factors"
))


#
# Match only meat items to create Yields per animal for
# lightweight conversions.
#
# In the case where the live weight conversions are missing
# from the FAOSTAT data, use the imputed live weights derived from the
# FAOSTAT Technical conversion factors
#
livestock$yield <- livestock$yield |>
  dplyr::mutate(
    animal = trimws(stringr::str_remove(item, "meat_"))
  ) |>
  dplyr::mutate(
    animal = case_when(
      animal == "goose_and_guinea_fowl" ~ "goose",
      TRUE ~ animal
    )
  ) |>
  dplyr::left_join(carcass_pct, by = c("iso3_code", "animal")) |>
  dplyr::mutate(
    lbw_kg = ifelse(!is.na(yield_kg), yield_kg / carcass_pct, live_weight_kg_imp)
  ) |>
  dplyr::mutate(
    item = "stock"
  ) |>
  dplyr::select(-head, -tonnes)



#
# Check results
#
tapply(livestock$yield$lbw_kg, livestock$yield$animal, FUN = summary)


# Remove LBW KG Outliers --------------------------------------------------
#
# Checking with
# Remove Malysia - Huge outlier due to low carcass pct
# Remove Niger - Appear to be consistently way too high
#
#
# livestock$yield |>
#    dplyr::group_by(animal) |>
#    dplyr::slice_max(lbw_kg, n  = 5) |> View()
#
#
livestock$yield[(livestock$yield$iso3_code %in% c("MYS", "NER")) &
  livestock$yield$animal == "cattle", ] <- NA



# Livestock Table ---------------------------------------------------------
# Create Livestock Table With Weights values and prices
#  - Use nested tibble data frames

livestock$value_of_production <-
  livestock$value_of_production |>
  dplyr::select(
    -element_code,
    -year_code,
    -unit,
    -flag
  ) |>
  tidyr::spread(
    element,
    value
  ) |>
  tidyr::separate(item, c("item", "animal"),
    sep = "_", extra = "merge", fill = "left"
  ) |>
  dplyr::mutate(
    animal = trimws(animal),
    item = tolower(item)
  )

# aggregate(gross_production_value_constant_2014_2016_thousand_us*1000 ~ year,
# data = livestock$value_of_production, FUN=sum, na.rm = TRUE)


#
# Extract livestock production and reshape
#
livestock$production <- livestock$production |>
  dplyr::filter(element %in% c(
    "stocks",
    "producing_animals_slaughtered",
    "production"
  )) |>
  dplyr::mutate(
    value = case_when(
      stringr::str_detect(unit, "1000") ~ value * 1000,
      TRUE ~ value
    ),
    unit = case_when(
      stringr::str_detect(unit, "1000") ~ "head",
      TRUE ~ unit
    )
  ) |>
  tidyr::separate(item, c("item", "animal"),
    sep = "_",
    extra = "merge", fill = "right"
  ) |>
  dplyr::mutate(
    item = ifelse(element == "stocks", "stock", item)
  ) |>
  dplyr::select(-element, -element_code, -year_code, -flag) |>
  dplyr::mutate(
    animal = trimws(animal)
  ) |>
  tidyr::spread(unit, value)


#
# Extract out the livestock live weight prices and assign them to match
# the stock items
#
livestock$prices <- livestock$prices |>
  dplyr::select(
    -element_code, -year_code, -unit,
    -flag, -months, -months_code, -unit
  ) |>
  tidyr::separate(
    item,
    c("item", "animal"),
    sep = "_",
    extra = "merge",
    fill = "right"
  ) |>
  dplyr::mutate(
    animal = trimws(animal)
  ) |>
  tidyr::spread(element, value) |>
  dplyr::mutate(
    item = case_when(
      stringr::str_detect(animal, "live_weight") ~ "stock",
      TRUE ~ item
    ),
    item = tolower(item)
  ) |>
  dplyr::filter(animal != "meat_nes")


# Check the price per animal
tapply(livestock$prices$producer_price_usd_tonne,
  livestock$prices$animal,
  FUN = summary, na.rm = TRUE
)


#
# Create a list to match the items by FAOSTAT code for each table
#
# Stock Item Code is first  -> From QVL
# Price Items second, - > From PP (Live Weights)
# Yield Items third -> From QVL (Slaughtered)
#
#
match_live_weight_items <- list(
  ass = c(1107, 1123, 1108),
  buffalo = c(946, 973, 947),
  camel = c(1126, 1138, 1127),
  cattle = c(866, 945, 867),
  chicken = c(1057, 1095, 1058),
  duck = c(1068, 1071, 1069),
  goat = c(1016, 1033, 1017),
  goose = c(1072, 1078, 1073),
  horse = c(1096, 1121, 1097),
  mule = c(1110, 1125, 1111),
  pig = c(1034, 1056, 1035),
  rabbit = c(1140, 1145, 1141),
  sheep = c(976, 1013, 977),
  turkey = c(1079, 1088, 1080)
)

# Match all tables to the stock codes
matches <- setNames(
  data.frame(t(as_tibble(match_live_weight_items))),
  c("stock_code", "price_code", "yield_code")
)

# Save this table to the codes directory
saveRDS(
  match_live_weight_items |>
    setNames(c("qvl_stock_code", "pp_price_code", "qvl_yield_code")),
  file.path("data", "codes", "faostat_livestock_matching_codes.rds")
)


# Match Item and Animal ---------------------------------------------------



# Prices ------------------------------------------------------------------

recode_lvs <- function(var) {
  recode(
    var,
    and_guinea_fowls = "goose",
    goose_and_guinea_fowl = "goose",
    hen_in_shell = "chicken",
    and_hares = "rabbit",
    buffaloes = "buffalo",
    camels = "camel",
    cow = "cattle",
    goats = "goat",
    goose_and_guinea_fowl = "goose",
    horses = "horse",
    pigs = "pig"
  )
}


table(livestock$prices$item, livestock$prices$animal)
livestock$prices$animal <- stringr::str_remove_all(
  livestock$prices$animal,
  "live_weight_|whole_fresh_"
)

livestock$prices$animal <- recode_lvs(livestock$prices$animal)
table(livestock$prices$item, livestock$prices$animal)

#
# Check that all live weight items have matched correctly with
# their associated price item code
#
stopifnot(
  length(setdiff(
    names(match_live_weight_items),
    livestock$prices$animal
  )) == 0
)


# Production --------------------------------------------------------------

livestock$production$animal <- purrr::imap_chr(
  livestock$production$animal,
  ~ ifelse(is.na(.x),
    rownames(matches)[matches$stock_code == livestock$production$item_code[.y]], .x
  )
)

table(livestock$production$item, livestock$production$animal)
livestock$production$animal <- recode_lvs(livestock$production$animal)


stopifnot(
  length(setdiff(names(match_live_weight_items), unique(livestock$production$animal))) == 0
)



# Value of Production -----------------------------------------------------
table(livestock$value_of_production$item, livestock$value_of_production$animal)

livestock$value_of_production$animal <- recode_lvs(livestock$value_of_production$animal)
stopifnot(
  length(setdiff(names(match_live_weight_items), unique(livestock$value_of_production$animal))) == 0
)



# Yield (Should all be stock)
table(livestock$yield$item, livestock$yield$animal)

stopifnot(
  length(setdiff(names(match_live_weight_items), unique(livestock$yield$animal))) == 0
)

livestock$yield$animal <- recode_lvs(livestock$yield$animal)


# Create Livestock DF -----------------------------------------------------

# Match the Live Weight Price Item Codes to the stock items
livestock <- livestock[c("production", "yield", "value_of_production", "prices")]



livestock_df <- purrr::reduce(livestock, full_join,
  by = c("iso3_code", "faost_code", "area", "year", "item", "animal")
)


aggregate(livestock_df$gross_production_value_constant_2014_2016_thousand_us * 1000,
  by = livestock_df["year"],
  FUN = sum, na.rm = TRUE
)


# Asset Values ------------------------------------------------------------
livestock_df <- livestock_df |>
  dplyr::mutate(
    tonnes = dplyr::case_when(
      is.na(tonnes) ~ lbw_kg * head / 1000,
      TRUE ~ tonnes
    )
  ) |>
  dplyr::mutate(
    stock_value_lcu = ifelse(item == "stock",
      producer_price_lcu_tonne * tonnes, 0
    ),
    stock_value_slc = ifelse(item == "stock",
      producer_price_slc_tonne * tonnes, 0
    ),
    stock_value_usd = ifelse(item == "stock",
      producer_price_usd_tonne * tonnes, 0
    )
  )

#
# Check values in USD
# For 2018 they should be around 1e12
#
aggregate(livestock_df$stock_value_usd, livestock_df["year"], sum, na.rm = TRUE)


# Constant Dollars --------------------------------------------------------

# Convert all liveweight prices to constant USD using a base period of 2014-2016
# to match the series already present for livestock value of production from
# FAOSTAT
#
# To obtain the constant price series for each animal, the stock numbers will be
# used as a weighting variable for each of the base year, so that the
#

#
# Import PU.NUS.ATLS Exchange Rates from the world bank
# Missing values have been imputed using the PU.NUS.FCRF
# exchange rates.
# provided via the IFS
#

lcu_conversion <-
  config$data$output$lcu_conversion |>
  arrow::read_parquet() |>
  dplyr::filter(year %in% 2014:2016)

#
# Get average 2014-2016 US dollar prices for each livestock
# type and product using a weighted
# average exchange rate
#
# The mean exchange rate is weighted by the stock held by that country
# in the given year, and the price is similarly weighted
#
mean_usd_prices_2014_2016 <- livestock_df |>
  dplyr::filter(year %in% 2014:2016, head > 0) |>
  dplyr::ungroup() |>
  dplyr::select(iso3_code, item, animal, head, year, producer_price_slc_tonne) |>
  dplyr::left_join(lcu_conversion, by = c("iso3_code", "year")) |>
  dplyr::group_by(iso3_code, animal, item) |>
  dplyr::summarise(
    mean_slc_price_per_tonne_2014_2016 = weighted.mean(producer_price_slc_tonne,
      head,
      na.rm = TRUE
    ),
    mean_usd_conversion_2014_2016 = 1 / weighted.mean(value, head, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    producer_price_usd_per_tonne_2014_2016 = mean_slc_price_per_tonne_2014_2016 *
      mean_usd_conversion_2014_2016
  )


# Liveweight prices per animal ----------------------------------------------------------
aggregate(producer_price_usd_per_tonne_2014_2016 ~ animal + item,
  data = mean_usd_prices_2014_2016, summary,
  na.rm = TRUE
)

#
# To determine the outliers we can get the min and max
# of each group
#
# mean_usd_prices_2014_2016 |>
#      group_by(animal) |>
#      slice_max(producer_price_usd_per_tonne_2014_2016, n = 5) |>
#      View()



# Append the average prices to the existing dataframe
livestock_df <- livestock_df |>
  dplyr::left_join(mean_usd_prices_2014_2016,
    by = c(
      "iso3_code",
      "animal",
      "item"
    )
  ) |>
  dplyr::mutate(
    stock_value_constant_2014_2016_usd = ifelse(item == "stock",
      producer_price_usd_per_tonne_2014_2016 * tonnes, 0
    )
  )

# Get the gross production value for each year
aggregate(gross_production_value_constant_2014_2016_thousand_us * 1000 ~ year,
  data = livestock_df,
  FUN = sum, na.rm = TRUE
)

tapply(stock_value_constant_2014_2016_usd ~ year,
  data = livestock_df,
  FUN = summary
)

# Get the stock value per year
aggregate(stock_value_constant_2014_2016_usd ~ year,
  data = livestock_df,
  sum, na.rm = TRUE
)

#
# Venezuela shows higher than expected values for its asset prices
# however, non of the conversions are too outlandish since they
# do have approximately 15 million head of cattle in stock
# therefore, they have been left in the estimation at this current
# time.
# The conversions that are done to convert from lcu to USD in a constant
# manner are do not produce radically different prices compared to the
# current set of FAO conversions

# livestock_df |>
#    dplyr::filter(iso3_code == "VEN", item == "stock", year %in% 2014:2016) |>
#    dplyr::select(year, item, animal, producer_price_usd_tonne,
#                  producer_price_usd_per_tonne_2014_2016) |>
#    View()

#  - Attach metadata  ------------------------------

# Turn into arrow table so schema and metadata can be attached
metadata_list <- list(
  iso3_code = "ISO 3166-1 alpha-3",
  faost_code = "FAOSTAT Area Code",
  area = "FAOSTAT Area Name",
  year = "Year in YYYY format",
  item_code = "FAOSTAT item code",
  item = "FAOSTAT production item name (lowercase)",
  animal = "english name for livestock type",
  head = "Number of animals",
  tonnes = "Metric Tonnes",
  gross_production_value_constant_2014_2016_thousand_i = "Gross production value of item in constant thousand 2014 2016 international dollars, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_constant_2014_2016_thousand_slc = "Gross production value of item in constant 2014 2016 in thousand standard local currency units, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_constant_2014_2016_thousand_us = "Gross production value of item in constant thousand 2014 2016 US dollars,  for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_current_thousand_slc = "Gross production value of item in current thousand   standard local currency units, for a certain item calculated to average 100 between 2014 and 2016",
  gross_production_value_current_thousand_us = "Gross production value of item in current thousand US dollars",
  producer_price_index_2014_2016_100 = "An FAOSTAT Items producer price index, for a certain item calculated to average 100 between 2014 and 2016",
  producer_price_lcu_tonne = "Producer price of item in local currency per tonne",
  producer_price_slc_tonne = "Producer price of item in national currency per tonne",
  producer_price_usd_tonne = "Producer price of item in current USD per tonne",
  yield_kg = "Animal yield/carcass weight calculated from FAOSTAT data",
  carcass_pct = "FAO carcass % conversion factor",
  lbw_kg = "Adult live body weight equivalent in kg, calculated via yield_kg/(carcass_pct/100)",
  stock_value_lcu = "Value of animal stock in local currency units, calculated via producer_price_lcu_tonne * tonnes",
  stock_value_slc = "Value of animal stock in standard currency units, calculated via producer_price_slc_tonne * tonnes",
  stock_value_usd = "Value of animal stock in US dollars, calculated via producer_price_usd_tonne * tonnes",
  mean_slc_price_per_tonne_2014_2016 = "mean slc price per tonne averaged over 2014 to 2016 using a head weigted mean",
  mean_usd_conversion_2014_2016 = "Mean conversion of slc to US dollars from  2014 to 2016 using the annual exchange rates weighted by the number of head of stock",
  producer_price_usd_per_tonne_2014_2016 = "Producer price per tonne in USD, calculated via mean_slc_price_per_tonne_2014_2016 / mean_usd_conversion_2014_2016",
  stock_value_constant_2014_2016_usd = "Value of animal stock in constant 2014 2016 US dollars, calculated via producer_price_usd_per_tonne_2014_2016 * tonnes",
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

# Subset for the names specified in the table
# metadata schema above
livestock_df <- livestock_df[, names(livestock_df) %in% names(metadata_list)]


# Convert to an arrow table for storage as a parquet file
livestock_table <- arrow::Table$create(livestock_df)



# Attach metadata
livestock_table$metadata <- metadata_list




##  - Write to file  ------------------------------#
loginfo("Writing to File")

arrow::write_parquet(
  livestock_table,
  here::here("data", "output", "faostat", "faostat_livestock_values.parquet")
)
