#! /usr/bin/Rscript --vanilla


# Description -------------------------------------------------------------
#
# Project: GBADS
# Name: generate-tables.R
#
# Author: Gabriel Dennis <gabriel.dennis@csiro.au>
#
# Description:  Generates different tables for the
# livestock value manuscript
#
# For examples of output tables:
# see output/tables/README.md
#
#
# To remake output tables, run
# `make tables` from the project root directory
# -------------------------------------------------------------------------


# Activate Project --------------------------------------------------------
renv::activate(project = here::here("."))



# Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(openxlsx)
})



# Configuration -----------------------------------------------------------
config <- config::get()




# Constants ---------------------------------------------------------------

# Tempral range to show in each output table
year_span <- 1998:2018




# Create Excell Files of Tables -------------------------------------------

# Livestock
livestock <- config$data$output$livestock_values |>
  arrow::read_parquet() |>
  dplyr::filter(year %in% year_span)

# Aquaculture
aquaculture <- config$data$output$aquaculture_values |>
  arrow::read_parquet() |>
  dplyr::filter(year %in% year_span)

# Crops
crops <- config$data$output$crop_values |>
  arrow::read_parquet() |>
  dplyr::filter(year %in% year_span)



# Total Values ------------------------------------------------------------

livestock_total <- livestock |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    asset = sum(ifelse(item == "stock", stock_value_constant_2014_2016_usd, 0), na.rm = TRUE),
    output = sum(ifelse(item != "stock", gross_production_value_constant_2014_2016_thousand_us, 0), na.rm = TRUE) * 1e3,
    livestock_tonnes = sum(ifelse(item != "stock", tonnes, 0), na.rm = TRUE)
  ) |>
  dplyr::mutate(
    livestock = asset + output
  ) |>
  dplyr::select(-asset, -output) |>
  dplyr::select(year, livestock, livestock_tonnes)

aquaculture_total <- aquaculture |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    aquaculture = sum(constant_2014_2016_usd_value, na.rm = TRUE),
    aquaculture_tonnes = sum(tonnes, na.rm = TRUE)
  )

crop_total <- crops |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    crops = sum(gross_production_value_constant_2014_2016_thousand_us, na.rm = TRUE) * 1e3,
    crops_tonnes = sum(tonnes, na.rm = TRUE)
  )

total_table <- purrr::reduce((list(crop_total, livestock_total, aquaculture_total)), full_join, by = "year") |>
  arrange(desc(year)) |>
  dplyr::mutate_at(vars(contains("tonnes")), ~ scales::comma(.x, scale = 1e-6, suffix = "M", accuracy = 1)) |>
  dplyr::mutate_at(vars(-contains("tonnes"), -year), ~ scales::dollar(.x, scale = 1e-9, suffix = "B", accuracy = 1))

names(total_table) <- c(
  "Year", "Crops Value (USD ($))", "Crop Quantities (Tonnes)",
  "Livestock Value (USD ($))",
  "Livestock Quantities (Tonnes)",
  "Aquaculture Value (USD ($))",
  "Aquaculture Quantities (Tonnes)"
)

wb <- createWorkbook(creator = "Gabriel Dennis")
addWorksheet(wb, sheetName = "TotalValueAndQuantity")
writeData(wb,
  "TotalValueAndQuantity",
  "Note: All values are in constant 2014-2016 US Dollars",
  startCol = 1, startRow = 1
)

writeData(wb,
  "TotalValueAndQuantity",
  total_table,
  startCol = 1, startRow = 3
)

saveWorkbook(wb, here::here("output", "tables", glue::glue("table_appendix_tev_crops_livestock_aquaculture_{year_span[1]}-{year_span[2]}.xlsx")),
  overwrite = TRUE
)


# Total Values with Aquaculture ---------------------------------------------------------------


# Livestock Totals
livestock_total <- livestock |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    livestock_asset = sum(ifelse(item == "stock", stock_value_constant_2014_2016_usd, 0), na.rm = TRUE),
    livestock_output = sum(ifelse(item != "stock", gross_production_value_constant_2014_2016_thousand_us, 0), na.rm = TRUE) * 1e3,
    livestock_tonnes = sum(ifelse(item != "stock", tonnes, 0), na.rm = TRUE)
  )

# Aquaculture Totals
aquaculture_total <- aquaculture |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    aquaculture_asset = NA,
    aquaculture_output = sum(constant_2014_2016_usd_value, na.rm = TRUE),
    aquaculture_tonnes = sum(tonnes, na.rm = TRUE)
  )


# Combined Totals
total_table <- dplyr::full_join(livestock_total, aquaculture_total, by = "year") |>
  arrange(desc(year)) |>
  dplyr::mutate_at(vars(contains("tonnes")), ~ ifelse(is.na(.x), NA, scales::comma(.x, scale = 1e-6, suffix = "M", accuracy = 1))) |>
  dplyr::mutate_at(vars(-contains("tonnes"), -year), ~ ifelse(is.na(.x), NA, scales::dollar(.x, scale = 1e-9, suffix = "B", accuracy = 1)))

names(total_table) <- c(
  "Year",
  "Livestock Asset Value (USD ($))",
  "Livestock Output Value (USD ($))",
  "Livestock Quantities (Tonnes)",
  "Aquaculture Asset Value (USD ($))",
  "Aquaculture Output Value (USD ($))",
  "Aquaculture Quantities (Tonnes)"
)

wb <- createWorkbook(creator = "Gabriel Dennis")
addWorksheet(wb, sheetName = "AssetAndOutput")
writeData(wb,
  "AssetAndOutput",
  "Note: All values are in constant 2014-2016 US Dollars",
  startCol = 1, startRow = 1
)

writeData(wb,
  "AssetAndOutput",
  total_table,
  startCol = 1, startRow = 3
)

saveWorkbook(wb,
  file = here::here("output", "tables",
                    glue::glue("table_appendix_asset_output_livestock_aquaculture_{year_span[1]}-{year_span[2]}.xlsx")),
  overwrite = TRUE
)



# Selected Countries ------------------------------------------------------

high_impact_countries <- c("BRA", "CHN", "IND", "USA")

livestock_selected <- livestock |>
  dplyr::filter(iso3_code %in% high_impact_countries) |>
  dplyr::group_by(year, iso3_code) |>
  dplyr::summarise(
    asset = sum(stock_value_constant_2014_2016_usd, na.rm = TRUE),
    output = sum(gross_production_value_constant_2014_2016_thousand_us, na.rm = TRUE) * 1e3,
    tonnes = sum(tonnes, na.rm = TRUE)
  )

livestock_total <- livestock |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total_asset = sum(stock_value_constant_2014_2016_usd, na.rm = TRUE),
    total_output = sum(gross_production_value_constant_2014_2016_thousand_us, na.rm = TRUE) * 1e3,
    livestock_tonnes = sum(tonnes, na.rm = TRUE)
  )

table_df <- livestock_selected |>
  left_join(livestock_total) |>
  dplyr::mutate(
    `Asset Value (USD ($))` = sprintf(
      "%s (%s)", scales::dollar(asset, scale = 1e-9, suffix = "B"),
      scales::percent(asset / total_asset, accuracy = 1)
    ),
    `Output Value (USD ($))` = sprintf(
      "%s (%s)", scales::dollar(output, scale = 1e-9, suffix = "B"),
      scales::percent(output / total_output, accuracy = 1)
    ),
    Tonnes = sprintf(
      "%s (%s)", scales::dollar(tonnes, scale = 1e-6, suffix = "M"),
      scales::percent(tonnes / livestock_tonnes, accuracy = 1)
    )
  ) |>
  select(year, iso3_code, `Asset Value (USD ($))`, `Output Value (USD ($))`, Tonnes) |>
  mutate(
    Country = case_when(
      iso3_code == "BRA" ~ "Brazil",
      iso3_code == "CHN" ~ "China",
      iso3_code == "IND" ~ "India",
      iso3_code == "USA" ~ "USA"
    )
  ) |>
  tidyr::pivot_wider(
    id_cols = year, names_from = Country, names_glue = "{Country} {.value}",
    values_from = c(`Asset Value (USD ($))`, `Output Value (USD ($))`, Tonnes),
    names_sort = TRUE
  ) |>
  arrange(desc(year)) %>%
  select("year", sort(colnames(.)[2:length(colnames(.))], decreasing = F))




wb <- createWorkbook(creator = "Gabriel Dennis")
addWorksheet(wb, sheetName = "SelectedCounties")
writeData(wb,
  "SelectedCounties",
  "Note: All values are in constant 2014-2016 US Dollars",
  startCol = 1, startRow = 1
)

writeData(wb,
  "SelectedCounties",
  table_df,
  startCol = 1, startRow = 3
)

saveWorkbook(wb,
  file = here::here("output", "tables", glue::glue("table_appendix_bra_chn_ind_usa_{year_span[1]}-{year_span[2]}.xlsx")),
  overwrite = TRUE
)



# Value Proportion in LMICs -----------------------------------------------

# Table showing the value proportion in LMIC countries as a
# proportion of the total value

#
# Read in IMF Country designations
income_config <-
  config$data$source$tables$income_classification_history
income_classifications <-
  file.path(income_config$dir, income_config$file_name) |>
  readxl::read_xlsx(
    sheet = 3,
    skip = 11,
    col_names = c(
      "iso3_code",
      "country",
      as.character(1987:2020)
    )
  ) |>
  tidyr::gather(key = "year", value = "gini_income_cl", -iso3_code, -country) |>
  dplyr::mutate(
    year = as.numeric(year),
    gini_income_cl = factor(
      gini_income_cl,
      levels = c("L", "LM", "UM", "H"),
      labels = c(
        "Low Income",
        "Low Middle Income",
        "Upper Middle Income",
        "High Income"
      )
    )
  )

livestock_total <- livestock |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total_asset = sum(stock_value_constant_2014_2016_usd, na.rm = TRUE),
    total_output = sum(gross_production_value_constant_2014_2016_thousand_us, na.rm = TRUE) * 1e3
  ) |>
  dplyr::mutate(
    total = total_asset + total_output
  ) |>
  dplyr::ungroup()

# Livestock
livestock_lmics <- livestock |>
  dplyr::group_by(year, iso3_code) |>
  dplyr::summarise(
    total_asset = sum(stock_value_constant_2014_2016_usd, na.rm = TRUE),
    total_output = sum(
      gross_production_value_constant_2014_2016_thousand_us,
      na.rm = TRUE
    )
  ) |>
  dplyr::left_join(income_classifications,
    by = c("iso3_code", "year")
  ) |>
  dplyr::filter(gini_income_cl == "Low Middle Income") |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total_asset = sum(total_asset, na.rm = TRUE),
    total_output = sum(total_output, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    total_lmics = total_asset + total_output
  ) |>
  dplyr::select(year, total_lmics)

# Form table
table_df <- dplyr::left_join(livestock_total, livestock_lmics) |>
  dplyr::select(year, total, total_lmics) |>
  dplyr::mutate(
    `Total Value ($)` = scales::dollar(total,
      scale = 1e-12,
      accuracy = 0.05,
      suffix = "T"
    ),
    `Lower Middle Income Value ($)` = scales::dollar(total_lmics,
      scale = 1e-9,
      accuracy = 5,
      suffix = "B"
    ),
    `Lower Middle Income Value (%)` = scales::percent(total_lmics / total,
      accuracy = 1
    )
  ) |>
  dplyr::arrange(desc(year)) |>
  dplyr::select(-total, -total_lmics)

wb <- createWorkbook(creator = "Gabriel Dennis")
addWorksheet(wb, sheetName = "LivestockLMICValue")
writeData(wb,
  "LivestockLMICValue",
  "Note: All values are in constant 2014-2016 US Dollars",
  startCol = 1, startRow = 1
)

writeData(wb,
  "LivestockLMICValue",
  "Note: China moves from Lower Middle Income to Upper Middle Income starting in 2010",
  startCol = 1, startRow = 2
)

writeData(wb,
  "LivestockLMICValue",
  table_df,
  startCol = 1, startRow = 5
)

writeData(wb,
  "LivestockLMICValue",
  sprintf(
    "Source: Income Classifications sourced from %s",
    config$data$source$tables$income_classification_history$url
  ),
  startCol = 1, startRow = 25
)


saveWorkbook(wb, here::here("output", "tables", "table_lmic_proportion_1998-2018.xlsx"),
  overwrite = TRUE
)


# Generate Value tables by Country ----------------------------------------


# Livestock Asset + output values Globally

lvst_df <- livestock |>
  dplyr::select(
    iso3_code, area, year, item, animal, gross_production_value_constant_2014_2016_thousand_us,
    stock_value_constant_2014_2016_usd
  ) |>
  dplyr::group_by(iso3_code, area, year, animal) |>
  dplyr::summarise(
    asset_value = sum(ifelse(item == "stock", stock_value_constant_2014_2016_usd, 0), na.rm = TRUE),
    output_value = sum(ifelse(item != "stock", gross_production_value_constant_2014_2016_thousand_us,
      0
    ), na.rm = TRUE) * 1e3, .groups = "drop"
  ) |>
  dplyr::mutate(
    animal = case_when(
      !(animal %in% c("cattle", "chicken", "pig", "sheep", "goat")) ~ "Other Livestock",
      TRUE ~ animal
    )
  ) |>
  dplyr::mutate(
    animal = stringr::str_to_title(animal)
  )

lvst_df[lvst_df == 0] <- NA

grouping_sum <- function(df, var1, var2, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      "{{ var1 }}" := sum({{ var1 }}, na.rm = TRUE),
      "{{ var2 }}" := sum({{ var2 }}, na.rm = TRUE),
      .groups = "drop"
    )
}

convert_to_table <- function(df, suffix = "B") {
  scale_val <- switch(suffix,
    "M" = 1e-6,
    "B" = 1e-9,
    "T" = 1e-12
  )
  df <- df |>
    dplyr::rename_with(
      ~ stringr::str_to_title(gsub("_", " ", .x))
    ) |>
    dplyr::mutate_if(
      is.numeric,
      ~ replace(.x, .x == 0, NA)
    ) |>
    dplyr::arrange(desc(Year)) |>
    dplyr::mutate_at(
      vars(contains("Value")),
      ~ scales::dollar(.x,
        scale = scale_val, suffix = suffix,
        accuracy = 1
      )
    )
  if ("Area" %in% names(df)) {
    df <- df |> dplyr::mutate(
      Area = countrycode::countrycode(`Iso3 Code`, "iso3c", "country.name")
    )
  }
  df
}

# Table of values
# Per Animal and per country
# Use the same grouping of animals that is used elsewhere
# Cattle, Chicken, Pig, Sheep, Goat, Other Livestock
per_country_livestock_values <- lvst_df |>
  grouping_sum(asset_value, output_value, iso3_code, area, year, animal) |>
  convert_to_table(suffix = "M")


# Table of values per country
# Output and asset values per country
per_country_values <- lvst_df |>
  grouping_sum(asset_value, output_value, iso3_code, area, year) |>
  convert_to_table(suffix = "M")

# Globally
per_year_values <- lvst_df |>
  grouping_sum(asset_value, output_value, year) |>
  convert_to_table(suffix = "B")
