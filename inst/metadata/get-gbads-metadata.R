#!/usr/bin/env Rscript --vanilla

# Info ----------------------------------------------
#
# Name: get-gbads-metadata.R
# Project: GBADS (Global Burden of Animal Disease)
# Author: Gabriel Dennis
# Email: Gabriel.Dennis@csiro.au
#
# Date Created: 20220809
#
# Description:
#
#   This script generates the json metadata
#   files for the GBADS Knowledge engine portal for each
#   dataset used in this analysis.
#
# Outputs:
#
#   All output json metadata files will be stored in the
#   directory `data/metadata`.
#
# The point of this script as it is, is that all the information that
# will be stored in each of the datasets and displayed to the JSON metadata
# file
#  ---------------------------------------------------


# Activate Project --------------------------------------------------------

renv::activate(".")

# Config -----------------------------------------------------------------

config <- config::get()

# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(LivestockValueGBADS)
})


# Metadata File Naming ----------------------------------------------------

#' metadata_file_name
#'
#' @param filename metadata file name as a string
metadata_file_name <- function(filename) {
  file.path(
    "data",
    "metadata",
    sprintf(
      "%s_%s_metadata.json",
      format(Sys.Date(), format = "%Y%m%d"),
      filename
    )
  )
}



#' remove_newlines
#'
#' Recursively removes newlines from a list
#'
#' @param metadata_list  a nested list  of metadata files
remove_newlines <- function(metadata_list) {
  if (class(metadata_list) != "list") {
    gsub(
      "\\s+", " ",
      gsub("\\n", " ", metadata_list)
    )
  } else {
    purrr::map(metadata_list, ~ remove_newlines(.x))
  }
}




# Load Item Codes ---------------------------------------------------------

codes <- get_livestock_codes()

country_codes <- arrow::read_parquet(
  here::here(config$data$codes$faostat$country_codes$output_codes)
) |>
  drop_na(iso3_code) |>
  pull(iso3_code)


# Tables Metadata-----------------------------------------------------------

# Get the tables from the configuration file
table_metadata <- config$data$output

# Load metadata for every

metadata <- list(
  qcl = with(config$data$source$tables, crops_and_livestock_products),
  qv = with(config$data$source$tables, value_of_production),
  pp = with(config$data$source$tables, producer_prices)
)


# QCL ---------------------------------------------------------------------

qcl_values_metadata <- list(
  About = list(
    text = 'The Global Burden of Animal Disease Informatics Metadata provides
           information about how data can be accessed via the GBADs API, as well
           as other relevant information about the data. All metadata presented
           here pertains to the data after it was collected by GBADs
           (i.e. publication date on the GBADs API). This page provides a
           summary of metadata in a user-friendly format. To download the full
           metadata for this dataset select the "download as json-ld" button
           at the top of this page. Information about terms used or to access
           FAOSTAT metadata for this dataset can be found here:',
    metadata_url = with(metadata$qcl, metadata_url)
  ),
  Name = "FAOSTAT Crops and Livestock Products (QCL)",
  Identifier = with(metadata$qcl, url),
  PublicationDate = "25/07/2022",
  Creator = "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)",
  `Contact Point` = "The Global Burden of Animal Disease Informatics informatics@gbadske.org",
  Description = paste0("Data were accessed via the FAOSTAT Fenix Services bulk download link (see identifier).
                The crop production in tonnes was used from the  bulk download.
                The data were processed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-crop-values.R).
               The FAOSTAT crop items which are included in this data set are
               those in the CPC Code  groupings from Division 01
               'Products of Agriculture Horticulture and market Gardening'.
               In particular the groupings, Cereals (011), Vegetables (012), Fruits and Nuts (013),
               Oilseeds and oleaginous fruits (015),Edible roots and tubers with high starch or insulin content (015),
               Stimulant, spice and aromatic crops (016), Pulses (dried leguminous vegetables) (017),
               Sugar crops (018).
               For a full list of crops included in the analysis see the table in the
                [documentation for the analysis](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/vignettes/Documentation.Rmd).",
    sprintf(
      "The Livestock stock numbers, production quantity and number of livestock slaughtered were used from this bulk
    download. The data were processeed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-livestock-values.R).
    The FAOSTAT live animal items which where included in this dataset are %s (Item Codes: %s)
    The FAOSTAT slaughtered animal meat production items  which where included in this dataset are %s (Item Codes: %s)
  ",
      paste(with(
        codes$code_list$livestock_codes,
        unique(item[domain_code == "QCL"])
      ), collapse = ","),
      paste(with(
        codes$code_list$livestock_codes,
        unique(item_code[domain_code == "QCL"])
      ), collapse = ","),
      paste(unique(codes$code_list$livestock_meat_item_codes$item), collapse = ","),
      paste(unique(codes$code_list$livestock_meat_item_codes$item_code), collapse = ",")
    ),
    sep = " "
  ),
  `Statistical Unit` = "Tonnes,Head",
  Periodicity = "Annual",
  Licence = "FAO Open Data for Statistical Databases Policy
             [http://www.fao.org/3/ca7570en/ca7570en.pdf](http://www.fao.org/3/ca7570en/ca7570en.pdf)",
  Language = "English",
  `Date Modified` = metadata$qcl$last_download,
  Species = paste(with(
    codes$code_list$livestock_codes,
    unique(item[domain_code == "QCL"])
  ), collapse = ","),
  Subject = "Economic Value, Crops, Livestock",
  `Temporal Coverage` = list(
    Start = 1994,
    End = 2018
  ),
  `Spatial Coverage` = list(
    text = "Data are reported by country. Countries are reported in the ISO3 format",
    countries = country_codes
  ),
  metadata_created = format(Sys.time(), format = "%Y%m%d:%H%M%S")
)



# Write to File -----------------------------------------------------------
write_pretty_json(
  remove_newlines(qcl_values_metadata),
  metadata_file_name("faostat_qcl")
)




# QV ----------------------------------------------------------------------


qv_values_metadata <- list(
  About = list(
    text = 'The Global Burden of Animal Disease Informatics Metadata provides
           information about how data can be accessed via the GBADs API, as well
           as other relevant information about the data. All metadata presented
           here pertains to the data after it was collected by GBADs
           (i.e. publication date on the GBADs API). This page provides a
           summary of metadata in a user-friendly format. To download the full
           metadata for this dataset select the "download as json-ld" button
           at the top of this page. Information about terms used or to access
           FAOSTAT metadata for this dataset can be found here:',
    metadata_url = with(metadata$qv, metadata_url)
  ),
  Name = "FAOSTAT Value of Agricultural Production (QV)",
  Identifier = with(metadata$qcl, url),
  PublicationDate = "25/07/2022",
  Creator = "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)",
  `Contact Point` = "The Global Burden of Animal Disease Informatics informatics@gbadske.org",
  Description = paste0("Data were accessed via the FAOSTAT Fenix Services bulk download link (see identifier).
                The crop gross value of production in tonnes was used from the  bulk download.
                The data were processed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-crop-values.R).
               The FAOSTAT crop items which are included in this data set are
               those in the CPC Code  groupings from Division 01
               'Products of Agriculture Horticulture and market Gardening'.
               In particular the groupings, Cereals (011), Vegetables (012), Fruits and Nuts (013),
               Oilseeds and oleaginous fruits (015),Edible roots and tubers with high starch or insulin content (015),
               Stimulant, spice and aromatic crops (016), Pulses (dried leguminous vegetables) (017),
               Sugar crops (018).
               For a full list of crops included in the analysis see the table in the
                [documentation for the analysis](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/vignettes/Documentation.Rmd).",
    sprintf(
      " The Livestock value of production was used from this download.
                   The data were processeed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-livestock-values.R).
    The FAOSTAT livestock production items which where included in this dataset are %s (Item Codes: %s)
  ",
      paste(with(
        codes$code_list$vop_items_non_indigenous_codes,
        unique(item[domain_code == "QV"])
      ), collapse = ","),
      paste(with(
        codes$code_list$vop_items_non_indigenous_codes,
        unique(item_code[domain_code == "QV"])
      ), collapse = ",")
    ),
    sep = " "
  ),
  `Statistical Unit` = "LCU,SLC,USD",
  Periodicity = "Annual",
  Licence = "FAO Open Data for Statistical Databases Policy
             [http://www.fao.org/3/ca7570en/ca7570en.pdf](http://www.fao.org/3/ca7570en/ca7570en.pdf)",
  Language = "English",
  `Date Modified` = metadata$qv$last_download,
  Species = paste(with(
    codes$code_list$livestock_codes,
    unique(item[domain_code == "qv"])
  ), collapse = ","),
  Subject = "Economic Value, Crops, Livestock",
  `Temporal Coverage` = list(
    Start = 1994,
    End = 2019
  ),
  `Spatial Coverage` = list(
    text = "Data are reported by country. Countries are reported in the ISO3 format",
    countries = country_codes
  ),
  metadata_created = format(Sys.time(), format = "%Y%m%d:%H%M%S")
)



# Write to File -----------------------------------------------------------
write_pretty_json(
  remove_newlines(qv_values_metadata),
  metadata_file_name("faostat_qv")
)




# PP ----------------------------------------------------------------------

pp_values_metadata <- list(
  About = list(
    text = 'The Global Burden of Animal Disease Informatics Metadata provides
           information about how data can be accessed via the GBADs API, as well
           as other relevant information about the data. All metadata presented
           here pertains to the data after it was collected by GBADs
           (i.e. publication date on the GBADs API). This page provides a
           summary of metadata in a user-friendly format. To download the full
           metadata for this dataset select the "download as json-ld" button
           at the top of this page. Information about terms used or to access
           FAOSTAT metadata for this dataset can be found here:',
    metadata_url = with(metadata$pp, metadata_url)
  ),
  Name = "FAOSTAT Producer Prices (PP)",
  Identifier = with(metadata$pp, url),
  PublicationDate = "25/07/2022",
  Creator = "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)",
  `Contact Point` = "The Global Burden of Animal Disease Informatics informatics@gbadske.org",
  Description = paste0("Data were accessed via the FAOSTAT Fenix Services bulk download link (see identifier).
                All price series where filtered for the FAOSTAT Months Code 7021 (Annual Prices).
                The crop producer prices  were used from the  bulk download.
                The data were processed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-crop-values.R).
               The FAOSTAT crop items which are included in this data set are
               those in the CPC Code  groupings from Division 01
               'Products of Agriculture Horticulture and market Gardening'.
               In particular the groupings, Cereals (011), Vegetables (012), Fruits and Nuts (013),
               Oilseeds and oleaginous fruits (015),Edible roots and tubers with high starch or insulin content (015),
               Stimulant, spice and aromatic crops (016), Pulses (dried leguminous vegetables) (017),
               Sugar crops (018).
               For a full list of crops included in the analysis see the table in the
                [documentation for the analysis](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/vignettes/Documentation.Rmd).",
    sprintf(
      " The Livestock meat live weight prices were used from this dataset.
                   The data were processeed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-livestock-values.R).
    The FAOSTAT livestock meat live items which where included in this dataset are %s (Item Codes: %s)
  ",
      paste(with(
        codes$code_list$meat_live_weight_codes,
        unique(item)
      ), collapse = ","),
      paste(with(
        codes$code_list$meat_live_weight_codes,
        unique(item_code)
      ), collapse = ",")
    ),
    sep = " "
  ),
  `Statistical Unit` = "LCU,SLC,USD",
  Periodicity = "Annual",
  Licence = "FAO Open Data for Statistical Databases Policy
             [http://www.fao.org/3/ca7570en/ca7570en.pdf](http://www.fao.org/3/ca7570en/ca7570en.pdf)",
  Language = "English",
  `Date Modified` = metadata$pp$last_download,
  Species = paste(with(
    codes$code_list$meat_live_weight_codes,
    unique(gsub("Meat live weight, ", "", item))
  ), collapse = ","),
  Subject = "Economic Value, Crops, Livestock",
  `Temporal Coverage` = list(
    Start = 1994,
    End = 2019
  ),
  `Spatial Coverage` = list(
    text = "Data are reported by country. Countries are reported in the ISO3 format",
    countries = country_codes
  ),
  metadata_created = format(Sys.time(), format = "%Y%m%d:%H%M%S")
)



# Write to File -----------------------------------------------------------
write_pretty_json(
  remove_newlines(pp_values_metadata),
  metadata_file_name("faostat_pp")
)


# Aquaculture -------------------------------------------------------------
fao_aqua <- arrow::read_parquet(
  config$data$output$aquaculture_values
)

aqua_metadata_values <- list(
  About = list(
    text = 'The Global Burden of Animal Disease Informatics Metadata provides
           information about how data can be accessed via the GBADs API, as well
           as other relevant information about the data. All metadata presented
           here pertains to the data after it was collected by GBADs
           (i.e. publication date on the GBADs API). This page provides a
           summary of metadata in a user-friendly format. To download the full
           metadata for this dataset select the "download as json-ld" button
           at the top of this page. Information about terms used or to access
           FAO metadata for this dataset can be found here:',
    metadata_url = config$data$source$tables$global_aquaculture_production$url
  ),
  Name = "Fishery and Aquaculture Statistics. Global aquaculture production 1950-2019",
  Identifier = config$data$source$tables$global_aquaculture_production$url,
  PublicationDate = "25/07/2022",
  Creator = "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)",
  `Contact Point` = "The Global Burden of Animal Disease Informatics informatics@gbadske.org",
  Description = "Data were accessed via the FAO  bulk download link (see identifier).
                Species are filtered for the FAO Yearbook Custom Group 'Fish, crustaceans and molluscs, etc.'
   The data were processed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-fao-aquaculture-values.R).
   As the data is currently in current USD ($) values,
   it is converted to 2014-2016 Constant USD by first converting back to LCU ($)
   using a combination of the World Bank indicators
       - PA.NUS.ATLS [https://api.worldbank.org/v2/country/all/indicator/PA.NUS.ATLS?downloadformat=csv]
       - PA.NUS.FCRF [https://api.worldbank.org/v2/country/all/indicator/PA.NUS.FCRF?downloadformat=csv]
  and then re converting to USD using a production weighted exchange rate for the 2014-2016
  base period to determine a mean price per tonne for each item.
  ",
  `Statistical Unit` = "Tonnes,LCU, USD",
  Periodicity = "Annual",
  Licence = "FAO Open Data for Statistical Databases Policy
             [http://www.fao.org/3/ca7570en/ca7570en.pdf](http://www.fao.org/3/ca7570en/ca7570en.pdf)",
  Language = "English",
  `Date Modified` = config$data$source$tables$global_aquaculture_production$last_download,
  Species = "Fish, crustaceans and molluscs, etc",
  Subject = "Economic Value, Aquaculture",
  `Temporal Coverage` = list(
    Start = "1994",
    End = "2019"
  ),
  `Spatial Coverage` = list(
    text = "Data are reported by country. Countries are reported in the ISO3 format",
    countries = with(fao_aqua, unique(iso3_code))
  )
)


# Write to File -----------------------------------------------------------
write_pretty_json(
  remove_newlines(aqua_metadata_values),
  metadata_file_name("fao_global_aquaculture_production")
)



# Output Data -------------------------------------------------------------

informatics_data <- arrow::read_parquet(
  config$data$output$informatics
)

info_metadata_values <- list(
  About = list(
    text = 'The Global Burden of Animal Disease Informatics Metadata provides
           information about how data can be accessed via the GBADs API, as well
           as other relevant information about the data. All metadata presented
           here pertains to the data after it was collected by GBADs
           (i.e. publication date on the GBADs API). This page provides a
           summary of metadata in a user-friendly format. To download the full
           metadata for this dataset select the "download as json-ld" button
           at the top of this page. '
  ),
  Name = "Economic Value of Livestock and Aquaculture",
  Identifier = "",
  PublicationDate = "25/07/2022",
  Creator = "Food and Agriculture Organization of the United Nations Statistical Database (FAOSTAT)",
  `Contact Point` = "The Global Burden of Animal Disease Informatics informatics@gbadske.org",
  Description = "
         This data set contains estimates of the economic value of Livestock,Aquaculture
         and crops asset value and output values on a country by country basis.
         The data were processed using the [code in the GitHub repository](https://github.com/GBADsInformatics/PPSTheme/blob/livestock-value/inst/values/get-info-data.R).
         Note: no asset values are included for Aquaculture.
  ",
  `Statistical Unit` = "2014-2016 Constant USD",
  Periodicity = "Annual",
  Licence = "FAO Open Data for Statistical Databases Policy
             [http://www.fao.org/3/ca7570en/ca7570en.pdf](http://www.fao.org/3/ca7570en/ca7570en.pdf)",
  Language = "English",
  `Date Modified` = "20220603",
  Species = unique(informatics_data$category),
  Subject = "Economic Value, Livestock, Aquaculture, crops",
  `Temporal Coverage` = list(
    Start = min(informatics_data$year),
    End = max(informatics_data$year)
  ),
  `Spatial Coverage` = list(
    text = "Data are reported by country. Countries are reported in the ISO3 format",
    countries = unique(informatics_data$iso3_code)
  )
)


write_pretty_json(
  remove_newlines(info_metadata_values),
  metadata_file_name("economic_value_of_livestock_and_aquaculture")
)
