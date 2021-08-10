#!/usr/bin/Rscript --vanilla

###################################################
# nolint start
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
# Description: This file takes as input the current
# FAOSTAT Country Codes and produces a subset of these
# codes based which are used for all subsequent analysis
#
#  Because of the importance of the more recent values in this
#  analysis, the ISO3 codes that will be used are the current set
#  197 member states of the United Nations.  This is done to avoid any
#  sort of change in allocation/double counting.
#
# - Note: these codes contain multiple Chinese regions
#   which are subset to make sure that china is not double
#   counted. China will always be mapped to CHN
#
# The input code file is contained in
## - data/codes/FAOSTAT_Country_Codes.Rds
#
# Output:
## - data/output/codes/faostat_iso3_country_codes.parquet
#
# These locations are contained in the project configuration file
# - conf/config.yml
# and the dependencies are contained within the Makefile recipe
# to rebuild the output file.
# nolint end 
####################################


# Activate Project --------------------------------------------------------

renv::activate(project = ".")
logging::basicConfig()

# Load Configuration ------------------------------------------------------

config <- config::get()



# Import Raw FAOSTAT Country Codes as RDS ---------------------------------
country_codes_config <- config$data$codes$faostat$country_codes

# Input and output locations
input_country_codes_file <- normalizePath(country_codes_config$raw_codes)
output_country_codes_file <- country_codes_config$output_codes


# Create a directly to store the outputs
# if it does not currently exist
if (dir.exists(here::here(dirname(output_country_codes_file)))) {
  logging::loginfo("Creating output directory")
  dir.create(here::here(dirname(output_country_codes_file)))
} else {
  logging::loginfo("Output directory already exists")
}

logging::loginfo("Subsetting FAOSTAT Country Codes")

iso3_codes <-
  input_country_codes_file |>
  readr::read_csv() |>
  janitor::clean_names() |>
  dplyr::distinct(country, iso3_code, .keep_all = TRUE)





# Subset for Countries, including former countries ------------------------

# Since 1994, only Netherlands Antilles, Sudan, Belgium-Luxembourg
# and Serbia and Montenegro have been removed from the country classification
# system.

# Remove any statistical areas which do not have an M49 code
#  - as they need to match any UN classifications which are used later
#
# Drop any region which has no iso2_code and is NA for both the end or start
# year recorded

iso3_codes <- iso3_codes |>
  tidyr::drop_na(m49_code) |>
  dplyr::filter(
    !(is.na(iso2_code) & is.na(end_year) & is.na(start_year)),
    (is.na(end_year) | end_year > 1994)
  )

#
# Filter out the following countries/regions
# as they are too small/ do not have
# data reported in 99% of cases/
# will overley current regions that are
# being reported.
#
exclude_regions <- c(
  # Small Island nations/territories
  "Aland Islands" = "ALA",
  "Antartica" = "ATA",
  "Bouvet Island" = "BWA",
  "British Virgin Islands" = "VGB",
  "British Indian Ocean Territory" = "IOT",
  "Bonaire, Sint Eustatius and Saba" = "BES",
  "Caymen Islands" = "CYM",
  "Christmas Island" = "CXR",
  "Cocos (Keeling) Islands" = "CCK",
  "Cook Islands" = "COK",
  "Falkland Islands (Malvinas)" = "FLK",
  "French Southern Territories" = "ATF",
  "Gibraltar" = "GIB",
  "Heard and McDonald Islands" = "HMD",
  "Holy See" = "VAT",
  "Isle of Man" = "IMN",
  "Johnston Island" = "JTN",
  "Martinique" = "MTQ",
  "Mayotte" = "MYT",
  "Midway Island" = "MID",
  "Montserrat" = "MSR",
  "Norfolk Island" = "NFK",
  "Northern Mariana Islands" = "MNP",
  "Pitcairn" = "PCN",
  "Puerto Rico" = "PRI",
  "Saint Barthélemy" = "BLM",
  "Saint Helena, Ascension and Tristan da Cunha" = "SHN",
  "Saint Pierre and Miquelon" = "SPM",
  "Saint-Martin (French part)" = "MAF",
  "Seychelles" = "SYC",
  "Sint Maarten (Dutch part)" = "SXM",
  "South Georgia and the South Sandwich Islands" = "SGS",
  "Svalbard and Jan Mayen Islands" = "SJM",
  "Turks and Caicos Islands" = "TCA",
  "United States Minor Outlying Islands" = "UMI",
  "United States Virgin Islands" = "VIR",
  "Wake Island" = "WAK",
  "Wallis and Futuna Islands" = "WLF",
  "Western Sahara" = "ESH",
  "Netherlands Antilles (former)" = "ANT"
)

iso3_codes <- dplyr::filter(iso3_codes, !(iso3_code %in% exclude_regions))


# #
# Match countries which have ended in 1994-present
# and assign an area weighting to the new areas
#
# This weighting is determined by the countries
# current surface area as defined in AG.SRF.TOTL.K2
#
# TODO: add in the country weighting
ended_countries <-
  tribble(
    ~m49_code, ~country, ~end, ~iso_1, ~w_1, ~iso_2, ~w_2,
    "058", "Belgium-Luxembourg", 1999, "LUX", , "BEL", ,
    "891", "Serbia and Montenegro", 2005, "SRB", , "MNE", ,
    "736", "Sudan", 2011, "SSD", , "SDN", ,
  )

# Set the China Countrycode to CHN
iso3_codes$iso3_code[iso3_codes$country == "China, mainland"] <- "CHN"

# Write to File ----------------------------------------------------------

# Write to parquet
arrow::write_parquet(
  x = iso3_codes,
  sink = here::here(output_country_codes_file)
)


# Write summary results to zip file
output_results_dir <- file.path(
  dirname(output_country_codes_file),
  sprintf(
    "country_codes_%s",
    format(Sys.Date(), format = "%Y%m%d")
  )
)

dir.create(output_results_dir, recursive = TRUE)


# Write to CSV
readr::write_csv(
  iso3_codes,
  file = here::here(output_results_dir, "country_codes.csv")
)

# Generate a Kable
source(here::here("R", "table-functions.R"))
generate_kbl(
  df = iso3_codes |> dplyr::select(iso3_code, country),
  col_names = c(
    "Current ISO3 Code",
    "Country"
  ),
  caption = "ISO3 Country Codes Used",
  header_spec_fun = function(x) x,
  footnotes = c(
    "Source: FAOSTAT (URL:https://www.fao.org/faostat/en/#definitions)"
  ),
  output_name = file.path(
    output_results_dir,
    "livestock_value_country_codes.pdf"
  )
)

logging::loginfo("Zipping results directory %s", output_results_dir)
zip(
  zipfile = paste0(output_results_dir, ".zip"),
  files = list.files(output_results_dir, full.names = TRUE),
  flags = "-j"
)

# Remove the temporary output directory
unlink(output_results_dir, recursive = TRUE)


logging::loginfo("Exit")
