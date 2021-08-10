#' World Health Population Dataset
#'
#' World Bank population indicator `SP.POP.TOTL`.
#'
#'
#' @format ## `world_bank_population`
#' A data frame with 16,123 rows and 15 columns:
#' \describe{
#'   \item{country_code}{ISO3 Country code}
#'   \item{year}{1960-2020}
#'   \item{population}{numeric population numeber}
#'   \item{url}{download url}
#'   \item{source_dir}{location where the raw source data is downloaded}
#'   \item{source_typ}{type of source file (e.g., zip, tar)}
#'   \item{last_download}{data of last download in YYYYMMDD format}
#'   \item{region}{Countries World Bank Region}
#'   \item{income_group}{Countries World Bank Income Classification}
#'   \item{special_notes}{World Bank metadata notes on each country}
#'   \item{table_name}{}
#'   \item{indicator_code}{World Bank Indicator Code}
#'   \item{indicator_name}{World Bank Indicator Name}
#'   \item{source_note}{Additional notes on the data}
#' }
#' @source <https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?downloadformat=csv>
"world_bank_population"
