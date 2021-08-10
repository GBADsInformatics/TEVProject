#' iso_date
#'
#' @return returns data in YYYYMMDD format
#' @export
#'
#' @examples
#' iso_date()
iso_date <- function() {
  format(Sys.Date(), format = "%Y%m%d")
}


file_name <- function(source, tags,
                      extension = ".parquet",
                      output_dir = here::here("data", "output")) {
  base_dir <- file.path(output_dir, source)
  if (!dir.exists(base_dir)) {
    dir.create(base_dir)
  }
  file.path(base_dir, paste0(
    source, "_",
    paste(tags, collapse = "_"), extension
  ))
}




#' get_gbads_file
#'
#' @param str regex string to match file name
#' @param dir directory of gbads file
#'
#' @return the full path the the appropriate name
get_gbads_file <- function(str, dir) {
  files <- list.files(dir)
  file_match <- grep(str, files, value = TRUE, fixed = TRUE)
  return(file.path(dir, sort(file_match, decreasing = TRUE)[1]))
}





#' clean_countries
#'
#' Cleans the countries imported from FAOSTAT by removing multiple versions
#' of China for any one year and removing countries which invalid ISO3 codes
#'
#' @param df a dataframe containing the correct columns
#' @param code_col column name containing faostat codes
#' @param year_col column name containing years as %Y format
#' @param value_col column name containing the faostat values
#'
#' @return dataframe with double counted and missing values removed
#' @export
#'
clean_countries <- function(df,
                            code_col = "area_code",
                            year_col = "year",
                            value_col = "value") {
  names(df)[names(df) == code_col] <- "FAOST_CODE"

  countries <- FAOSTAT::FAOcountryProfile %>%
    dplyr::select(FAOST_CODE, ISO3_CODE)

  df %>%
    FAOSTAT::FAOcheck(
      var = value_col,
      year = year_col,
      data = .,
      type = "multiChina"
    ) %>% # Check for multiple China entries
    tidyr::drop_na(value) %>% # Remove missing values
    dplyr::left_join(countries, by = "FAOST_CODE") %>%
    tidyr::drop_na(ISO3_CODE) %>% # Remove countries without valid ISO3_CODES according to FAO
    dplyr::mutate(
      area = plyr::mapvalues(
        area, c("China, mainland"),
        c("China")
      )
    ) %>%
    dplyr::relocate(ISO3_CODE, .before = everything()) %>%
    janitor::clean_names()
}


#' sanitize_columns
#'
#' this function sanitizes character columns by removing unusual characters
#' and turns selected colums to snakecase
#'
#' @param df a dataframe
#' @param exclude character vector containing which column names will be excluded
#'
#' @return a dataframe which only has ASCII characters in its character columns
#' @export
#'
#' @examples
#' sanitize_columns(data.frame(a = "a"))
sanitize_columns <- function(df, exclude = c("iso3_code")) {
  character_cols <- sapply(df, class)
  character_cols <- names(character_cols)[character_cols == "character"]
  df |>
    dplyr::mutate_at(
      setdiff(character_cols, exclude),
      ~ snakecase::to_any_case(stringi::stri_enc_toascii(.x))
    )
}


# -------------------------------------------------------------------------

#' get_livestock_codes
#'
#' Returns the FAO item codes which will be used in the
#' valuation of livestock types
#'
#' @param code_dir directory containing rds files of FAOSTAT item codes
#'
#' @return a vector containing the livestock codes used
#' @export
get_livestock_codes <- function(code_dir = config$data$codes$faostat$dir,
                                write_codes = TRUE) {

  # FAOSTAT Item Codes to Use for each category that will be used
  code_rds_files <- list.files(
    config$data$codes$faostat$dir,
    full.names = TRUE,
    pattern = "*.rds$"
  )

  # Name files
  names(code_rds_files) <- basename(code_rds_files) |>
    stringr::str_remove_all(".rds") |>
    stringr::str_remove_all("FAOSTAT_")

  # Read in the item code data
  faostat_item_codes <- purrr::map(
    code_rds_files,
    ~ readRDS(.x) |>
      janitor::clean_names()
  )

  names(faostat_item_codes) <- names(faostat_item_codes) |>
    janitor::make_clean_names()

  # Names of RDS files
  rds_names <- c(
    "livestock_codes",
    "vop_items_non_indigenous_codes",
    "meat_live_weight_codes",
    "livestock_meat_item_codes"
  )

  if (length(setdiff(rds_names, names(faostat_item_codes))) > 0) {
    stop("Incorrect code RDS file names")
  }

  # - Unique Livestock Codes that will be used
  livestock_codes <- unique(
    c(
      faostat_item_codes$livestock_codes$item_code,
      faostat_item_codes$vop_items_non_indigenous_codes$item_code,
      faostat_item_codes$meat_live_weight_codes$item_code,
      faostat_item_codes$livestock_meat_item_codes$item_code
    )
  )

  # Save the current version of codes used
  if (write_codes) {
    write.csv(
      faostat_item_codes$item_codes |>
        dplyr::filter(
          item_code %in% livestock_codes,
          domain_code %in% c("QCL", "QV", "PP")
        ),
      file.path(
        code_dir,
        "livestock_item_codes.csv"
      ),
      row.names = FALSE
    )
  }

  return(
    list(
      codes = livestock_codes,
      code_list = faostat_item_codes
    )
  )
}
