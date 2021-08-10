#' write_pretty_json
#'
#' Serialises a list to a json file with
#' pretty indentations to make it easier
#' to read
#'
#' @param x the list object to be serialised
#' @param file_name file name of output json
#'
#' @export
#'
#' @examples
#'
#' t <- tempfile()
#' x <- list(a = "a", b = "b")
#' write_pretty_json(x, t)
#' cat(paste(readLines(t), collapse = "\n"))
#'
write_pretty_json <- function(x, file_name) {
  x <- jsonlite::prettify(jsonlite::toJSON(x, pretty = TRUE))
  f <- file(file_name)
  on.exit(close(f))
  writeLines(x, con = f)
}
