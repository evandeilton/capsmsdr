#' Clean dates from earthquak data
#'
#' @param data A data.frame.
#'
#' @return The input data.frame plus a date column.
#' @import dplyr
#' @examples
#' \dontrun{
#' fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
#' signif <- read_delim(fp, delim = "\t") %>%
#'   eq_clean_date %>%
#' }
#' @export
eq_clean_date <- function(data) {
  data %>%
    dplyr::mutate(DATE = paste(YEAR, MONTH, DAY, ''),
                  DATE = lubridate::ymd(DATE))
}

#' Clean locations from earthquak data
#'
#' @description This function use some fnctions from stringr package to fix data fom location names in original data.frame.
#' 
#' @param data A data.frame.
#' 
#' @return The input data.fram plus a location column
#' @examples
#' \dontrun{
#' fp <- system.file("data", "earthquake.csv", package = "capsmsdr")
#' signif <- read_delim(fp, delim = "\t") %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#' }
#' @import stringr dplyr
#' @export
eq_clean_location <- function(data) {
  data %>%
    dplyr::mutate(LOCATION = stringr::str_to_title(LOCATION_NAME),
                  LOCATION = stringr::str_split(LOCATION_NAME, ': ', simplify = TRUE)[,2])
}
