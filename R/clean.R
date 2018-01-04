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


#' NOOA 's earthquake data set
#'
#' The Significant Earthquake Database contains information on destructive earthquakes from 2150 B.C. to the present that meet at least one of the following criteria: Moderate damage (approximately $1 million or more), 10 or more deaths, Magnitude 7.5 or greater, Modified Mercalli Intensity X or greater, or the earthquake generated a tsunami.
#' 
#' @source  \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#' @format A data frame with large number of columns
#' @examples
#' \dontrun{
#' earthquake
#' }
"earthquake"
