#' Create labels for earthquak data
#'
#' @param data A data.frame.
#'
#' @return A column of earthquak annotation; locaion, magnitude and deaths. Removes NA.
#' @examples
#' \dontrun{
#' # Read and clean data
#' 
#' fp <- system.file("exdata", "earthquake.csv", package = "capsmsdr")
#' 
#' signif <- read_tsv(fp) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # plot
#' 
#' signif %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import dplyr
#' @export
eq_create_label <- function(data) {
  data %>%
  dplyr::mutate(location = ifelse(is.na(LOCATION), '', paste("<b>Location: </b>", LOCATION, "<br>")),
                magnitude = ifelse(is.na(EQ_PRIMARY), '', paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br>")),
                deaths = ifelse(is.na(DEATHS), '', paste("<b>Total deaths: </b>", DEATHS))) %>%
    dplyr::rowwise() %>%
    do(popup_text = paste(.$location, .$magnitude, .$death)) %>%
    unlist(recursive = FALSE) %>%
    as.character
}

#' Map earthquak data
#'
#' @param data A data.frame
#'
#' @return A plot with info from location, magnitude and deaths.
#' @examples
#' \dontrun{
#' # Read and clean data
#' fp <- system.file("exdata", "earthquake.csv", package = "capsmsdr")
#' signif <- read_tsv(fp) %>%
#'   eq_clean_date %>%
#'   eq_clean_location
#'
#' # Plot
#' 
#' signif %>%
#' filter(COUNTRY == 'MEXICO' & year(date) >= 2000) %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map()
#' }
#' @import leaflet
#' @export
eq_map <- function(data) {
  leaflet::leaflet(data) %>%
    leaflet::addProviderTiles(providers$CartoDB) %>%
    leaflet::addCircleMarkers(~LONGITUDE, ~LATITUDE,
                     popup = ~popup_text,
                     labelOptions = labelOptions(direction = 'bottom'),
                     radius = 5)
}
