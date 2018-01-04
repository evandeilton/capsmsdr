#' Earthquak graph with timeline
#'
#' @param mapping aethetics by aes
#' @param data A data.fram
#' @param stat A name from a statistical transfromation
#' @param position Position adjust
#' @param na.rm Remove missing values
#' @param show.legend Show legend, TRUE or FALSE
#' @param inherit.aes Override default aesthetics, TRUE or FALSE
#' @param ... Extra arguments
#'
#' @details A timeline plot earthquaks by dates. Colors represents number of deaths and size is the magnitude of the earthquaks.
#' 
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
#' # Plot
#' signif %>%
#' filter(COUNTRY == 'MEXICO') %>%
#'   ggplot(aes(date = date,
#'              xmin = as.Date('1995-01-01'),
#'              xmax = as.Date('2000-12-30'),
#'              y = COUNTRY,
#'              colour = DEATHS,
#'              fill = DEATHS,
#'              size = EQ_PRIMARY,
#'              location = LOCATION)) +
#'   geom_timeline() +
#'   geom_timeline_label() +
#'   theme(axis.line.y = element_blank(),
#'         axis.line.x = element_line(),
#'         axis.ticks.y = element_blank(),
#'         axis.text.y = element_blank(),
#'         axis.title.y = element_blank(),
#'         legend.position = 'bottom',
#'         panel.grid = element_blank(),
#'         panel.background = element_blank())
#' }
#' @import ggplot2 dplyr grid
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping, 
    data = data,
    stat = stat,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom configuration for ploting timeline
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline",
                                 ggplot2::Geom,
                                 required_aes = c("date", "xmin", "xmax", "y", "colour", "size"),
                                 default_aes = ggplot2::aes(shape = 20, stroke = .4, alpha = .6),
                                 draw_key = function(data, params, size) {
                                   grid::pointsGrob(0.5, 0.5,
                                                    pch = data$shape,
                                                    gp = grid::gpar(fontsize = data$size * .pt + data$stroke * .stroke / 3)
                                   )
                                 },
                                 draw_panel = function(data, panel_scales, coord) {
                                   # prepare data
                                   ## subset by xmin and xmax
                                   data <- data %>%
                                     dplyr::filter(date >= xmin, date <= xmax) %>%
                                     dplyr::mutate(x = as.numeric(date))

                                   coo <- coord$transform(data, panel_scales)
                                   
                                   grid::grobTree(
                                     grid::pointsGrob(
                                       coo$x, coo$y,
                                       pch = coo$shape,
                                       gp = grid::gpar(col = alpha(coo$colour, coo$alpha),
                                                       fill = alpha(coo$fill, coo$alpha),
                                                       fontsize = coo$size * .pt + coo$stroke * .stroke / 3)
                                     ),
                                     grid::segmentsGrob(
                                       x0 = unit(coo$xmin, 'npc'),
                                       x1 = unit(coo$xmax, 'npc'),
                                       y0 = unit(coo$y, 'npc'),
                                       y1 = unit(coo$y, 'npc'),
                                       gp = grid::gpar(col = 'gray')
                                     )
                                   )
                                 }
)

#' Label for timeline
#'
#' @param mapping Aethetics by aes.
#' @param data A data.fram
#' @param stat A name from a statistical transfromation
#' @param position Position adjustment.
#' @param na.rm Remove missing values
#' @param show.legend Show legend, TRUE or FALSE
#' @param inherit.aes Override default aesthetics, TRUE or FALSE
#' @param ... Extra arguments
#' 
#' @details Labels for the locations of earthquaks.
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
#' # Plot
#' signif %>%
#' filter(COUNTRY == 'MEXICO') %>%
#'   ggplot(aes(date = date,
#'              xmin = as.Date('1995-01-01'),
#'              xmax = as.Date('2000-12-30'),
#'              y = COUNTRY,
#'              colour = DEATHS,
#'              fill = DEATHS,
#'              size = EQ_PRIMARY,
#'              location = LOCATION)) +
#'   geom_timeline() +
#'   geom_timeline_label() +
#'   theme(axis.line.y = element_blank(),
#'         axis.line.x = element_line(),
#'         axis.ticks.y = element_blank(),
#'         axis.text.y = element_blank(),
#'         axis.title.y = element_blank(),
#'         legend.position = 'bottom',
#'         panel.grid = element_blank(),
#'         panel.background = element_blank())
#' }
#' @import dplyr grid ggplot2
#' @export
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity", 
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,  
    data = data, 
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Geom for timeline labels
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimeline",
                                      ggplot2::Geom,
                                      required_aes = c("date", "xmin", "xmax", "location"),
                                      draw_panel = function(data, panel_params, coord) {
                                        data <- data %>%
                                          dplyr::filter(date >= xmin, date <= xmax) %>%
                                          dplyr::mutate(x = as.numeric(date))
                                        
                                        coo <- coord$transform(data, panel_params)

                                        txt <- grid::textGrob(label = coo$location,
                                                              x = unit(coo$x, 'npc'),
                                                              y = unit(coo$y + .2, 'npc'),
                                                              hjust = 0,
                                                              rot = 45,
                                                              gp = grid::gpar(fontsize = 8))
                                        segments <- grid::segmentsGrob(x0 = unit(coo$x, 'npc'),
                                                                       x1 = unit(coo$x, 'npc'),
                                                                       y0 = unit(coo$y, 'npc'),
                                                                       y1 = unit(coo$y + .15, 'npc'),
                                                                       gp = grid::gpar(col = 'gray'))
                                        grid::grobTree(txt, segments)
                                      }
)


#' Extra Theme to use with geom_timeline
theme_timeline <- ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                                 axis.line.x = ggplot2::element_line(),
                                 axis.ticks.y = ggplot2::element_blank(),
                                 axis.text.y = ggplot2::element_blank(),
                                 axis.title.y = ggplot2::element_blank(),
                                 legend.position = 'bottom',
                                 panel.grid = ggplot2::element_blank(),
                                 panel.background = ggplot2::element_blank())
