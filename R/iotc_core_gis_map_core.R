#' Plots a (ggplot) map of the IO and (optionally) an overlay of data visualization artifacts
#'
#' @param xlim A vector with the min - max longitude values
#' @param ylim A vector with the min - max latitude values
#' @param fill_color The fill color used to draw the map
#' @param border_color The border color used to draw the map
#' @param show_grids Flag to display reference grids every 20 degrees (both horizontally and vertically)
#' @param show_IO_areas Flag to display the two main IO areas (F51 and F57) as a separate layer
#' @param show_EEZs Flag to display the boundary lines of the EEZs
#' @param show_high_seas Flag to display the boundary lines of the high seas
#' @param content_drawer an (optional) function to plot an overlay of data visualization artifacts
#' @param draw_content_first Flag to force plotting the content (through \code{content_drawer} when provided) before or after the other layers (grids, IO areas and EEZs)
#' @return A map with the features enabled by the flags and parameters above, with an optional overlay of data visualization artifacts of any kind
#' @export
IO_map = function(xlim = IO_map_xlim,
                  ylim = IO_map_ylim,
                  fill_color   = "black",
                  border_color = "black",
                  show_grids = TRUE,
                  show_IO_areas = TRUE,
                  show_EEZs = FALSE,
                  show_high_seas = TRUE,
                  content_drawer = NULL,
                  draw_content_first = TRUE) {

  draw_content = !is.null(content_drawer);

  world = map_data("world")

  map =
    ggplot() +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )

  if(!show_grids) {
    map = map +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }

  if(draw_content & draw_content_first) { map = content_drawer(map) }

  sf_border_color = "grey" #darken("grey", .5)
  io_border_color = darken(sf_border_color, .4)

  sf_fill_color   = "transparent"

  if(show_EEZs)
    map = map +
    geom_sf(data = iotc.core.gis.wkt::sf_by_code("IRALLEZ"),
            color = sf_border_color,
            fill  = sf_fill_color)

  if(show_high_seas) {
    map = map +
      geom_sf(data = iotc.core.gis.wkt::sf_by_code("IRALLHS"),
              color = sf_border_color,
              fill  = sf_fill_color)
  }

  if(show_IO_areas) {
    #Defaults on using 'black' as border color for the IO regions
    map = map +
      geom_sf(data = iotc.core.gis.wkt::sf_by_code("IRFAO51"),
              fill = sf_fill_color) +
      geom_sf(data = iotc.core.gis.wkt::sf_by_code("IRFAO57"),
              fill = sf_fill_color)
  }

  # See: https://github.com/tidyverse/ggplot2/issues/2799
  c_sf = coord_sf(xlim = xlim,
                  ylim = ylim)

  c_sf$default = TRUE

  map = map +
    geom_map(data = world, map = world,
             aes(map_id = region),
             fill  = fill_color,
             color = border_color) +
    c_sf

  if(draw_content & !draw_content_first) { map = content_drawer(map) }

  return(map)
}
