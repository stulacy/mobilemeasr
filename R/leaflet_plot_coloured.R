#' Function to plot coloured geometries on a leaflet map
#'
#' This function is a wrapper around \code{\link[sspatialr]{plot_sf_leaflet}} that allows geometries to be coloured according to a variable.
#' Discrete and continuous scales are supported.
#'
#' @param st Object with class \code{sf} or \code{sfc}.
#'
#' @param colour_by Variable to colour geometries by.
#'
#' @param palette Colours that values will be mapped to.
#'
#' @param popup Should interactive popups be included in the map if possible? 
#'
#' @return Leaflet map/plot.
#'
#' @author Shona Wilde
#'
#' @seealso \code{\link[gissr]{leaflet_plot}}, \code{\link[leaflet]{colorNumeric}}
#'
#' @export

leaflet_plot_coloured <- function(st, colour_by = "value", palette = "plasma", popup = T) {
  
  # define variables to colour by
  colour_var <- sym(colour_by)
  
  domain <- st %>%
    pull(!!colour_var)
  
  
  # set palette based on continuous or discrete variables
  if (class(domain) %in% c("integer", "numeric"))
    
  {
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = domain
    )
    
  }
  
  else {
    
    pal <- leaflet::colorFactor(
      palette = palette,
      domain = domain,
      na.color = "gray50"
    )
    
  }
  
  # build plot with legend
  plot <- plot_sf_leaflet(
    st,
    colour = ~pal(domain),
    popup = popup
  )  %>%
    leaflet::addLegend(
      "bottomright",
      pal = pal,
      values = ~domain,
      title = colour_by,
      opacity = 1
    )
  
  
  return(plot)
  
}
