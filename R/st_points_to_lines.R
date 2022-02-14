#' St points to lines
#'
#' Wrapper to cast a spatial points object to lines
#'
#' @param st Object with class \code{sf} or \code{sfc}. The geometry type must be 'POINTS'
#'
#' @param group_by Optional grouping variable
#'
#' @param do_union Should geometries be created by unioning using st_union, or simply by combining using st_combine?
#'
#' @author Shona Wilde
#'
#' @return Sf object with LINESTRING geometries
#'
#' @export



st_points_to_lines <- function(st, group_by = NULL, do_union = F) {


  if(length(group_by) > 0) {

    group_vars <- syms(group_by)

    st_lines <- st %>%
      group_by(!!!group_vars) %>%
      summarise(do_union = do_union) %>%
      sf::st_cast("LINESTRING")

  }

  else {

    st_lines <- st %>%
      summarise(do_union = do_union) %>%
      sf::st_cast("LINESTRING")

  }

  return(st_lines)

}
