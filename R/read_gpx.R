#' Read GPX
#'
#' Read a GPX file into a spatial data frame
#'
#' @param file File to read. Must have a .gpx extension
#' @param lines Convert point geometries to lines?
#'
#' @return \code{sf} data frame
#'
#' @author Shona Wilde
#' @export


read_gpx <- function(file, lines = T){

  gpx <- plotKML::readGPX(file)

  df <- gpx$tracks[[1]] %>%
    pluck(1) %>%
    tibble()

  st <- df %>%
    st_from_df(longitude = "lon")

  if (lines) {

    st <- st_points_to_lines(st)
  }

  return(st)

}





