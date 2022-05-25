#' Covert a coordinate pair to a point
#'
#' @param latitude Latitude
#' @param longitude Longitude
#' @param crs Coordinate reference system of supplied latitude and longitude
#'
#' @return \code{sf} object with POINT geometry
#'
#' @author Shona Wilde
#'
#' @export
st_coords_as_point <- function(latitude, longitude, crs = 4326) {

  coords <- c(longitude, latitude)

  st_point <- st_point(coords) %>%
    st_sfc(crs = crs) %>%
    st_as_sf() %>%
    rename(geometry = x)

  return(st_point)
}

