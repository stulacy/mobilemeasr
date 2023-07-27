#' Haversine distance
# Function to calculate haversine distance using a simplified formula
#'
#' @param long_1 Start longitude
#' @param lat_1 Start latitude
#' @param long_2 End longitude
#' @param lat_2 End latitude
#'
#' @return Numeric value
#'
#' @author Shona Wilde & David Carslaw
#'
#' @export

haversine_distance <- function(long_1, lat_1, long_2, lat_2) {

  dist <-
    acos(
      sin(lat_1 * pi / 180) * sin(lat_2 * pi / 180) + cos(lat_1 * pi / 180) *
        cos(lat_2 * pi / 180) * cos(long_2 * pi / 180 - long_1 * pi / 180)
    ) * 6371000

  return(dist)

}
