#' Function to arrange a set of points
#'
#' \code{st_arrange_points} can be used to arrange a set of points/coordinates in either a clockwise or anticlockwise direction or in ascending order according
#' to their latitude or longitude.
#'
#' @param st Object of class \code{sf}.
#'
#' @param direction Direction to order points in. Options are 'clockwise', 'anticlockwise', 'x', or 'y'.
#'
#' @author Shona Wilde
#'
#' @return \code{st} with ordered geometry.
#'
#' @export

st_arrange_points <- function(st, direction = "clockwise"){

  # check class
  stopifnot(
    "class must be 'sf' & geometry must be of type 'sfc_POINT'..." = inherits(st, "sf") && inherits(sf::st_geometry(st), "sfc_POINT")
  )

  stopifnot(
    "Direction must be one of 'clockwise', 'anticlockwise', 'x' or 'y'" = direction %in% c("clockwise", "anticlockwise", "x", "y")
  )


  # get crs of object
  crs <- st_crs(st)

  # as data frame
  df <- st %>%
    mobilemeasr::sfc_to_columns(
      names = c("x", "y"),
      drop_geometry = T
      )

  # find centre to arrange around
  df_angle <- df %>%
    mutate(
      centre_x = mean(x),
      centre_y = mean(y),
      angle = atan2(y-centre_y, x-centre_x)
    )

  # arrange by angle
  if (direction == "clockwise") {

    df_sort <- df_angle %>%
      arrange(
        desc(angle)
      )

  } else if (direction == "anticlockwise") {

    df_sort <- df_angle %>%
      arrange(
        angle
      )

  } else if (direction == "x") {

    df_sort <- df_angle %>%
      arrange(x, y)

  } else if (direction == "y") {

    df_sort <- df_angle %>%
      arrange(y, x)
  }

  # drop temp vars and convert back to spatial data frame
  st_sort <- df_sort %>%
    select(
      -centre_x,
      -centre_y,
      -angle
    ) %>%
    mobilemeasr::st_from_df(
      latitude = "y",
      longitude = "x",
      crs = crs
    )

  return(st_sort)


}



