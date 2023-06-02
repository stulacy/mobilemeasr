#' Function for promoting a data frame to an \code{sf} object
#'
#' @param df Data frame to be converted to spatial data frame - currently only works for `POINTS`
#'
#' @param latitude Latitude variable in df
#'
#' @param longitude Longitude variable in df
#'
#' @param remove_empty Should rows with missing geometries be removed? Default is FALSE.
#'
#' @param crs Coordinate reference system of df
#'
#' @author Shona Wilde
#'
#' @return `sf` object
#'
#' @export



st_from_df <- function(df, latitude = "lat", longitude = "long", remove_empty = F, crs = 4326){


  if (any(is.na(c(df[, latitude, drop = T], df[, longitude, drop = T]))))

  {

    # df <- df %>%
    #   filter(
    #     across(c(!!latitude, !!longitude),
    #            ~ !is.na(.x)
    #     )
    #   )

    warning("Missing values in coordinates detected...",
            call. = FALSE)

  }

  st <- sf::st_as_sf(
    df,
    coords = c(longitude, latitude),
    crs = crs,
    na.fail = F
  )

  if (remove_empty)
  {
    st <- st %>%
      filter(
        !st_is_empty(geometry)
      )
  }

  return(st)

}



