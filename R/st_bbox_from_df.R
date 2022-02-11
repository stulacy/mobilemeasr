#' Function for promoting a data frame containing bbox coordinates to a spatial data frame
#'
#' @param df Data frame containing bbox boundaries in columns. Order of columns must be xmin, ymin, xmax, ymax
#'
#' @param crs Coordinate reference system of df
#'
#' @author Shona Wilde
#'
#' @return `sf`object containing POLYGON of bounding box
#'
#' @export


st_bbox_from_df <- function(df, crs = 4326) {

  lat <- c(df[[2]], df[[4]])
  lon <- c(df[[1]], df[[3]])

  df_coords <- tibble(lat, lon)  %>%
    st_as_sf(
      coords = c("lon", "lat")
    ) %>%
    st_set_crs(4326)

  st_bbox <- df_coords %>%
    st_get_boundary()

  return(st_bbox)

}



