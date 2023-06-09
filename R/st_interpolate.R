#' Function to interpolate points along a line geometry
#'
#' Return points at equal distances along a line.
#'
#' @param st Object with class \code{sf} or \code{sfc}. The geometry type must be a 'LINESTRING'.
#'
#' @param distance Distance between points in m.
#'
#' @param crs Projection of output. Default is to convert to \emph{WGS 84}.
#'
#' @author Shona Wilde
#'
#' @return An object of the same class as \code{st}, with manipulated geometry.
#'
#' @export

st_interpolate <- function(st, distance, crs = 4326) {

  # some checking for line geometry
  if (any(!str_detect(st_geometry_type(st), "LINESTRING")))
    stop("Geometry type must be a 'LINESTRING'")

  # projection system must in meters
  if (!grepl("+units=m", st_projection(st)))
    {

    st <- st_transform(st, "+proj=moll")

  }

  # calculate length of line
  length <- as.numeric(st_length(st))

  # convert to SpatialLines object
  sp <- as_Spatial(st)

  # interpolate
  message("Creating points every ", distance, " m...")

  suppressWarnings(

    sp_points <- rgeos::gInterpolate(
      sp,
      seq(0, length, by = distance),
      normalized = F
    )

  )

  # back to st
  st_points <- sp_points %>%
    st_as_sf() %>%
    st_transform(crs = crs)

  # bind original data slot if there is one
  if ("data" %in% slotNames(sp)) {

    st_join <- st_points %>%
      bind_cols(sp@data) %>%
      rowid_to_column("point_id")
  }

  else {

    # just add points IDs
    st_join <- st_points %>%
      rowid_to_column("point_id")

  }

  return(st_join)


}



