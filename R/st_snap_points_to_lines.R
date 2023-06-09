#' Function to "snap" spatial points to the nearest spatial line.
#'
#' @param x \code{sf} POINTS object 
#' 
#' @param y \code{sf} LINES object to snap points to.
#' 
#' @param max_dist The maximum distance allowed between a point and the nearest line. Avoids snapping points to lines far away.
#'
#' @return \code{sf} object.
#' 
#' @export
#'
#' @author Shona Wilde

st_snap_points_to_lines <- function(x, y, max_dist = 1000) {
  

  # split geometries into list
  geom <- st_geometry(x) %>%
    st_as_sf() %>% 
    rowid_to_column() %>% 
    split(.$rowid)
  
  # apply function
  st_snap <- map_dfr(
      geom,
      ~st_nearest_point_on_line(
        .x, 
        y, 
        max_dist = max_dist
      )
  )
  
  # overwrite geometry column
  x$snap_dist <- st_snap$snap_dist
  
  x$geometry <- st_snap$geometry
  
  
  return(x)  
  
}



st_nearest_point_on_line <- function(x, y, max_dist = 1000) {

  
  # get nearest point on each road
  nearest <- st_nearest_points(x, y)
  
  # find distances - use length as st_nearest length returns a linestring
  nearest_length <- st_length(nearest)
  
  # find shortest line
  min_dist <- min(nearest_length)
  
  # get index of closest line
  min_index <- which.min(nearest_length)
  
  # return geometry
  if (as.vector(min_dist) > max_dist) {
    
    geometry <- st_geometry(x)
    
  }
  
  else {
  # extract nearest point on nearest road
  geometry <- st_cast(
    nearest[min_index],
    "POINT"
  )[2]
  
  }
  
  # as spatial
  st <- geometry %>% 
    st_sf() %>% 
    mutate(
      snap_dist = as.vector(min_dist)
    )
    
  return(st)
  
}


