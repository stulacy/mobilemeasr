#' Function to combine multiple linestrings,
#'
#' @param st \code{sf} LINES object.
#'
#' @return \code{sf} object containing a single LINESTRING.

#' @export
#'
#' @author Shona Wilde

st_combine_lines <- function(st) {
  
  st_points <- st %>% 
    st_union() %>% 
    st_cast("POINT") %>% 
    st_as_sf() %>% 
    rename(geometry = x) 

  bbox <- st_bbox(st_points)
  
  x_range <- bbox[3] - bbox[1]
  y_range <- bbox[4] - bbox[2]
  
  direction <- if_else(
    x_range > y_range, "x", "y"
  )
  
  st_line <- st_points %>% 
    st_arrange_points(direction = direction) %>% 
    rowid_to_column("point_id") %>% 
    st_points_to_lines()
  
  return(st_line)
  
}


