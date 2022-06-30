#' Function to average groups of linestrings
#' 
#' Calculate the average path of linestrings. Designed for use with the road network where a single named road may be 
#' made up of multiple linestrings.
#'
#' @param st \code{sf} object containing LINESTRING geometries.
#' 
#' @param distance Resolution to perform interpolation at, in m.
#' 
#' @param crs Projection of output. Default is to convert to WGS 84.
#'
#' @return \code{sf} object containing summarised geometries.
#' 
#' @author Shona Wilde
#' 
#' @export

st_average_linestring <- function(st, distance = 5, crs = 4326) {
  
  # to points
  st_points <- st %>% 
    st_union() %>% 
    st_as_sf() %>% 
    st_interpolate(distance = distance) %>% 
    sfc_to_columns(drop_geometry = T) 
    
  # averaging
  if (diff(range(st_points$long)) > diff(range(st_points$lat))) {
    
    st_avg <- st_points %>% 
      mutate(long = round_any(long, accuracy = 0.001)) %>% 
      group_by(long) %>% 
      summarise(lat = mean(lat)) 
  }
  
  else {
    
    st_avg <- st_points %>% 
      mutate(lat = round_any(lat, accuracy = 0.001)) %>% 
      group_by(lat) %>% 
      summarise(long = mean(long)) 
    
  }
  
  st_lines <- st_avg %>% 
    st_from_df(crs = crs) %>% 
    st_points_to_lines() 
  
  return(st_lines)

}



