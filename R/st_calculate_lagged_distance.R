#' Function to calculate distance between points
#'
#' @param st Object of class \code{sf}.
#' 
#' @param name Name of new column.
#' 
#' @param as_numeric Should the new column be numeric? Default is TRUE.
#' 
#' @param crs Projection of output. Default is to convert to \emph{WGS 84}.
#' 
#' @author Shona Wilde
#' 
#' @return An object of the same class as \code{st}.
#' 
#' @export

st_calculate_lagged_distance <- function(st, name = "distance_from_previous", as_numeric = T, crs = 4326) {

  
  # projection system must in meters
  if (!grepl("+units=m", st_projection(st)))
  {
    
    st <- st_transform(st, "+proj=moll")
    message("Temporarily transforming coordinates...")
    
  }
  
  # define empty geometries
  empty <- st_as_sfc("POINT(EMPTY)", crs = "+proj=moll")
  
  # distance between points
  st_dist <- st %>%
    mutate(
      !!name := st_distance(
        geometry, 
        lag(geometry, default = empty), 
        by_element = T)
    ) %>% 
    st_transform(crs = crs)
  
  if (as_numeric) {
    
    name <- sym(name)
    
    st_dist <- st_dist %>% 
      mutate(!! name := as.numeric(!! name)) %>% 
      relocate(geometry, .after = last_col())
    
  }
  
  return(st_dist)
  
  
}



