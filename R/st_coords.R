#' Retrieve coordinates in tibble form
#'  
#' @param st Object of class sf
#' 
#' @param names Vector of names for the columns in the order x, y
#' 
#' @return Tibble of x and y coordinates
#' 
#' @author Shona Wilde
#' 
#' @export

st_coords <- function(st, names = c("x", "y")) {
  
  stopifnot("Object class must be 'sf' " = inherits(st, "sf"))
  
  df <- st_coordinates(st) %>% 
    as_tibble() %>% 
    set_names(names)
  
  return(df)
  
}

