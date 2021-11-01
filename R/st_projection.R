#' Function to return the projection system of a spatial object.
#'
#' @param st Object with class \code{sf} or \code{sfc}.
#' 
#' @author Shona Wilde
#' 
#' @return Character.
#' 
#' @export

st_projection <- function(st){
  
  suppressWarnings(
    
    st_crs(st, parameters = T)$proj4string
  
  )

}

