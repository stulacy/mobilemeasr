#' Function to filter spatial geometries based on a latitude or longitude range
#'
#' @param st  \code{sf} POINTS object.
#' 
#' @param min Minimum latitude/longitude value.
#' 
#' @param max Maximum latitude/longitude value.
#' 
#' @param direction Dimension to filter - either 'x' or 'y'. If 'x' use a longitude range. If 'y' use a latitude range.
#'
#' @return Filtered \code{sf} object.
#' 
#' @author Shona Wilde
#' 
#' @export


st_filter_by_range <- function(st, min, max, direction = "x") {
  
  if (!direction %in% c("x", "y")) {
    stop("Direction  must be `x` `or y`.", call. = FALSE)
  }
  
  st <- st %>% 
    sfc_to_columns()
  
  if (direction == "x") {
    
    st_filt <- st %>%
      filter(
        between(long, min, max)
      )
  }
  
  
  if (direction == "y") {
    
    st_filt <- st %>%
      filter(
        between(lat, min, max)
      )
    
  }
  st_filt_drop <- st_filt %>% 
    select(-lat, -long)
  
  return(st_filt_drop)
  
}



