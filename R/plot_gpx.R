#' Function to plot GPX files on a map
#'
#' @param file GPX file
#'
#' @author Shona Wilde
#'
#' @export

plot_gpx <- function(file) {

  
  gpx <- tmaptools::read_GPX(file)
  
  st_line <- gpx %>% 
    purrr::pluck(1)
  
  st_line %>% 
    gissr::leaflet_plot()  
  
}

