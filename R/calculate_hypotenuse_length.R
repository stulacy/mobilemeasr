#' Function to calculate the length of the hypotenuse in a triangle
#'
#' @param x Horizontal side of triangle.
#'
#' @param y Vertical side of triangle.
#'
#' @param verbose Should messages be displayed to the user?
#'
#' @return Value.
#'
#' @author Shona Wilde
#'
#' @export



calculate_hypotenuse_length <- function(x, y, verbose) {

  sides <- c(x, y)


  if(any(sides[!is.na(sides)] < 0))  {
    stop("x and y must be positive...", call. = FALSE)
  }

  if(!is.numeric(x = sides))  {
    stop("x and y must be numeric...", call. = FALSE)
  }

  if(any(is.na(sides)) && verbose) {
    message("Missing values detected, calculation of regression line length will return NA...")
  }

  length <- sqrt(x^2 + y^2)

  return(length)


}

