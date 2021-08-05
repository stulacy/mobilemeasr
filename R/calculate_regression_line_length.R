#' Function to calculate the length of a regression line
#' 
#' @param df Input data frame. Must contain a slope variable.
#'
#' @param x x variable for regression.
#' 
#' @param y y variable for regression.
#' 
#' @return Tibble.
#' 
#' @author Shona Wilde
#' 
#' @export


calculate_regression_line_length <- function(df, x, y){
  
  # find min and max x values
  df <- df %>% 
    mutate(
      across(x, list(min = min, max = max)
      )
    )
  
  # calculate lengths
  # need some extra logic if slope is negative
  df_length <- df %>% 
    mutate(
      y_min = (slope*x_min) + intercept,
      y_max = (slope*x_max) + intercept,
      x_length = if_else(
        x_min < x_max, x_max-x_min, x_min-x_max
      ),
      y_length = if_else(
        y_min < y_max, y_max-y_min, y_min-y_max
      ),
      length = calc_hypotenuse_length(x_length, y_length)
    ) %>% 
    as_tibble()
  
  return(df_length)
  
}



