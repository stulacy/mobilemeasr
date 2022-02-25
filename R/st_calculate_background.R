#' Calculate background values from spatial data
#'
#' This function is designed to be used prior to \code{st_weighted_quantile_regression}.
#' Background values are calculated using Gaussian kernel weighting from a point of interest. Observations are weighted according to their distance away from the point of interest.
#' The median of the lowest 20 observations at each point is taken as the background value.
#'  
#' @param st sf object containing spatial data
#' 
#' @param st_point Locations to perform calculation at. Should be an sf object containing POINT geometries
#' 
#' @param formula Formula used for regression. Background values are calculated for each variable in the formula
#' 
#' @param sigma Width of Gaussian kernel in m
#' 
#' @param verbose Display message to user?
#' 
#' @return sf object
#' 
#' @author Shona Wilde
#' 
#' @export

st_calculate_background <- function(st,
                              st_point,
                              formula,
                              sigma,
                              verbose = T) {
  
  
  
  n_points <- nrow(st_point)
  n <- seq(1:n_points)
  
  list_points <- map(
      n, ~slice(st_point, .x)
    )
  
  st_background <- imap_dfr(
    list_points,
    ~st_calculate_background_worker(
      st = st,
      st_point = .x,
      formula = formula,
      sigma = sigma,
      index = .y,
      n_points = n_points,
      verbose = verbose
    )
  )
  
  return(st_background)
  
}


st_calculate_background_worker <- function(st,
                                     st_point,
                                     formula,
                                     sigma,
                                     index,
                                     n_points,
                                     verbose) {
  
  
  
  
  index <- as.numeric(index)

  # Display message for every 10th model
  if (verbose && index%%100 == 0) {
    
    message(
      lubridate::now(tz = Sys.timezone()),
      ": Calculating ", index, " of ",  n_points, " times..."
    )
  }
  
  # calculate distance to each observation
  distance <- st_distance(
    st_point,
    st,
    by_element = T
  ) %>%
    as.numeric()
  
  # check how far away nearest observation is
  min_dist <- min(distance, na.rm = T)
  
  if (min_dist > 20) {
    
    message("No observations within 20 m of supplied geometry. Skipping model...")
    
    return(st_point)
    
  }
  
  # calculate weights
  weights <- exp(-(abs(distance - 0)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))
  
  weights_norm <- shonarrr::normalise(weights)
  
  st$weights <- weights_norm
  
  # filter to only influencing observations to speed calculation up
  st_filt <- st %>%
    filter(weights > 0.01)
  
  # get col names from formula
  cols <- cols_from_formula(formula)
  
  # calculate median minimum values at each point
  df_min <- st_filt %>% 
    st_drop_geometry() %>% 
    select(all_of(cols)) %>% 
    pivot_longer(
      cols = all_of(cols),
      "variable"
    ) %>% 
    group_by(variable) %>% 
    slice_min(value, n = 20) %>% 
    summarise(background = median(value))
  
  # make wide
  df_min_wide <- df_min %>% 
    pivot_wider(
      names_from = "variable",
      values_from = "background",
      names_prefix = "background_"
    ) %>% 
    mutate(sigma = sigma)
  
  # build tibble
  st_background <- st_point %>% 
    bind_cols(df_min_wide)
  
  return(st_background)
  
}




