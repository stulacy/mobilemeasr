#' Function to perform run regression analysis 
#'
#' \code{do_run_regression} calculates the regression results for fixed-width, overlapping windows of data
#'
#' @param df Input data frame
#' 
#' @param formula A formula specifying the model, as in \code{\link[lmodel2]{lmodel2}} and \code{\link[stats]{lm}}
#' 
#' @param n Size of moving window
#' 
#' @param complete Should the function be evaluated on complete windows only? 
#' If FALSE then partial computations will be allowed.
#' 
#' @param method Regression method as in \code{\link[lmodel2]{lmodel2}}.
#' Options are ordinary least squares (OLS), major axis (MA), standard major axis (SMA), and ranged major axis (RMA)
#' 
#' @param verbose Should messages be displayed to the user?
#' 
#' @return Tibble of original data with list columns containing observations for each moving window, model objects and model summaries.
#' 
#' @author Shona Wilde
#' 
#' @examples 
#' \dontrun{
#' 
#' # Perform rolling regression with window size 3
#' do_run_regression(df, nox~co2, n = 3)
#' 
#' }
#' 
#' 
#' @export


do_run_regression <- function(df, formula, n = 5, complete = T, method = "SMA", verbose = T) {
  
  # get and y variables
  formula <- as.formula(formula)
  
  y <- attr(terms(formula), which = "variable") %>%
    as.character() %>%
    `[`(2)
  
  x <- attr(terms(formula), which = "term.labels")
  
  # check names and class
  if (any(!c(x, y) %in% names(df))) {
    stop("Input data must contain both `x` and `y` variables for regression...", call. = FALSE)
  }
  
  if (any(!c(class(df[[x]]), class(df[[y]])) %in% c("numeric", "integer"))) {
    stop("`x` and `y` variables must be either numeric or integer class...", 
         call. = FALSE)
  }
  
  # print formula used for regression
  if (verbose) {
    message(threadr::str_date_formatted(), ": Performing regression using ", y, "~", x, "...")
  }
  
  
  # add list column containing rolling observations
  df <- df %>% 
    get_rolling_observations(n = n) 
  
  # length of df
  n_models <- nrow(df)
  
  # run models
  df_model <- df %>% 
    mutate(
      model = map2(
        observations, .$window_id,
        ~do_run_regression_worker(
          data = .x, 
          index = .y, 
          formula = formula,
          n_models = n_models, 
          n = n,
          complete = complete,
          verbose = verbose)
      )
    )
  
  # calculate model summaries
  df_model_stats <- df_model %>%
    filter(!map_lgl(model, is.null)) %>% 
    mutate(
      model_stats = purrr::map(model, shonarrr::tidy_model_summary)
    )
  
  # get names for use later
  model_stats_names <- names(df_model_stats$model_stats[[1]])
  
  # some tidying and calculate lengths of regression line and add this to model stats df
  df_lengths <- df_model_stats %>% 
    select(
      window_id, 
      observations,
      model_stats
    ) %>% 
    tidyr::unnest(observations) %>% 
    rename(y = !!y,
           x = !!x) %>% 
    tidyr::unnest(model_stats) %>% 
    filter(method == !!method) %>% 
    select(
      window_id,
      x,
      y,
      everything()
    ) %>% 
    group_by(window_id) %>% 
    calculate_regression_line_length() %>% 
    select(
      window_id, 
      all_of(model_stats_names),
      x_min,
      x_max,
      y_min,
      y_max,
      length
    ) %>% 
    distinct_all() %>% 
    ungroup() %>% 
    group_nest(window_id, .key = "model_stats") %>% 
    mutate(
      model_stats = map(model_stats, as_tibble)
    )
    
  # overwrite model stats column
  df_model_stats$model_stats <- df_lengths$model_stats
  
  
  return(df_model_stats)

  
}



do_run_regression_worker <- function(data, index, formula, n, n_models, complete, verbose){
  
  # get length of data
  nrow_window <- nrow(data)
  
  # only display message for every 10th model
  if (verbose && index%%10 == 0) {
    
    message(
      str_date_formatted(), ": Modelling ", index, 
      " of ", n_models, " times..."
    )
  }
  
  
  # wrap to prevent function falling over when lmodel2 encounters an error
  possible_lmodel2 <-  purrr::possibly(.f = lmodel2::lmodel2, otherwise = NULL)
  
  model <- suppressWarnings(
    
    possible_lmodel2(
      formula = formula,
      data = data, 
      range.y = "interval",
      range.x = "interval", 
      nperm = 99)
    
  )
  
  # invalidate models where window is smaller than n
  if (complete){
    
    if(nrow_window < n)
      
      model <- NULL
  }
  
  return(model)
  
}




