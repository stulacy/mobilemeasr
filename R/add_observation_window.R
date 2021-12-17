#' Function to add observations from one data frame to another
#' 
#' Function to add observations from one data frame to another according to a time window
#'
#' @param df Data frame to add observations to
#'
#' @param df2 Data frame to extract observation window
#' 
#' @param threshold Minimum gap between observations in df (seconds). Used for flagging. Observations closer than threshold with be flagged as 1
#' 
#' @param n_max Maximum number of seconds in window before and after each observation in df. Used to prevent windows becoming too large if observations are far apart
#'
#' @return Tibble containing nested observations from df2
#'
#' @author Shona Wilde
#'
#' @export

add_observation_window <- function(df, df2, threshold = 10, n_max = 60) {
  
  # get names
  names1 <- names(df)
  names2 <- names(df2)
  
  # check for parsed date column 
  if (any(!"date" %in% names1, !"date" %in% names2)) {
    stop("Both input data frames must contain a `date` variable.", call. = FALSE)
  }
  
  if (any(!"POSIXct" %in% class(df$date)[1], !"POSIXct" %in% class(df2$date)[1])) {
    stop("`date` variables must be a parsed date (POSIXct).", call. = FALSE)
  }
  
  # compute lead and lag variables
  df <- df %>% 
    mutate(lead = lead(date),
           lag = lag(date))
  
  # calculate time difference to neighboring observations
  df_diff <- df %>% 
    mutate(lead_diff = as.numeric(lead - date),
           lag_diff = as.numeric(date - lag),
           n_forward = lead_diff/3,
           n_back = lag_diff/3,
           n_forward = if_else(n_forward > n_max, n_max, n_forward),
           n_back = if_else(n_back > n_max, n_max, n_back),
           flag = if_else(lead_diff < threshold | lag_diff < threshold, 1, 0),
           flag = replace_na(flag, 1),
           flag = as.factor(flag))
  
  # create windows
  # add flag variable if vehicle passes are too close together
  # first and last passes too
  df_window <- df_diff %>% 
    mutate(date_start = date - n_back,
           date_end = date + n_forward) %>% 
    select(all_of(names1),
           date_start, 
           date_end,
           flag) %>% 
    mutate(date_start = if_else(is.na(date_start), date, date_start),
           date_end = if_else(is.na(date_end), date, date_end))
  
  # filter second df to windows
  list_window <- map2(df_window$date_start,
                      df_window$date_end,
                      ~add_observation_window_worker(df2, 
                                                     date_start = .x,
                                                     date_end = .y))
  
  # add to df
  df_window_obs <- df_window %>% 
    mutate( observations = !!list_window)
  
  
  return(df_window_obs)
  
  
}


add_observation_window_worker <- function(df, date_start, date_end) {
  
  df_filt <- df %>% 
    filter(between(date, date_start, date_end))
  
  return(df_filt)
  
}
