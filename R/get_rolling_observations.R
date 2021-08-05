#' Function to get observations resulting from a rolling window
#'
#' @param df Input data frame.
#' 
#' @param n Size of rolling window.
#' 
#' @return Tibble.
#' 
#' @author Shona Wilde & Stuart Grange
#' 
#' @export


get_rolling_observations <- function(df, n = 5) {
  
  # add id variable
  df <- df %>% 
    rowid_to_column("observation_id")
  
  # Create the indices
  # Get length of input
  index_start <- seq_len(nrow(df))
  
  # Make end index, but this will be less than the start since we are
  # working with lags
  index_end <- index_start - (as.integer(n) - 1L)
  # Catch the negative and 0 values
  index_end <- if_else(index_end <= 1, 1L, index_end)
  
  # Slice by the two indices
  list_sliced <- purrr::map2(
    index_start, 
    index_end, 
    ~slice_by_indices(df, .x, .y)
  )
  
  # Add the list column to the input
  #df <- mutate(df, observations = !!list_sliced) 
  
  # build tibble
  df_sliced <- tibble(
    observations = !!list_sliced
  ) %>% 
    rowid_to_column("window_id")
  
  return(df_sliced)
  
}


slice_by_indices <- function(df, index_start, index_end) {
  slice(df, index_end:index_start)
}
