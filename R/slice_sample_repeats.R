#' Sample random rows   
#'
#' Create multiple sub-samples of a chosen size containing rows of a data frame or tibble
#'
#' @param df A data frame or tibble 
#' 
#' @param n Sample size
#' 
#' @param times Number of repeated samples to take
#' 
#' @param replace Should sampling be performed with (\code{TRUE}) or without (\code{FALSE}) replacement?
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

slice_sample_repeats <- function(df, n, times, replace = F) {
  
  sample_id <- seq(1, times, 1)
  
  sample <- map_dfr(
    sample_id,
    ~slice_sample_repeats_worker(
      df = df,
      n = n,
      times = times,
      replace = replace,
      sample_id = .x
    )
  )
  
  return(sample)
  
}

slice_sample_repeats_worker <- function(df, n, times, replace, sample_id) {
  
  sample <- df %>% 
    slice_sample(
      n = n,
      replace = replace
    ) %>% 
    mutate(
      sample_id = sample_id,
      sample_size = n
    ) %>% 
    tibble()
  
  return(sample)
  
}

