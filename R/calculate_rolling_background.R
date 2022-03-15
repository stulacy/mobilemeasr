#' Function to calculate background values based on a rolling percentile.
#'
#' @param df Input data
#' @param cols Columns to calculate background for
#' @param width Width of rolling window
#' @param probs Percentile to calculate. Defualt is 1st
#' @param align Should the index of the result be center (default), left or right aligned compared to the rolling window of observations?
#' @param na.rm Should NAs be removed before the percentile is computed?
#' @param round Number of digits to round result to
#'
#' @return Data frame with additional background variables
#'
#' @examples
#' # calculate rolling 5-day 1st percentiles of ozone and solar radiation
#' calculate_rolling_background(
#'  df = airquality,
#'  cols = c(Ozone, Solar.R),
#'  width = 5,
#'  probs = 0.01
#' )
#'
#' @export


calculate_rolling_background <- function(df, cols, width, probs = 0.01, align = "center", na.rm = TRUE, round = NA){

  # make long
  df_long <- df %>%
    pivot_longer(
      cols  = {{cols}},
      names_to = "variable"
    )

  # do
  df_background <- df_long %>%
    group_by(variable) %>%
    mutate(
      background = rolling_percentile(
        value,
        width = width,
        align = align,
        probs = probs,
        na.rm = na.rm,
        round = round
      )
    )

  # wide format
  df_background_wide <- df_background %>%
    pivot_wider(
      names_from = "variable",
      values_from = c("value", "background"),
      values_fn = mean
    ) %>%
    rename_with(
      ~str_remove(., "^value_")
    )

  return(df_background_wide)

}



