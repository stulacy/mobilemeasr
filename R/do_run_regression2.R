#' Function to apply a rolling linear regression model to data
#'
#' \code{do_run_regression} calculates the regression results for fixed-width, overlapping windows of data.
#'
#' @param df Input data frame.
#'
#' @param formula A formula specifying the model, as in code{\link[stats]{lm}}.
#'
#' @param n Length of moving window.
#'
#' @param complete Should the function be evaluated on complete windows only?
#' If FALSE then partial computations will be allowed.
#'
#' @param verbose Should messages be displayed to the user?
#'
#' @return Tibble with list columns containing observations and model summaries for each window.
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


do_run_regression2 <- function(df, formula, n = 5, complete = T, verbose = T) {

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
    message(lubridate::now(tzone = Sys.timezone()), ": Performing regression using ", y, "~", x, "...")
  }


  # add list column containing rolling observations
  df_roll <- df %>%
    get_rolling_observations(n = n)

  # length of df
  n_models <- nrow(df_roll)

  # run models
  df_model_stats <- df_roll %>%
    rowwise() %>%
    mutate(
      model_stats = list(
        do_run_regression_worker2(
          data = observations,
          formula = formula,
          n = n,
          complete = complete,
          verbose = verbose,
          index = window_id,
          n_models = n_models
        )
      )
    )


  return(df_model_stats)


}




do_run_regression_worker2 <- function(data, formula, n, complete, verbose, index, n_models){

  # only display message for every 10th model
  if (verbose && index%%100 == 0) {

    message(
      lubridate::now(tzone = Sys.timezone()), ": Modelling ", index,
      " of ", n_models, " times..."
    )
  }


  # wrap to prevent function falling over when lm encounters an error
  possible_lm <-  purrr::possibly(.f = lm, otherwise = NULL)

  model <- suppressWarnings(

    possible_lm(
      formula = formula,
      data = data
    )

  )

  # calculate model summaries
 # model_stats <- lm_model_stats(model)

  model_stats <- tryCatch( {

    lm_model_stats(model)
  },
  error = function(e) {

    print(paste("MY_ERROR:  ",e))
    return(tibble())

  }
  )

  # invalidate models where window is smaller than n
  if (complete){

    if (nrow(data) < n)

      model_stats <- tibble()
  }


  return(model_stats)

}



lm_model_stats <- function(model) {

  suppressWarnings(

    # build tibble
    df <- tibble(
      slope = model$coefficients[[2]],
      intercept = model$coefficients[[1]],
      r_squared = summary(model)$r.squared,
      p_value = summary(model)$coefficients[,"Pr(>|t|)"][2]
    )

  )
  return(df)

}









