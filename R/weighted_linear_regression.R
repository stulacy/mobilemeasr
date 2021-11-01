#' Function to perform weighted simple linear or multiple regression
#'
#' @param data Input data.
#'
#' @param formula A formula specifying the model, as in \code{\link[stats]{lm}}.
#'
#' @param x Variable for calculating weights.
#'
#' @param mean_vector Vector of means for centre of Gaussian kernel. For example a vector of distances along a route.
#'
#' @param sigma Width of the Gaussian kernel smoothing function.
#' Small \sigma leads to a narrow Gaussian and hence allows the user to focus on very localised effects, whereas
#'  a bigger \sigma has the effect of smoothing things out.
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export

weighted_linear_regression <- function(data,
                                formula,
                                x = "x",
                                mean_vector,
                                sigma = 20) {

  df <- map_dfr(
    mean_vector,
    ~weighted_linear_regression_worker(
      data = data,
      formula = formula,
      x = x,
      sigma = sigma,
      mean = .x
    )
  )

  return(df)

}


weighted_linear_regression_worker <- function(data,
                                formula,
                                x,
                                mean,
                                sigma)
{

  # variable for calculating weights
  x <- data %>%
    pull(x)

  # Gaussian kernel weights
  weights <- exp(-(abs(x - mean)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))

  # bind to data
  data$weights <- weights

  # linear regression model
  model <- lm(
    formula = formula,
    data = data,
    weights = weights
  )

# build tibble
  results <- tidy_lm_output(model) %>%
    mutate(
      sigma = sigma,
      mean = mean,
      formula = format(formula)
    )

  return(results)

}







