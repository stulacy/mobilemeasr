#' Function to perform weighted quantile regression
#'
#' @param data Input data.
#'
#' @param formula A formula specifying the model, as in \code{\link[quantreg]{rq}} and \code{\link[stats]{lm}}.
#'
#' @param x Variable for calculating weights.
#'
#' @param mean_vector Vector of means for centre of Gaussian kernel. For example a vector of distances along a route.
#'
#' @param tau The quantile(s) to be estimated. Generally a number between 0 and 1.
#'
#' @param sigma Width of the Gaussian kernel smoothing function.
#' Small \eqn{\sigma} leads to a narrow Gaussian and hence allows the user to focus on very localised effects, whereas
#'  a bigger \eqn{\sigma} has the effect of smoothing things out.
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export


weighted_quantile_regression <- function(data,
                                  formula,
                                  x = "x",
                                  mean_vector,
                                  tau = c(0.5, 0.75, 0.9, 0.95, 0.99),
                                  sigma = 20)
{

  df <- map_dfr(
    mean_vector,
    ~weighted_quantile_regression_worker(
      data = data,
      formula = formula,
      x = x,
      mean = .x,
      tau = tau,
      sigma = sigma
    )
  )

  return(df)

}

# Gaussian smoother
weighted_quantile_regression_worker <- function(data,
                                         formula,
                                         x,
                                         mean,
                                         tau,
                                         sigma)
{

  # variable for calculating weights
  x <- data %>%
    pull(x)

  # Gaussian kernel weights
  weights <- exp(-(abs(x - mean)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))

  # bind to data
  data$weights <- weights


  # quantile regression model
  model <- quantreg::rq(
    formula = formula,
    data = data,
    tau = tau,
    weights = weights
  )

  # build tibble
  results <- tidy_rq_output(model) %>%
    mutate(
    mean = mean,
    sigma = sigma,
    formula = format(formula)
  ) %>%
    select(
      term,
      formula,
      mean,
      value,
      quantile,
      quantile_name,
      std_error,
      sigma,
      p_value,
      t_value
    )


  return(results)

}
