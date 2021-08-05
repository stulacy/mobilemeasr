#' Function to perform distance-weighted quantile regression
#'
#' @param data Input data.
#' 
#' @param x Variable for calculating weights.
#' 
#' @param formula A formula specifying the model, as in \code{\link[quantreg]{rq}} and \code{\link[stats]{lm}}.
#' 
#' @param distance Distance variable.
#' 
#' @param tau The quantile(s) to be estimated. Generally a number between 0 and 1.
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



# Gaussian smoother 
distance_weighted_quant_reg <- function(data,
                                        x = "x",
                                        formula,
                                        distance,
                                        tau = c(0.5, 0.75, 0.9, 0.95, 0.99),
                                        sigma = 20) {
  
  # variable for calculating weights
  x <- data %>% 
    pull(x)

  # Gaussian kernel weights
  weights <- exp(-(abs(x - distance)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))
  
  # bind to data
  data$weights <- weights
  
  
  #obs_weights <- exp(-(abs(data$total_dist - distance)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))
  
  # quantile regression model
  model <- rq(
    formula = formula, 
    data = data, 
    tau = tau,
    weights = weights
  )
  
  # build tibble
  results <- tibble(
    distance = distance,
    coef = coef(model)[2, ],
    quantile = model$tau,
    quantile_name = paste0("Q", quantile*100)
  )
  
  
  return(results)
  
}


