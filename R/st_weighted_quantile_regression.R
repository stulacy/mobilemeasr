#' Function to perform distance-weighted quantile regression
#'
#' @param st Object of class \code{sf} containing the data on which to perform the regression.
#'
#' @param location Object of class \code{sf} containing POINT geometries of locations to perform the regression at.
#'
#' @param formula A formula specifying the model, as in \code{\link[quantreg]{rq}} and \code{\link[stats]{lm}}.
#'
#' @param tau The quantile(s) to be estimated. Generally a number between 0 and 1.
#'
#' @param sigma Width of the Gaussian kernel smoothing function.
#' Small \eqn{\sigma} leads to a narrow Gaussian and hence allows the user to focus on very localised effects, whereas
#'  a bigger \eqn{\sigma} has the effect of smoothing things out.
#'
#' @param se Method to use for calculating standard errors. One of NULL, "nid", "rank", "iid", "ker", or "boot".
#'
#' @param verbose Should a message be displayed to the user?
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export



st_weighted_quantile_regression <- function(st,
                                     location,
                                     formula,
                                     tau = c(0.5, 0.75, 0.9, 0.95, 0.99),
                                     sigma = 20,
                                     se = NULL,
                                     verbose = T) {


  n_models <- nrow(location)

  geometry <- st_geometry(location) %>%
    st_as_sf() %>%
    rowid_to_column() %>%
    split(.$rowid)

  df <- imap_dfr(
    geometry,
    .id = "model_id",
    ~st_weighted_quantile_regression_worker(
      st = st,
      location = .x,
      index = .y,
      n_models = n_models,
      formula = formula,
      tau = tau,
      sigma = sigma,
      se = se,
      verbose = verbose
    )
  )


  return(df)

}



st_weighted_quantile_regression_worker <- function(st,
                                            location,
                                            index,
                                            n_models,
                                            formula,
                                            tau,
                                            sigma,
                                            se,
                                            verbose) {


  index <- as.numeric(index)


  # Display message for every 10th model
  if (verbose && index%%10 == 0) {

    message(
      lubridate::now(tz = Sys.timezone()),
      ": Modelling ", index, " of ",  n_models, " times..."
    )
  }


  # format geometry column
  df_geometry <- location %>%
    st_as_sf() %>%
    sfc_to_columns(drop_geometry = T)

  lat <- df_geometry %>%
    pull(latitude)

  long <- df_geometry %>%
    pull(longitude)

  # calculate distance to each observation
  distance <- st_distance(
    location,
    st,
    by_element = T
  ) %>%
    as.numeric()

  # check how far away nearest observation is
  min_dist <- min(distance, na.rm = T)

  if (min_dist > 10) {

    message("No observations within 10 m of supplied geometry. Skipping model...")

    return(tibble())

  }

  # calculate weights
  weights <- exp(-(abs(distance - 0)) ^ 2/(2 * sigma ^ 2)) / (sigma * sqrt(2 * pi))

  weights_norm <- shonarrr::normalise(weights)

  st$weights <- weights_norm

  # filter to only influencing observations to speed calculation up
  st_filt <- st %>%
    filter(weights > 0.01)


  # quantile regression model
  model <- quantreg::rq(
    formula = formula,
    data = st_filt,
    tau = tau,
    weights = weights
  )



  # build tibble
  # build tibble
  results <- tryCatch(
    {
      broom::tidy(model) %>%
        janitor::clean_names() %>%
        rename(
          quantile = tau,
          value = estimate
        ) %>%
        mutate(
          lat = lat,
          long = long,
          sigma = sigma,
          formula = format(formula),
          term = str_replace(term, "(\\(Intercept)\\)", "intercept"),
          quantile_name = paste0("Q", quantile*100)
        ) %>%
        select(
          lat,
          long,
          term,
          formula,
          value,
          quantile,
          quantile_name,
          conf_low,
          conf_high,
          sigma
        )
    },
    error = function(e) {
      print(e)
      return(tibble())

    }

  )

  # results <- tryCatch(
  #   {
  #   tidy_rq_output(model, se = se) %>%
  #   mutate(
  #     lat = lat,
  #     long = long,
  #     sigma = sigma,
  #     formula = format(formula)
  #   ) %>%
  #   select(
  #     lat,
  #     long,
  #     term,
  #     formula,
  #     value,
  #     quantile,
  #     quantile_name,
  #     std_error,
  #     sigma,
  #     p_value,
  #     t_value
  #   )
  #   },
  #   error = function(e) {
  #     print(e)
  #     return(tibble())
  #
  #   }
  #
  # )

  return(results)

}




