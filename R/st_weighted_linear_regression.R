#' Function to perform distance-weighted quantile regression
#'
#' @param st Object of class \code{sf} containing the data on which to perform the regression.
#'
#' @param location Object of class \code{sf} containing POINT geometries of locations to perform the regression at.
#'
#' @param formula A formula specifying the model, as in \code{\link[quantreg]{rq}} and \code{\link[stats]{lm}}.
#'
#' @param sigma Width of the Gaussian kernel smoothing function.
#' Small \eqn{\sigma} leads to a narrow Gaussian and hence allows the user to focus on very localised effects, whereas
#'  a bigger \eqn{\sigma} has the effect of smoothing things out.
#'
#' @param verbose Should a message be displayed to the user?
#'
#' @return Tibble containing model results.
#'
#' @author Shona Wilde
#'
#' @export

st_weighted_linear_regression <- function(st,
                                          location,
                                          formula,
                                          sigma = 250,
                                          verbose = T) {

  # count models
  n_models <- nrow(location)

  # pull out geometries to iterate over
  geometry <- st_geometry(location) %>%
    st_as_sf() %>%
    rowid_to_column() %>%
    split(.$rowid)

  # do
  df <- imap_dfr(
    geometry,
    .id = "model_id",
    ~st_weighted_linear_regression_worker(
      st = st,
      location = .x,
      index = .y,
      n_models = n_models,
      formula = formula,
      sigma = sigma,
      verbose = verbose
    )
  )


  return(df)

}


st_weighted_linear_regression_worker <- function(st,
                                                 location,
                                                 index,
                                                 n_models,
                                                 formula,
                                                 sigma,
                                                 verbose)
{

  # get current model number
  index <- as.numeric(index)


  # Display message for every 10th model
  if (verbose && index%%100== 0) {

    message(
      lubridate::now(tz = Sys.timezone()),
      ": Modelling ", index, " of ",  n_models, " times..."
    )
  }


  # store lat and lon
  lat <- st_coordinates(location)[2]
  long <- st_coordinates(location)[1]

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


  # linear regression model
  model <- lm(
    formula = formula,
    data = st_filt,
    weights = weights
  )

  # build tibble
  df_results <- shonarrr::tidy_lm_output(model) %>%
    mutate(
      model = list(model),
      lat = lat,
      long = long,
      sigma = sigma,
      formula = format(formula)
    ) %>%
    select(
      model,
      lat,
      long,
      term,
      formula,
      value,
      std_error,
      r_squared,
      sigma,
      p_value,
      t_value
    )

  return(df_results)

}







