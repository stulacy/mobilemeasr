#' Distance weighted mean
#'
#' Function to calculate a geographically-weighted mean
#'
#' @param st Object of class \code{sf} containing the data on which to perform the calculation
#'
#' @param location Object of class \code{sf} containing POINT geometries of locations to perform the calculation at.
#'
#' @param variable Name of the variable within \code{st} to use for the mean calculation.
#'
#' @param sigma Width of the Gaussian kernel smoothing function in m.
#' Small \eqn{\sigma} leads to a narrow Gaussian and hence allows the user to focus on very localised effects, whereas
#'  a bigger \eqn{\sigma} has the effect of smoothing things out.
#'
#' @param verbose Should a message be displayed to the user?
#'
#' @return Tibble.
#'
#' @author Shona Wilde
#'
#' @export

st_weighted_mean <- function(st,
                             location,
                             variable,
                             sigma,
                             verbose = T)
{

  n <- nrow(location)

  geometry <- st_geometry(location) %>%
    st_as_sf() %>%
    rowid_to_column() %>%
    split(.$rowid)

  # do
  df <- imap_dfr(
    geometry,
    .id = "id",
    ~st_weighted_mean_worker(
      st = st,
      location = .x,
      variable = variable,
      sigma = sigma,
      verbose = verbose,
      index = .y,
      n = n
    ),
    .progress = T
  )


  return(df)

}


st_weighted_mean_worker <- function(st,
                             location,
                             variable,
                             sigma,
                             verbose,
                             index,
                             n
                             ) {


  index <- as.numeric(index)


  # Display message for every 10th model
  if (verbose && index%%100== 0) {

    message(
      lubridate::now(tz = Sys.timezone()),
      ": Calculating ", index, " of ",  n, " times..."
    )
  }


  # format geometry column
  df_geometry <- location %>%
    st_as_sf() %>%
    sfc_to_columns(drop_geometry = T)

  lat <- df_geometry %>%
    pull(lat)

  long <- df_geometry %>%
    pull(long)

  # calculate distance to each observation
  distance <- st_distance(
    location,
    st,
    by_element = F
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

  # normalise
  weights_norm <- shonarrr::normalise(weights)

  # assign to df
  st$weights <- weights_norm

  # filter to only influencing observations to speed calculation up
  st_filt <- st %>%
    filter(weights > 0.01)

  # calc stats
  value <- weighted.mean(
    st_filt[[variable]],
    w = st_filt$weights,
    na.rm = T
  )

  # variance
  # sd <- Hmisc::wtd.var(
  #   st_filt[[variable]],
  #   weights = st_filt$weights
  # )

  # build tibble
  df_results <- tibble(
  #  variable = variable,
    value = value,
  #  sd = sd,
    lat = lat,
    long = long,
    sigma = sigma
  )

  return(df_results)

}






