#' Filter layers of a raster brick between two dates
#'
#' Function to subset layers of a raster brick object based on a date range. \code{filter_raster_by_date} was designed for use on the era5 reanalysis dataset.
#'
#' @param raster Raster brick for filtering. Layers of the brick must correspond to dates e.g hourly time series data
#'
#' @param start Start date (yyyy-mm-dd)
#'
#' @param end End date (yyyy-mm-dd)
#'
#' @return Filtered raster object
#'
#' @author Shona Wilde
#'
#' @export


filter_raster_by_date <- function(raster, start, end) {


  # parse dates
  start_date <- start %>% as.POSIXct(tz = "UTC")
  end_date <- end %>% as.POSIXct(tz = "UTC") + threadr::seconds_in_a_day()

  # parse dates from raster
  dates <- names(raster) %>%
    str_remove("^X") %>%
    ymd_hms()

  # find index of dates needed
  date_index_range <- tibble(
    date = dates
  ) %>%
    rowid_to_column("index") %>%
    filter(
      between(
        date,
        start_date,
        end_date
      )
    ) %>%
    shonarrr::slice_min_max(index) %>%
    pull(index)

  # create sequence of indexes for subsetting
  date_index_seq <- seq(date_index_range[1], date_index_range[2], 1)


  # subset raster
  ra_filt <- subset(
    raster, date_index_seq
  )

  return(ra_filt)

}


