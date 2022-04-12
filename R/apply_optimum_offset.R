#' Function to apply optimum offset to align peaks
#'
#' Function to apply optimum offset to align peaks of different variables
#'
#' @param df Data frame to containing observations to align
#'
#' @param cols Columns to align, specified as in \code{\link[tidyr]{tidyr_tidy_select}}
#'
#' @param align_by Column used to align all other variables to. Must not be included in \code{cols}
#'
#' @param n Range of offsets to apply. Varies from -n to +n
#'
#' @return Tibble containing aligned observations
#'
#' @author Shona Wilde
#'
#' @export



# VECTORISED FUNCTION
apply_optimum_offset <- function(df, cols, align_by, n = 10) {


  if (nrow(df) == 0) {

    message("Data frame has ", nrow(df), " rows. Skipping...")

    return(tibble())

  }

  # prepare data
  df_prep <- df %>%
    prepare_offset_data(
      cols = {{cols}},
      align_by = {{align_by}}
    )

  col <- sym("value")

  # align all columns at once
  df_align <- df_prep %>%
    mutate(
      data_aligned = map(
        data,
        ~apply_optimum_offset_worker(
          df = .x,
          col = {{col}},
          align_by = {{align_by}},
          n = n
        )
      )
    )


  # make wide
  df_wide <- df_align %>%
    select(-data) %>%
    unnest(data_aligned) %>%
    pivot_wider(
      names_from = "variable",
      values_from = "value",
      values_fn = mean)

  return(df_wide)

}

# APPLY LAGS
apply_optimum_offset_worker <- function(df, col, align_by, n) {

  # generate sequence of offsets
  offset <- seq(-n, n, 1)


  df_corr <- map_dfr(
    offset,
    ~calculate_offset_correlation(
      df = df,
      col = {{col}},
      align_by = {{align_by}},
      offset = .x
    )
  )

  if (nrow(df_corr) == 0) {

    return(df)

  }


  # find best correlation
  optimum_offset <- df_corr %>%
    slice_max(estimate) %>%
    slice(1) %>%
    pull(offset)

  corr <- df_corr %>%
    slice_max(estimate) %>%
    slice(1) %>%
    pull(estimate)

  message(
    lubridate::now(), ": ",
    enquo(col) %>% rlang::as_name() %>% str_remove("~"), enquo(align_by), " optimum offset = ", optimum_offset, ", r = ", round(corr, digits = 3)
  )

  # apply offsets
  if (optimum_offset > 0) {

    df_align <- df %>%
      mutate({{col}} := lag({{col}}, optimum_offset))

  }

  if (optimum_offset < 0) {

    df_align <- df %>%
      mutate({{col}} := lead({{col}}, abs(optimum_offset)))

  }

  if (optimum_offset == 0) {

    df_align <- df

  }


  return(df_align)


}

# FIND BEST CORRELATION ----


calculate_offset_correlation <- function(df, col, align_by, offset) {


  if (offset > 0) {

    df_offset <- df %>%
      mutate({{col}} := lag({{col}}, offset))
  }

  if (offset < 0) {

    df_offset <- df %>%
      mutate({{col}} := lead({{col}}, abs(offset)))
  }

  if (offset == 0) {

    df_offset <- df

  }

  # calculate correlation statistics
  # catch cases where there are not enough observations
  df_corr <- tryCatch(
    {
      df_offset %>%
        correlation_summary(
          x = enquo(align_by),
          y = enquo(col)
        ) %>%
        mutate(
          offset = offset,
          .before = 1
        )
    },
    error = function(e) {
      print(e)
      message("Not enough finite observations to calculate correlation statistics. Skipping...")
      return(tibble())
    },
    warning = function(w) {
      if (str_detect(as.character(w), "the standard deviation is zero"))
        return(tibble())
    }

  )

  return(df_corr)

}


# PREPARE DATA
prepare_offset_data <- function(df, cols, align_by) {

  # check for duplicated data
  if (any(duplicated(df$date))) {

    warning("Duplicated dates detected. Output will be summarised...", call. = F)

  }


  df_select <- df %>%
    select(
      date,
      {{cols}},
      {{align_by}}
    )

  df_long <- df_select %>%
    pivot_longer(
      cols = -c(date, {{align_by}}),
      names_to = "variable",
      values_to = "value"
    )

  df_nest <- df_long %>%
    group_nest(variable)

  return(df_nest)

}





