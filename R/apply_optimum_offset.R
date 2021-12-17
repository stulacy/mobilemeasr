#' Function to apply optimum offset to align peaks 
#' 
#' Function to apply optimum offset to align peaks of different variables
#'
#' @param df Data frame to containing observations to align
#'
#' @param cols Columns to align, specified as in \code{\link[tidyr]{tidyr_tidy_select}}
#' 
#' @param align_by Column used to align all other variables to
#' 
#' @param offset_variable Name of variable to align
#' 
#' @param n Range of offsets to apply. Varies from -n to +n
#'
#' @return Tibble containing aligned observations
#'
#' @author Shona Wilde
#'
#' @export



# VECTORISED FUNCTION
apply_optiumum_offset <- function(df, cols, align_by = "nox", offset_variable = "value", n = 10) {
  
  
  if (nrow(df) == 0) {
    
    message("Data frame has ", nrow(df), " rows. Skipping...")
    
    return(tibble())
    
  }
  
  # prepare data
  df_prep <- df %>% 
    prepare_offset_data(cols = {{ cols }},
                        align_by = align_by)
  
  
  offset_variable <- sym(offset_variable)
  
  # generate sequence of offsets
  offset <- seq(-n, n, 1)
  
  
  df_align <- df_prep %>% 
    mutate(data_aligned = map(data,
                              ~apply_optimum_offset_worker(df = .x,
                                                           offset = offset,
                                                           align_by = align_by,
                                                           offset_variable = offset_variable)))

  
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
apply_optimum_offset_worker <- function(df, offset, align_by, offset_variable) {
  
  
  df_corr <- map_dfr(offset,
                     ~calculate_offset_correlation(df = df,
                                                   offset = .x,
                                                   align_by = align_by,
                                                   offset_variable = offset_variable))
  
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
  
  message("Optimum offset = ", optimum_offset, ", r = ", round(corr, digits = 3))
  
  # apply offsets
  if (optimum_offset > 0) {
    
    df_align <- df %>% 
      mutate(!!offset_variable := lag(!!offset_variable, optimum_offset))
    
  }
  
  if (optimum_offset < 0) {
    
    df_align <- df %>% 
      mutate(!!offset_variable := lead(!!offset_variable, abs(optimum_offset)))
    
  }
  
  if (optimum_offset == 0) {
    
    df_align <- df
    
  }
  
  
  return(df_align)
  
  
}

# FIND BEST CORRELATION ----
calculate_offset_correlation <- function(df, offset, align_by, offset_variable) {
  
  offset_variable <- sym(offset_variable)
  
  
  if (offset > 0) {
    
    df_offset <- df %>% 
      mutate(!!offset_variable := lag(!!offset_variable, offset))
  }
  
  if (offset < 0) {
    
    df_offset <- df %>% 
      mutate(!!offset_variable := lead(!!offset_variable, abs(offset)))
  }
  
  if (offset == 0) {
    
    df_offset <- df
    
  }
  
  # calculate correlation statistics
  # catch cases where there are not enough observations
  df_corr <- tryCatch(
    {
    df_offset %>% 
    correlation_summary(x = align_by,
                        y = offset_variable) %>% 
    mutate(offset = offset,
           .before = 1)
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
    select(date, 
           {{ cols }},
           !!align_by)
  
  df_long <- df_select %>% 
    pivot_longer(cols = -c(date, all_of(align_by)),
                 names_to = "variable")
  
  df_nest <- df_long %>% 
    group_nest(variable) 
  
  return(df_nest)
  
}





