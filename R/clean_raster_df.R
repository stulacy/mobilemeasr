#' Tidy output of raster summary
#' 
#' This function is specific to the era5 reanalysis dataset
#' 
#' @param df Data frame resulting from summarising a raster object using \code{\link{terra::global}}
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export


clean_raster_df <- function(df) {
  
  df_clean <- df %>% 
    rownames_to_column("date") %>% 
    as_tibble() %>% 
    mutate(
      date = str_remove(date, "^X") %>% 
        ymd_hms()
    )
  
  return(df_clean)
  
}
