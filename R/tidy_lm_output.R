#' Function to summarise the output from \code{\link[stats]{lm}} in a tidy format
#'
#' @param model Model object to summarise.
#' 
#' @return Tibble containing model results.
#' 
#' @author Shona Wilde
#' 
#' @export


tidy_lm_output <- function(model){
  
  summary <- summary(model)
  
  df_model <- summary$coefficients %>% 
    as_tibble(rownames = "coeff") %>% 
    clean_names() %>% 
    rename(
      value = estimate,
      p_value = pr_t,
    ) %>% 
    mutate(
      coeff = str_replace(coeff, "(\\(Intercept)\\)", "intercept"),
      r_squared = summary$r.squared
    )
  
  return(df_model)
  
}