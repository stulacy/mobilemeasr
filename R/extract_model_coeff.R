#' Function to extract coefficients from a GAM model
#'
#' @param model Model object.
#' 
#' @return Tibble containing model results.
#' 
#' @author Shona Wilde
#' 
#' @export
#' 

extract_model_coeff <- function(model){
  
  # standard summary object
  model_summary <- summary(model)
  
  # extract table of coefficients
  table <- model_summary$p.table
  
  row_names <- rownames(table)

  # create tibble and clean
  df <- table %>% 
    as_tibble() %>% 
    clean_names() %>% 
    mutate(
      term = row_names,
      .before = 1,
      term = str_replace_all(term, "\\(Intercept\\)", "intercept"),
      r_sq = model_summary$r.sq
    ) %>% 
    relocate(
      r_sq, .after = std_error
    )
  
  return(df)
  
}

