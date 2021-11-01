
leaflet_plot_coloured <- function(st, colour_by = "value", fill_opacity = 0.5) {
  
  
  colour_var <- sym(colour_by)
  
  domain <- st %>% 
    pull(!!colour_var)
 
  
  # set palette  based on continuous or discrete variables
  if (class(domain) %in% c("integer", "numeric")) 
    
  {
    pal <- colorNumeric(
      palette = "plasma",
      domain = domain
    )
    
  }
  
  else {
    
    pal <- colorFactor(
      "plasma",
      domain = domain, 
      na.color = "black"
    )
    
  }
  
  # build plot with legend
  plot <- gissr::leaflet_plot(
    st,
    fill_opacity = fill_opacity,
    colour = ~pal(domain)
  )  %>% 
    addLegend(
      "bottomright", 
      pal = pal,
      values = ~domain,
      title = colour_by,
      opacity = 1
    )
  
  
  return(plot)

}
  



