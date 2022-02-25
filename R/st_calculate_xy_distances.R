#' Calculate horizontal and vertical distances between points
#'  
#' @param st Spatial data frame containing points
#' 
#' @param id ID variable for each point e.g name. If no ID is supplied row numbers are used
#' 
#' @param location1 Start point (sf object containing single point)
#' 
#' @param location2 End point (sf object containing single point)
#' 
#' @return Tibble
#' 
#' @author Shona Wilde
#' 
#' @export

st_calculate_xy_distances <- function(st, id = NA) {

  # check class
  stopifnot("Object class must be 'sf' " = inherits(st, "sf"))
  
  # add id column if not supplied
    if (is.na(id)) {
    st <- st %>% 
      rowid_to_column("id")
  }
  
  else {
    
    # rename id column
    st <- st %>% 
      rename(
        id := id
      )
  }
  
  # find all combinations
  suppressMessages(
    
    df_pairs <- crossing(st$id, st$id, .name_repair = "unique") %>% 
      set_names(
        c("start", "end")
      )
    
  )
  
  # create lists for mapping over
  list_start <- df_pairs$start
  list_end <- df_pairs$end
  
  
  list_location1 <- map(
    list_start,
    ~filter(st, id == .x)
  )
  
  list_location2 <- map(
    list_end,
    ~filter(st, id == .x)
  )
  
  # do
  df <- map2_dfr(
    list_location1,
    list_location2,
    ~st_calculate_xy_distance(
      location1 = .x,
      location2 = .y
    )
  )

  # join to pairs df
  df_dist <- bind_cols(
    df_pairs,
    df
  )  
  
  return(df_dist)
  
  
} 


#' @rdname st_calculate_xy_distances
#' @export 
st_calculate_xy_distance <- function(location1, location2) {
  
  # check consistent crs
  if (st_crs(location1) != st_crs(location2)) 
    stop("Coordinate reference systems are not identical...")
  
  # get x and y coordinates
  df1 <- st_coords(location1, names = c("x1", "y1"))
  df2 <- st_coords(location2, names = c("x2", "y2"))
  
  df <- bind_cols(df1, df2)
  
  # coordinates of right angle between points
  p <- df %>% 
    select(x1, y2) %>% 
    st_from_df("y2", "x1")
  
  # calculate x and y distances
  x_dist <- sf::st_distance(location2, p) %>% 
    as.numeric()
  
  y_dist = sf::st_distance(location1, p) %>% 
    as.numeric()
  
  # logic to determine direction 
  if (df1$x1 > df2$x2) {
    x_dist <- -x_dist
  }
  
  if (df1$y1 > df2$y2) {
    y_dist <- -y_dist
  }
  
  # build tibble
  df_dist <- tibble(x_dist, y_dist)
  
  return(df_dist)

}









