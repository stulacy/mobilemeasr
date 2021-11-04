#' Function to create a \code{tbl_graph} from spatial linestrings
#'
#' This function is adapted from \href{https://r-spatial.org/r/2019/09/26/spatial-networks.html}{Spatial networks in R with sf and tidygraph}.
#'
#' @param st \code{sf} object with \code{LINESTRING} geometries.
#'
#' @param directed Should the graph be directed? Default is FALSE.
#'
#' @return \code{tbl_graph}.
#'
#' @author Shona Wilde
#'
#' @seealso \code{\link[tidygraph]{tbl_graph}}
#'
#' @export

st_make_tidygraph <- function(st, directed = F) {
  
  # check class
  stopifnot(
    "Object class must be `sf`" = 
      inherits(st,"sf") 
  )
  
  # check geometry type
  if (any(!st_geometry_type(st) %in% "LINESTRING")) {
    stop("Geometry must be of type 'LINESTRING'...",
         call. = FALSE)
  }
  
  
  # assign edge ID
  st_edges <- st %>% 
    mutate(edge_id = c(1:n()))
  
  # create nodes at start and end of roads
  st_nodes <- st_edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edge_id = L1) %>%
    group_by(edge_id) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(
      start_end = rep(c('start', 'end'), 
                      times = n()/2)
    )
  
  # give nodes a unique ID
  suppressWarnings(
    
    st_nodes <- st_nodes %>%
      mutate(
        xy = str_c(X, Y, sep = " "),
        node_id = group_indices(., factor(xy, levels = unique(xy)))
      ) %>% 
      select(-xy)
    
  )
  
  # get start and end nodes
  source_nodes <- st_nodes %>%
    filter(start_end == 'start') %>%
    pull(node_id)
  
  target_nodes <- st_nodes %>%
    filter(start_end == 'end') %>%
    pull(node_id)
  
  # add edge identifier
  st_edges <- st_edges %>%
    mutate(
      from = source_nodes,
      to = target_nodes
    )
  
  # remove duplicate nodes
  st_nodes_unique <- st_nodes %>%
    distinct(node_id, .keep_all = TRUE) %>%
    select(-c(edge_id, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(st_edges))
  
  # create graph
  st_graph <- tbl_graph(
    nodes = st_nodes_unique, 
    edges = as_tibble(st_edges), 
    directed = directed
  ) 
  
  # how many edges connected to each node?
  st_graph_degree <- st_graph %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree())
  
  return(st_graph_degree)
  
  
}



