#' Find a compatible row for extending a latin rectangle
#'
#' Given an input latin rectangle this function will
#' generate a new row that can be added to the latin
#' rectangle.
#'
#' The method used is to create a bipartite graph
#' with vertex partitions for columns and symbols
#' missing from columns and then find a maximum
#' matching in that bipartite graph.
#'
#' @param R A latin square
#' @param l_order The order of the input latin square
#'
#' @return A bipartite graph.
#' @export
next_row <- function(R, l_order) {
  bg <- to_tidygraph(R, l_order)
  
  m <- max_bipartite_match(bg)
  
  # names of edges in the matching
  matching_names <- match(m$matching, names(m$matching))
  
  # add a matching indicator to the edges
  bg <- bg |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      matching = to == matching_names[from]
    )
  
  # just the matching itself, as a graph
  mg <- bg |>
    tidygraph::activate(edges) |>
    dplyr::filter(matching)
  
  EE <- ends(mg, E(mg))
  return(as.numeric(gsub("s", "", EE[,2])))
}