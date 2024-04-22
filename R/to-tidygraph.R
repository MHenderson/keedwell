#' Symbols missing from columns bipartite graph
#'
#' Input is a latin rectangle as a data frame with
#' variables for row, column and symbol. Output is
#' a tidygraph representing the bipartite graph with
#' vertices for columns and symbols and edges representing
#' symbols missing from columns.
#'
#' @param R A latin rectangle.
#' @param l_order Order of R.
#'
#' @return A bipartite graph.
#' @export
to_tidygraph <- function(R, l_order = 3) {
  
  ## VERTEX DATA FRAME
  column_vertices <- paste0("c", 1:l_order)
  symbol_vertices <- paste0("s", 1:l_order)
  
  l_nodes <- tibble::tibble(
    name = c(column_vertices, symbol_vertices),
    type = c(rep(TRUE, l_order), rep(FALSE, l_order))
  )
  
  ## EDGE DATA FRAME
  f <- function(i) return(edge_tbl(R, i, l_order))
  
  l_edges <- purrr::map_df(1:l_order, f)
  
  tidygraph::tbl_graph(nodes = l_nodes, edges = l_edges)

}