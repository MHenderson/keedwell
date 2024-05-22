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

#' Symbols missing from rows bipartite graph
#'

#' @param R A latin rectangle.
#' @param l_order Order of R.
#' @param n_rows Number of rows.
#' @param n_cols Number of columns.
#'
#' @return A bipartite graph.
#' @export
to_tidygraph_2 <- function(R, l_order, n_rows, n_cols) {
  
  ## VERTEX DATA FRAME
  row_vertices <- paste0("r", 1:n_rows)
  symbol_vertices <- paste0("s", 1:l_order)
  
  n_dummy_nodes <- l_order - n_rows
  dummy_vertices <- paste0("d", 1:n_dummy_nodes)
  
  l_nodes <- tibble::tibble(
    name = c(row_vertices, dummy_vertices, symbol_vertices),
    type = c(rep(TRUE, n_rows + n_dummy_nodes), rep(FALSE, l_order))
  )
  
  ## EDGE DATA FRAME
  f <- function(i) return(edge_tbl_2(R, i, l_order))
  
  l_edges <- purrr::map_df(1:n_rows, f)
  
  G <- tidygraph::tbl_graph(nodes = l_nodes, edges = l_edges)
  
  d_edges <- tibble::tibble(
    to = rep(symbol_vertices, as.numeric(l_order - n_cols - igraph::degree(G, symbol_vertices))),
    from = rep(dummy_vertices, each = l_order - n_cols)
  )
  
  tidygraph::tbl_graph(nodes = l_nodes, edges = dplyr::bind_rows(l_edges, d_edges))
  
}
