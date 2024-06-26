#' Symbols missing from columns edge data frame
#'
#' Constructs a data frame representing the edges
#' of a bipartite graph based on a latin rectangle
#' where the graph has an edge for every symbol not
#' already used in a column.
#'
#' Acutally, this is just for one column.
#'
#' @param R latin rectangle
#' @param i column
#' @param l_order size of latin square R is going to be embedded into
#'
#' @return The edge data frame.
edge_tbl <- function(R, i, l_order = 3) {
  
  all_symbols <- 1:l_order
  
  # symbols used in column i
  used <- R |> dplyr::filter(column == i) |> dplyr::pull(symbol)
  
  # symbols missing from column i
  missing <- setdiff(all_symbols, used)
  
  # edge data frame for column i
  edge_df <- tibble::tibble(
    to = paste0("s", missing)
  ) |>
    dplyr::mutate(from = paste0("c", i))
  
  return(edge_df)
}

#' Symbols missing from rows data frame
#'
#' @param R latin rectangle
#' @param i row index
#' @param l_order size of latin square R is going to be embedded into
#'
#' @return The edge data frame.
edge_tbl_2 <- function(R, i, l_order = 3) {
  
  all_symbols <- 1:l_order
  
  # symbols used in row i
  used <- R |> dplyr::filter(row == i) |> dplyr::pull(symbol)
  
  # symbols missing from row i
  missing <- setdiff(all_symbols, used)
  
  # edge data frame for column i
  edge_df <- tibble::tibble(
    to = paste0("s", missing)
  ) |>
    dplyr::mutate(from = paste0("r", i))
  
  return(edge_df)
}