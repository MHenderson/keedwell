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
next_row <- function(R, l_order) {
  bg <- to_tidygraph(R, l_order)
  
  m <- igraph::max_bipartite_match(bg)
  
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
  
  EE <- igraph::ends(mg, igraph::E(mg))
  return(as.numeric(gsub("s", "", EE[,2])))
}

#' Find a random new row for a latin rectangle
#'
#' @param R A latin rectangle
#' @param i Number of columns to add
#' @param l_order Order of R
#'
#' @return A latin square with more rows.
next_row_random <- function(R, i, l_order) {
  
  R |>
    dplyr::bind_rows(
      tibble::tibble(
        row = rep(i, l_order),
        column = 1:l_order,
        symbol = sample(1:l_order, l_order)
      )
    )
  
}

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
#' @param R A latin rectangle
#' @param i Number of columns to add
#' @param l_order Order of R
#'
#' @return A latin rectangle with more rows.
next_row_matching <- function(R, i, l_order) {
  bg <- to_tidygraph(R, l_order)
  
  m <- igraph::max_bipartite_match(bg)
  
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
  
  EE <- igraph::ends(mg, igraph::E(mg))
  
  R |>
    dplyr::bind_rows(
      tibble::tibble(
        row = rep(i, l_order),
        column = 1:l_order,
        symbol = as.numeric(gsub("s", "", EE[,2]))
      )
    )
}
