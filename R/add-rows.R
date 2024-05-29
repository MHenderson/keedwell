#' Embed latin rectangle in a latin square
#'
#' Input is a latin rectangle as a data frame with
#' variables for row, column and symbol. Output is
#' a latin square in the same format which contains
#' the given latin rectangle in the first rows.
#'
#' Use can optionally provide a vector of row indices.
#' Only those rows will be filled if that optional
#' vector is provided.
#'
#' @param R latin rectangle
#' @param rows empty rows to be filled
#' @param strategy row filling strategy
#'
#' @return A latin rectangle
#' @export
add_rows <- function(R, rows, strategy = next_row_matching) {
  
  # we assume that the dimension equals
  # the number of columns
  l_order <- length(unique(R$column))
  
  for (i in rows) {
    
    R <- strategy(R, i, l_order)
    
  }
  
  return(R)

}