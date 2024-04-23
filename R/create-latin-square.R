#' Generate latin squares
#'
#' Create a latin square by first generating a
#' 1 x l_order latin rectangle and then embedding
#' it in a latin square.
#'
#' @param l_order Latin square order.
#'
#' @return A latin square.
#' @export
create_latin_square <- function(l_order) {
  tidyr::expand_grid(row = 1:1, column = 1:l_order) |>
  dplyr::mutate(symbol = 1:l_order) #|>
  #embedding(l_order = l_order, rows = 2:l_order)
}