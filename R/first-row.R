#' First row in in natural order
#'
#' @param n Number of columns
#'
#' @return A 1 x n latin rectangle with first row 1,...,n
#' @export
first_row_natural <- function(n) {
  tidyr::expand_grid(row = 1:1, column = 1:n) |>
    dplyr::mutate(symbol = 1:n)
}