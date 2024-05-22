#' Add new columns to a latin rectangle
#'
#' @param R A latin rectangle
#' @param cols Indices of columns to add
#' @param l_order Dimension of latin square
#' @param strategy Strategy for filling columns
#'
#' @return A latin rectangle
#' @export
add_cols <- function(R, cols, l_order, strategy = next_col_matching) {
  
  for (i in cols) {
    
    R <- strategy(R, i, l_order)
    
  }
  
  return(R)
  
}
