#' Case insensitive version of \code{\%in\%}
#'
#' @usage find \%IN\% table
#'
#' @param find The element(s) to look up in the vector or matrix.
#' @param table The vector or matrix in which to look up the element(s).
#'
#' @return A logical vector.
#' @export
#'
#' @examples letters[1:4] %IN% LETTERS
`%IN%` <- function(find, table) {
  return(toupper(find) %in% toupper(table));
}
