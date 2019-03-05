#' Remove extraneous parentheses from a formula.
#' 
#' @param x The formula (or call) to simplify
#' @return The simplified formula
#' @examples
#' simplify_parens(((a))~((b+c)))
#' @export
simplify_parens <- function(x) {
  paren <- as.name("(")
  if (is.name(x)) {
    ret <- x
  } else {
    # Simplfiy all children
    for (i in seq_along(x)) {
      x[[i]] <- simplify_parens(x[[i]])
    }
    # Simplify the parent
    if (x[[1]] == paren & length(x) == 2) {
      if (is.name(x[[2]])) {
        # A simple name doesn't need parentheses
        x <- x[[2]]
      } else if (x[[2]][[1]] == paren) {
        # Parentheses do not need duplication
        x <- x[[2]]
      }
    }
    x
  }
}
