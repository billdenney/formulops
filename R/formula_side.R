#' Extract formula parts
#'
#' @param x A formula (or something that can be coerced to a formula) to extract
#'   a part from
#' @return The requested part of the formula as a name or call or \code{NULL} if
#'   it does not exist.
#' @name formula_side
#' @importFrom stats as.formula
NULL

#' @describeIn formula_side Extract the left hand side (NULL for one-sided
#'   formula).
#' @export
get_lhs <- function(x) {
  if (!inherits(x, what="formula")) {
    x <- as.formula(x)
  }
  if (length(x) == 2) {
    NULL
  } else {
    x[[2]]
  }
}

#' @describeIn formula_side Extract the right hand side.
#' @export
get_rhs <- function(x) {
  if (!inherits(x, what="formula")) {
    x <- as.formula(x)
  }
  if (length(x) == 2) {
    x[[2]]
  } else {
    x[[3]]
  }
}
