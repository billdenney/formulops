#' A substituting formula helps clarify a formula where the parameters are more
#' simply described in separate formulae.
#'
#' @details Formula are substituted in order.  Substitutions may not have the
#'   same left hand side.
#'
#' @param x The base formula
#' @param substitutions A list of supporting formula.
#' @param ... Supporting formula of the form \code{x1~x2+x3*x4...}
#' @return A \code{substituting_formula} object which may be coerced into a
#'   single formula with an \code{as.formula()} method or printed as a list of
#'   formulae.
#' @examples
#' foo <- substituting_formula(y~x1+x2, x1~x3*x4, x2~x5/x6+x7)
#' as.formula(foo)
#' @export
substituting_formula <- function(x, ...) {
  as_substituting_formula(x=x, substitutions=list(...))
}

#' @describeIn substituting_formula Generate and check substituting_formula
#' @export
as_substituting_formula <- function(x, substitutions) {
  if (!is.list(substitutions)) {
    stop("`substitutions` must be a list.  (Try `substituting_formula()` to provide formula not as a list.)")
  }
  if (length(substitutions)) {
    # Verify that the same substitution isn't happening in multiple equations
    all_lhs <- lapply(X=substitutions, FUN=get_lhs)
    if (any(sapply(X=all_lhs, FUN=is.null))) {
      stop("All substitution formulae must be 2-sided")
    }
    for (idx_1 in seq_len(length(substitutions)-1)) {
      for (idx_2 in seq(idx_1+1, length(substitutions))) {
        if (identical(all_lhs[[idx_1]], all_lhs[[idx_2]])) {
          stop(
            "The left hand side of substitution ", idx_1, " and ", idx_2,
            " are identical and no left hand sides may match."
          )
        }
      }
    }
  }
  ret <-
    list(
      base=x,
      substitutions=substitutions
    )
  class(ret) <- "substituting_formula"
  ret
}

#' Convert a substituting_formula object into a regular formula.
#' 
#' @param x,object the substituting_formula object
#' @param ... Ignored
#' @param env The environment for the resulting formula
#' @return A formula with values substituted.
#' @export
formula.substituting_formula <- function(x, ...) {
  ret <- x$base
  for (i in seq_along(x$substitutions)) {
    ret <-
      modify_formula(
        formula=ret,
        find=get_lhs(x$substitutions[[i]]),
        replace=get_rhs(x$substitutions[[i]])
      )
  }
  ret
}

#' @rdname formula.substituting_formula as.formula Add the environment
#' @export
as.formula.substituting_formula <- function(object, env=parent.frame()) {
  ret <- formula.substituting_formula(object)
  environment(ret) <- env
  ret
}
