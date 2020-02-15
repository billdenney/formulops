#' Modify a formula by finding some part of it and replacing it with a new
#' value.
#'
#' @details Replacement occurs at the first match, so if the replacement list
#'   would modify something in the find list, that change will not occur (make
#'   two calls to the function for that effect).  See the "Replacement is not
#'   sequential" examples below.
#'
#'   A special call can be used to expand a formula.  If an expansion of
#'   arguments is desired to change a single function argument to multiple
#'   arguments, `formulops_expand()` can be used.  (See the examples.)
#'
#' @param formula The formula to modify (may also be a call)
#' @param find A call or name (or list thereof) to search for within the formula
#' @param replace A call or name (or list thereof) to replace the \code{find}
#'   values
#' @param add_parens Add parentheses if \code{replace} is not a name or if it is
#'   not already something in parentheses?
#' @return \code{formula} modified
#' @examples
#' modify_formula(a~b, find=quote(a), replace=quote(c))
#' modify_formula(a~b, find=quote(a), replace=quote(c+d))
#' modify_formula(a~b/c, find=quote(b/c), replace=quote(d))
#' # Replacement is not sequential
#' modify_formula(a~b/c, find=list(quote(b/c), quote(d)), replace=list(quote(d), quote(e)))
#' modify_formula(a~b/c+d, find=list(quote(b/c), quote(d)), replace=list(quote(d), quote(e)))
#' # Expanding arguments to functions is possible
#' modify_formula(a~b(c), find=quote(c), replace=quote(formulops_expand(d, e)))
#' @export
modify_formula <- function(formula, find, replace, add_parens=FALSE) {
  if (xor(is.list(find), is.list(replace))) {
    stop("Both or neither of `find` and `replace` must be a list.")
  }
  if (!is.list(find)) {
    find <- list(find)
    replace <- list(replace)
  }
  if (add_parens) {
    for (i in seq_along(replace)) {
      if (is.name(replace[[i]])) {
        # do nothing
      } else if (replace[[i]][[1]] == as.name("(")) {
        # do nothing
      } else {
        replace[[i]] <- {
          tmp <- quote((a))
          tmp[[2]] <- replace[[i]]
          tmp
        }
      }
    }
    # It has now been taken care of.
    add_parens <- FALSE
  }
  if (length(find) != length(replace)) {
    stop("`find` and `replace` lists must be the same length.")
  }
  if (length(find) == 0) {
    stop("`find` and `replace` lists must have length > 0.")
  }

  replaced <- FALSE
  for (idx in seq_along(find)) {
    if (identical(formula, find[[idx]])) {
      formula <- replace[[idx]]
      replaced <- TRUE
    }
    if (replaced) {
      break
    }
  }
  if (!replaced && length(formula) > 1) {
    for (idx in rev(seq_along(formula))) {
      new_value <-
        modify_formula(
          formula[[idx]],
          find,
          replace,
          add_parens=add_parens
        )
      if (length(new_value) > 1 && identical(as.name("formulops_expand"), new_value[[1]])) {
        old_formula <- formula
        current_idx <- idx
        # Expand the values
        for (new_idx in seq_len(length(new_value) - 1) + 1) {
          formula[[current_idx]] <- new_value[[new_idx]]
          names(formula)[current_idx] <-
            if (is.null(names(new_value))) {
              ""
            } else {
              names(new_value)[new_idx]
            }
          current_idx <- current_idx + 1
        }
        if (idx < length(old_formula)) {
          # Put the rest of the old formula back in place
          for (new_idx in seq(idx+1, length(old_formula))) {
            formula[[current_idx]] <- old_formula[[new_idx]]
            names(formula)[current_idx] <-
              if (is.null(names(old_formula))) {
                ""
              } else {
                names(old_formula)[new_idx]
              }
            current_idx <- current_idx + 1
          }
        }
      } else {
        formula[[idx]] <- new_value
      }
    }
  }
  formula
}

#' Add parentheses to a base formula to ensure clarify for order of operations
#'
#' @param base_formula A formula or name to modify with parts to be replaced
#'   named `a` and `b` for \code{e1} and {e2}.
#' @param e1,e2 a name or call
#' @return \code{base_formula} revised such that `a` and `b` have parentheses
#'   around them if \code{e1} and \code{e2} have lengths > 1, respectively.
#' @examples
#' add_parens_base_formula(a~b, quote(c+d), quote(e))
#' @noRd
add_parens_base_formula <- function(base_formula, e1, e2) {
  if (length(e1) > 1) {
    base_formula <- modify_formula(base_formula, find=quote(a), replace=quote((a)))
  }
  if (!missing(e2)) {
    if (length(e2) > 1) {
      base_formula <- modify_formula(base_formula, find=quote(b), replace=quote((b)))
    }
  }
  base_formula
}

#' Perform a mathematical operation on two formula
#'
#' @details The method for combination depends if the two formula are one- or
#'   two-sided.
#' 
#' If both formula are one-sided, the right hand side (RHS) of both are added
#' together with additional parentheses added, if parentheses appear to be
#' needed.  If both formula are two-sided, the left hand side (LHS) and RHS are
#' separately added.  If one formula is one-sided and the other is two-sided,
#' the LHS is selected from the two-sided formula and the RHS follows rules as
#' though two one-sided formula were added.
#'
#' @param op The operation to perform either as a name or something that can be
#'   coerced into a name.
#' @param e1,e2,x The formulae to operate on
#' @return \code{e1} and \code{e2} combined by the operation with the
#'   environment from \code{e1}.  See Details.
#' @examples
#' op_formula("+", a~b, c~d)
#' op_formula("+", a~b, ~d)
#' op_formula("+", ~b, c~d)
#' op_formula("+", ~b, ~d)
#' op_formula("-", a~b)
#' op_formula("-", -a~b) # Dumb, but accurate
#' op_formula("-", -a~b, c~-d) # Dumb, but accurate
#' 
#' log(a~b)
#' @export
op_formula <- function(op, e1, e2) {
  if (!is.name(op)) {
    op <- as.name(op)
  }
  if (!(length(e1) %in% 2:3)) {
    # is this possible?
    stop("`e1` must be a one- or two-sided formula.") # nocov
  }
  if (!missing(e2) && !(length(e2) %in% 2:3)) {
    # is this possible?
    stop("`e2` must be a one- or two-sided formula.") # nocov
  }
  if (missing(e2)) {
    # unary operator
    base_formula <-
      modify_formula(quote(-a), find=as.name("-"), replace=op)
    out <- e1
    out[[2]] <-
      modify_formula(
        add_parens_base_formula(base_formula, e1=e1[[2]]),
        find=quote(a),
        replace=e1[[2]]
      )
    if (length(e1) == 3) {
      out[[3]] <-
        modify_formula(
          add_parens_base_formula(base_formula, e1=e1[[3]]),
          find=quote(a),
          replace=e1[[3]]
        )
    }
  } else if (length(e1) != length(e2)) {
    if ((length(e1) == 2) &
        (length(e2) == 3)) {
      out <- e2
      environment(out) <- environment(e1)
      first_rhs <- e1
      second_rhs <- e2[c(1, 3)]
    } else if ((length(e1) == 3) &
               (length(e2) == 2)) {
      out <- e1
      first_rhs <- e1[c(1, 3)]
      second_rhs <- e2
    } else {
      stop("Unknown how to handle a formula with different lengths that are != 2 or 3") # nocov
    }
    out[[3]] <- op_formula(op, first_rhs, second_rhs)[[2]]
  } else if (length(e1) == length(e2)) {
    out <- e1
    # they are both one- or two-sided
    base_formula <-
      modify_formula(
        quote(a+b),
        find=as.name("+"),
        replace=op
      )
    # one- or two-sided
    out[[2]] <-
      modify_formula(
        formula=add_parens_base_formula(base_formula, e1[[2]], e2[[2]]),
        find=list(quote(a), quote(b)),
        replace=list(e1[[2]], e2[[2]])
      )
    if (length(e1) == 3) {
      # two-sided
      out[[3]] <-
        modify_formula(
          formula=add_parens_base_formula(base_formula, e1[[3]], e2[[3]]),
          find=list(quote(a), quote(b)),
          replace=list(e1[[3]], e2[[3]])
        )
    }
  }
  out
}

#' @rdname op_formula
#' @details \code{multiply_formula} Multiply two formula (identical to \code{(a~b) * (c~d)}
#' @export
multiply_formula <- function(e1, e2) {
  op_formula("*", e1, e2)
}

#' @rdname op_formula
#' @details \code{divide_formula} Divide two formula (identical to \code{(a~b) / (c~d)}
#' @export
divide_formula <- function(e1, e2) {
  op_formula("/", e1, e2)
}

#' @rdname op_formula
#' @details \code{add_formula} Add two formula (identical to \code{(a~b) + (c~d)}
#' @export
add_formula <- function(e1, e2) {
  op_formula("+", e1, e2)
}

#' @rdname op_formula
#' @details \code{subtract_formula} Multiply two formula (identical to \code{(a~b) - (c~d)}
#' @export
subtract_formula <- function(e1, e2) {
  op_formula("-", e1, e2)
}

#' @rdname op_formula
#' @details \code{Ops.formula} Supports generic binary operators and a couple of
#'   unary operators (see ?Ops).
#' @export
Ops.formula <- function(e1, e2) {
  if (missing(e2)) {
    # Unary operators
    op_formula(.Generic, e1)
  } else {
    op_formula(.Generic, e1, e2)
  }
}

#' @rdname op_formula
#' @details \code{Math.formula} Supports generic unary operators (see ?Math).
#' @param ... Ignored.
#' @export
Math.formula <- function(x, ...) {
  if (!(length(x) %in% 2:3)) {
    # Is this possible?
    stop("`x` must be a one- or two-sided formula.") # nocov
  }
  base_formula <-
    modify_formula(quote(a(b)), find=quote(a), replace=as.name(.Generic))
  out <- x
  out[[2]] <- modify_formula(base_formula, find=quote(b), replace=out[[2]])
  if (length(out) == 3) {
    out[[3]] <- modify_formula(base_formula, find=quote(b), replace=out[[3]])
  }
  out
}
