context("substituting_formla")

test_that("error checks work", {
  expect_error(
    as_substituting_formula(a~b, 1),
    regexp="`substitutions` must be a list",
    fixed=TRUE
  )
  expect_error(
    substituting_formula(a~b, b~1, b~2),
    regexp="The left hand side of substitution 1 and 2 are identical and no left hand sides may match",
    fixed=TRUE
  )
  expect_error(
    substituting_formula(a~b, ~1, b~2),
    regexp="All substitution formulae must be 2-sided",
    fixed=TRUE
  )
  expect_equal(
    substituting_formula(a~b, b~1, c~2),
    structure(
      list(
        base=a~b,
        substitutions=list(b~1, c~2)
      ),
      class="substituting_formula"
    )
  )
})

test_that("formula are created correctly", {
  expect_equal(
    formula(substituting_formula(a~b, b~c*d, d~e+f)),
    a~c*(e+f)
  )
  expect_equal(
    as.formula(substituting_formula(a~b, b~c*d, d~e+f), env=emptyenv()),
    `environment<-`(a~c*(e+f), emptyenv())
  )
})
