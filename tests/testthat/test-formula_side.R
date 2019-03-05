context("formula_side")

test_that("get_lhs works", {
  expect_equal(
    get_lhs(a~b),
    as.name("a")
  )
  expect_equal(
    get_lhs(~b),
    NULL
  )
  expect_equal(
    get_lhs(log(a)~b),
    quote(log(a))
  )
  expect_equal(
    get_lhs(lm(formula=log(d)~b, data=data.frame(d=1:3, b=6:4))),
    quote(log(d))
  )
})

test_that("get_rhs works", {
  expect_equal(
    get_rhs(a~b),
    as.name("b")
  )
  expect_equal(
    get_rhs(~b),
    as.name("b")
  )
  expect_equal(
    get_rhs(log(a)~b+c),
    quote(b+c)
  )
  expect_equal(
    get_rhs(lm(formula=log(d)~b, data=data.frame(d=1:3, b=6:4))),
    as.name("b")
  )
})
