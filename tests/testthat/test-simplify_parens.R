context("simplify_parens")

test_that("simplify_parens doesn't change things that shouldn't change", {
  expect_equal(simplify_parens(as.name("a")), as.name("a"))
  expect_equal(simplify_parens(quote(foo(bar, baz))), quote(foo(bar, baz)))
  expect_equal(simplify_parens(a~b), a~b)
  expect_equal(simplify_parens(~a), ~a)
})

test_that("simplify_parens doesn't change things that shouldn't change", {
  expect_equal(simplify_parens(as.name("a")), as.name("a"))
  expect_equal(simplify_parens(~foo*((qux(bar, baz)))), ~foo*(qux(bar, baz)))
  expect_equal(simplify_parens(a~b), a~b)
  expect_equal(simplify_parens(~a), ~a)
})
