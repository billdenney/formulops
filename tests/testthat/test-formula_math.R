context("Math and modification for formulae")

test_that("modify_formula tests its inputs", {
  expect_error(
    modify_formula(a~b, find=list(), replace=quote(c)),
    regexp="Both or neither of `find` and `replace` must be a list.",
    fixed=TRUE
  )
  expect_error(
    modify_formula(a~b, find=list(quote(a), quote(b)), replace=list(quote(c))),
    regexp="`find` and `replace` lists must be the same length.",
    fixed=TRUE
  )
  expect_error(
    modify_formula(a~b, find=list(), replace=list()),
    regexp="`find` and `replace` lists must have length > 0.",
    fixed=TRUE
  )
})

test_that("modify_formula replaces bits", {
  expect_equal(
    modify_formula(a~b, find=quote(a), replace=quote(c)),
    c~b
  )
  expect_equal(
    modify_formula(a~b, find=quote(a), replace=quote(c+d)),
    c+d~b
  )
  expect_equal(
    modify_formula(a~b/c, find=quote(b/c), replace=quote(d)),
    a~d
  )
})

test_that("modify_formula adds parentheses correctly", {
  expect_equal(
    modify_formula(a~b, find=quote(a), replace=quote(c), add_parens=TRUE),
    c~b
  )
  expect_equal(
    modify_formula(a~b, find=quote(a), replace=quote(c+d), add_parens=TRUE),
    (c+d)~b
  )
  expect_equal(
    modify_formula(a~b/c, find=quote(b/c), replace=quote(d), add_parens=TRUE),
    a~d
  )
})

test_that("modify_formula is not sequential", {
  expect_equal(
    modify_formula(a~b/c, find=list(quote(b/c), quote(d)), replace=list(quote(d), quote(e))),
    a~d
  )
  expect_equal(
    modify_formula(a~b/c+d, find=list(quote(b/c), quote(d)), replace=list(quote(d), quote(e))),
    a~d+e
  )
})

test_that("add_parens_base_formula gives the correct parentheses", {
  expect_equal(
    add_parens_base_formula(a~b, quote(c), quote(e)),
    a~b
  )
  expect_equal(
    add_parens_base_formula(a~b, quote(c+d), quote(e)),
    (a)~b
  )
  expect_equal(
    add_parens_base_formula(a~b, quote(c), quote(e+f)),
    a~(b)
  )
  expect_equal(
    add_parens_base_formula(a~b, quote(c+d), quote(e+f)),
    (a)~(b)
  )

  expect_false(
    identical(
      add_parens_base_formula(a~b, quote(c+d), quote(e)),
      a~(b)
    )
  )
  expect_false(
    identical(
      add_parens_base_formula(a~b, quote(c+d), quote(e)),
      a~b
    )
  )
})

test_that("op_formula works with binary operations with all combinations of one- and two-sided formula.", {
  expect_equal(
    op_formula("+", a~b, c~d),
    a+c~b+d
  )
  expect_equal(
    op_formula("+", a~b, ~d),
    a~b+d
  )
  expect_equal(
    op_formula("+", ~b, c~d),
    c~b+d
  )
  expect_equal(
    op_formula("+", ~b, ~d),
    ~b+d
  )
})

test_that("op_formula works with unary operations with all combinations of one- and two-sided formula.", {
  expect_equal(
    op_formula("-", a~b),
    -a~-b
  )
  expect_equal(
    op_formula("-", -a~b),
    -(-a)~-b
  )
  expect_equal(
    op_formula("-", ~b),
    ~-b
  )
  expect_equal(
    op_formula("-", ~-b),
    ~-(-b)
  )
})

test_that("op_formula adds parentheses correctly to all combinations of one- and two-sided formula.", {
  # one-sided
  expect_equal(
    op_formula("*", ~c+d, ~g+h),
    ~(c+d)*(g+h)
  )  
  expect_equal(
    op_formula("*", ~c, ~g+h),
    ~c*(g+h)
  )  
  expect_equal(
    op_formula("*", ~c+d, ~g),
    ~(c+d)*g
  )  
  expect_equal(
    op_formula("*", ~c, ~g),
    ~c*g
  )  

  # two-sided
  expect_equal(
    op_formula("*", a+b~c+d, e+f~g+h),
    (a+b)*(e+f)~(c+d)*(g+h)
  )  
  expect_equal(
    op_formula("*", b~c+d, e+f~g+h),
    b*(e+f)~(c+d)*(g+h)
  )  
  expect_equal(
    op_formula("*", a+b~c, e+f~g+h),
    (a+b)*(e+f)~c*(g+h)
  )  
  expect_equal(
    op_formula("*", a+b~c+d, f~g+h),
    (a+b)*f~(c+d)*(g+h)
  )  
  expect_equal(
    op_formula("*", a+b~c+d, e+f~h),
    (a+b)*(e+f)~(c+d)*h
  )  

  # Mixed one- and two-sided
  expect_equal(
    op_formula("*", ~c, e~g),
    e~c*g
  )  
  expect_equal(
    op_formula("*", ~c, e+f~g),
    e+f~c*g
  )  
  expect_equal(
    op_formula("*", ~c, e~g+h),
    e~c*(g+h)
  )  
  expect_equal(
    op_formula("*", ~c, e+f~g+h),
    e+f~c*(g+h)
  )  
  expect_equal(
    op_formula("*", ~c+d, e~g),
    e~(c+d)*g
  )  
  expect_equal(
    op_formula("*", ~c+d, e+f~g),
    e+f~(c+d)*g
  )  
  expect_equal(
    op_formula("*", ~c+d, e~g+h),
    e~(c+d)*(g+h)
  )  
  expect_equal(
    op_formula("*", ~c+d, e+f~g+h),
    e+f~(c+d)*(g+h)
  )  
})

test_that("Ops and convenience functions are equal", {
  expect_equal(
    multiply_formula(a~b, c~d),
    (a~b) * (c~d)
  )
  expect_equal(
    divide_formula(a~b, c~d),
    (a~b) / (c~d)
  )
  expect_equal(
    add_formula(a~b, c~d),
    (a~b) + (c~d)
  )
  expect_equal(
    subtract_formula(a~b, c~d),
    (a~b) - (c~d)
  )
})

test_that("Unary Ops work", {
  expect_equal(
    -(a~b),
    -a~-b
  )
})

test_that("Math works", {
  expect_equal(
    log(a~b),
    log(a)~log(b)
  )
  expect_equal(
    log(~b),
    ~log(b)
  )
})
