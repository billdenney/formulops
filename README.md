# formulops

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/billdenney/formulops.svg?branch=master)](https://travis-ci.org/billdenney/formulops)
[![Codecov test coverage](https://codecov.io/gh/billdenney/formulops/branch/master/graph/badge.svg)](https://codecov.io/gh/billdenney/formulops?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/formulops)](https://CRAN.R-project.org/package=formulops)
<!-- badges: end -->

The goal of formulops is to assist with formula modification in R treating
formulae as standard mathematical operations as used with `nlme::nlme()` and
`lme4::nlmer()` (for treatment as statistical formula see the `Formula`
package).

## Installation

You can install the released version of formulops from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("formulops")
```

## Example

A few common examples of modifying a formula are given below.

``` r
library(formulops)
# Replace a with c in the formula
modify_formula(a~b, find=quote(a), replace=quote(c))
# Replace a with c+d in the formula
modify_formula(a~b, find=quote(a), replace=quote(c+d))
# More complex parts can be replaced, too
modify_formula(a~b/c, find=quote(b/c), replace=quote(d))
# Multiple replacements can occur simultaneously
modify_formula(a~b/c+d, find=list(quote(b/c), quote(d)), replace=list(quote(d), quote(e)))
# Function arguments can be expanded
modify_formula(a~b(c), find=quote(c), replace=quote(formulops_expand(d, e)))
```

A substituting formula is a simple way to generate a complex formula from several simpler formulae.  Parentheses are appropriately added, if required.

``` r
foo <- substituting_formula(y~x1+x2, x1~x3*x4, x2~x5/x6+x7)
as.formula(foo)
```
