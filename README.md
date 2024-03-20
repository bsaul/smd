# `smd`

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/smd)](https://CRAN.R-project.org/package=smd)
[![R-CMD-check](https://github.com/bsaul/smd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bsaul/smd/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/bsaul/smd/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bsaul/smd?branch=master)
<!-- badges: end -->

An `R` package for computing the standardized mean difference between two groups for various data types. 

```r
x <- rnorm(100)
g <- rep(1:2, each = 50)
smd(x, g)
```

See [using smd](https://bsaul.github.io/smd/articles/smd_usage.html) for more details.
