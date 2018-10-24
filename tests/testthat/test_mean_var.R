context("Testing mean_var functions")

expect_list <- function(x){
  expect_is(mean_var(x), "list")
}

## TODO: tests to add
# unsorted factors
# missing x
# length(0) x
# length(1) x
# factor with only one level
# character with > 50 unique values

test_that("mean_var works for numeric values", {
  expect_list(rnorm(10))
})

test_that("mean_var works for integer values", {
  expect_list(1L:10L)
})

test_that("mean_var works for logical values", {
  expect_list(c(TRUE, FALSE, TRUE, FALSE))
})

test_that("mean_var works for factor values", {
  expect_list(factor(rep(c("x", "y", each = 4))))
})

test_that("mean_var works for character values", {
  expect_list(rep(c("x", "y", each = 4)))
})
