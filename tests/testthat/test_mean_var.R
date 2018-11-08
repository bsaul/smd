context("Testing n_mean_var functions")

expect_list <- function(x){
  expect_is(n_mean_var(x), "list")
}

## TODO: tests to add
# unsorted factors (done)
# missing x (done)
# length(0) x (done)
# length(1) x (done)
# factor with only one level (done)
# character with > 50 unique values (serious?)

test_that("n_mean_var works for numeric values", {
  expect_list(rnorm(10))
})

test_that("n_mean_var works for integer values", {
  expect_list(1L:10L)
})

test_that("n_mean_var works for logical values", {
  expect_list(c(TRUE, FALSE, TRUE, FALSE))
})

test_that("n_mean_var works for factor values", {
  expect_list(factor(rep(c("x", "y", each = 4))))
})

test_that("n_mean_var works for character values", {
  expect_list(rep(c("x", "y", each = 4)))
})

test_that("n_mean_var works for unsorted factors", {
  expect_list(factor(c(1,2,2,2,1,1)))
})

test_that("n_mean_var works for missing x", {
  expect_list(c(1, 2, 3, NA))
})

# Error occured
test_that("n_mean_var works for length(0) x", {
  expect_list(c(NULL))
})

test_that("n_mean_var works for length(1) x", {
  expect_list(1)
})

test_that("n_mean_var works for factor with only 1 level", {
  expect_list(factor(c(1,1,1,1,1,1)))
})

x <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
test_that("n_mean_var works for 26 unique values", {
  expect_list(x)
})