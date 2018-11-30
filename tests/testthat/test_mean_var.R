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
# character with > 50 unique values (seriously?, but done)

test_that("n_mean_var works for numeric values", {
  expect_list(rnorm(10))
})

test_that("n_mean_var works for integer values", {
  expect_list(1L:10L)
})

test_that("n_mean_var works for logical values", {
  expect_list(c(TRUE, FALSE, TRUE, FALSE))


  res <- n_mean_var(x = c(TRUE, TRUE, FALSE, FALSE), w = c(0, 1, 1, 0))
  expect_equal(res$n, 2)
  expect_equal(res$mean, 0.5)
})

test_that("n_mean_var works for factor values", {
  expect_list(factor(rep(c("x", "y", each = 4))))
})

test_that("n_mean_var works for character values", {
  expect_list(rep(c("x", "y", each = 4)))

  res <- n_mean_var(x = c("A", "B", "B", "A"), w = c(0, 1, 1, 0))
  expect_equal(res$n, 2)
  expect_equal(res$mean, c(0, 1), check.attributes = FALSE)
})

test_that("n_mean_var works for unsorted factors", {
  expect_list(factor(c(1,2,2,2,1,1)))
})

test_that("n_mean_var works for missing x", {
  expect_list(c(1, 2, 3, NA))
})

test_that("n_mean_var works for length(0) x", {
  expect_error(n_mean_var(NULL))
})

test_that("n_mean_var works for length(1) x", {
  expect_list(1)
})

test_that("n_mean_var works for factor with only 1 level", {
  expect_list(factor(c(1,1,1,1,1,1)))
})

test_that("n_mean_var returns error if x and w have different length", {
  expect_error(n_mean_var(x = c(1, 1, 1), w = c(1, 2)))
})


test_that("n_mean_var returns correct values with/without weights", {
  x <- factor(rep(LETTERS[1:4], times = 2))
  w <- rep(0:1, times = 4) # w zeros out all A and C
  res <- n_mean_var(x, w)
  expect_equal(res$n, sum(w))
  expect_equal(res$mean, c(A = 0, B = 0.5, C = 0, D = 0.5), check.attributes = FALSE)

  res <- n_mean_var(x)
  expect_equal(res$n, length(x))
  expect_equal(as.numeric(res$mean),
               c(A = 0.25, B = 0.25, C = 0.25, D = 0.25),
               check.attributes = FALSE)

  # Checking factors when all weights are 0 (hopefully avoiding NaN issues)
  x <- factor(rep(LETTERS[1:4], times = 2))
  w <- rep(0, 8)
  res <- n_mean_var(x, w)
  expect_equal(res$n, 0)
  expect_equal(res$mean, rep(0, 4), check.attributes = FALSE)
})

char_generator <- function(n){
  paste0(sample(letters, n, replace = TRUE), sample(999, n))
}

x <- char_generator(51)
test_that("n_mean_var works for >50 unique values", {
  expect_warning(n_mean_var(x), "more than 50 levels")
})
