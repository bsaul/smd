context("Testing the smd() functions")

# TODO: tests to add
# handling of unsorted grouping variable
# handling of unsorted factor (and character) levels
# standard error computations are correct
# once implemented add checks for changing reference group


## Comparing to other packages ####

compare_packages <- function(data){

  smdval <- abs(smd(data$x, data$g)$estimate)
  # compare to TableOne package
  # NOTE: for boolean variables in small datasets, tableone and smd will have
  # small differences due the way the variance is computed.

  if(is.logical(data$x)){
    expect_equal(
      smdval,
      tableone::ExtractSmd(tableone::CreateTableOne("x", "g", data)),
      tolerance = 0.001,
      check.attributes = FALSE
    )
  } else {
    expect_equal(
      smdval,
      tableone::ExtractSmd(tableone::CreateTableOne("x", "g", data)),
      check.attributes = FALSE
    )
  }


  # compare stddiff package
  expect_equal(
    round(smdval, 3), #Not sure why stddiff rounds, but it does
    stddiff::stddiff.numeric(data,"g","x")[7])
}

set.seed(123)
dg <- list(
  list(type = "numeric",
       x    = rnorm(60)),
  list(type = "integer",
       x    = rep(rep(1L:3L, each = 10), times = 2)),
  list(type = "factor",
       x    = factor(rep(rep(c("x", "y", "z"), each = 10), times = 2))),
  list(type = "factor_unsorted",
       x    = factor(rep(rep(c("x", "y", "z"), times = 10), times = 2))),
  list(type = "character",
       x    = rep(rep(c("x", "y", "z"), each = 10), times = 2)),
  list(type = "boolean",
       x    = as.logical(sample(rbinom(60, size = 1, prob = .5))))
)


for(i in seq_along(dg)){

  test_that(sprintf("smd() matches other packages for %s values", dg[[i]]$type), {
    dt <- data.frame(
        g = rep(c("A", "B"), each = 30),
        x = dg[[5]]$x
      )
    compare_packages(dt)
  })

}


##

test_that("smd() works/does not as appropriate with matrices", {
  X <- matrix(rnorm(100), ncol = 5)
  g <- rep(c("A", "B"), each = 10)
  expect_is(smd(x = X, g = g), "numeric")
})

test_that("smd() works/does not as appropriate with lists", {
  X <- replicate(rnorm(20), n = 5, simplify = FALSE)
  g <- rep(c("A", "B"), each = 10)
  expect_is(smd(x = X, g = g), "data.frame")

  # checking lists of different types
  expect_is(smd(x = purrr::map(dg, ~ .x$x), g = rep(c("A", "B"), each = 30)), "data.frame")
})

test_that("smd() works/does not as appropriate with data.frames", {
  X <- as.data.frame(replicate(rnorm(20), n = 5, simplify = FALSE), col.names = 1:5)
  g <- rep(c("A", "B"), each = 10)
  expect_is(smd(x = X, g = g), "data.frame")


})

test_that("smd() gives error on if x and g have different length", {
  x <- rnorm(20)
  g <- rep(c("A", "B"), each = 5)
  expect_error(smd(x = x, g = g))

  x <- rnorm(20)
  g <- rep(c("A", "B"), each = 20)
  expect_error(smd(x = x, g = g))
})

test_that("smd() gives error on if g does not have 2 levels", {
  x <- rnorm(20)
  g <- rep(c("A"), times = 20)
  expect_error(smd(x = x, g = g))

  x <- rnorm(30)
  g <- rep(c("A", "B", "C"), each = 30)
  expect_error(smd(x = x, g = g))
})


test_that("smd() runs if g is not sorted", {
  x <- rnorm(40)
  g <- rep(c("A", "B"), times = 20)

  expect_is(smd(x = x, g = g), "data.frame")

})
