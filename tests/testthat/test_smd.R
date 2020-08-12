context("Testing the smd() functions")

for(i in seq_along(dg)){



  test_that(sprintf("smd() matches other packages for %s values", dg[[i]]$type), {

    skip_on_ci()
    skip_on_cran()

    dt <- data.frame(
        g = rep(c("A", "B"), each = 30),
        x = dg[[i]]$x
      )
    compare_packages(dt)
  })

}

test_that("smd() returns correct values in specific cases", {
  x <- rep(0, 10)
  g <- rep(c("A", "B"), each = 5)
  w <- rep(0:1, each = 5)
  # means in both groups are 0; some weights are 0;
  expect_equal(smd(x, g, w)$estimate, 0)

  # means in one group is not 0; some weights are 0;
  x <- rep(0:1, times = c(7, 3))
  gBx <- c(0, 0, 1, 1, 1)
  expect_equal(smd(x, g, w)$estimate, -0.6/sqrt((var(gBx) * 4 /5)/ 2))

  # means in one group is not 0; some weights are 0;
  x <- rep(0:1, times = c(7, 3))
  w <- rep(0:1, times = c(6, 4))
  gBx <- c(0, 0, 1, 1, 1)
  expect_equal(smd(x, g, w)$estimate, -0.75/sqrt( (sum(w[6:10]*(gBx - 0.75)^2)/4)/2) )

  # means in one group is not 0; some weights are 0;
  x <- rep(0:1, times = c(7, 3))
  w <- c(rep(0:1, times = c(6, 3)), 0)
  gBx <- c(0, 0, 1, 1, 1)
  expect_equal(smd(x, g, w)$estimate, -(2/3)/sqrt( (sum(w[6:10]*(gBx - (2/3))^2)/3)/2) )

  # means in both group is not 0; some weights are 0;
  x <- rep(c(0, 1, 1, 1, 1), times = 2)
  w <- rep(c(0, 1, 1, 1, 0), times = 2)

  expect_equal(smd(x, g, w)$estimate, 0)

  ## Checking factors
  x <- factor(rep(LETTERS[1:4], times = 4))
  g <- rep(c("a", "b"), each = 8)
  w <- c(rep(0:1, times = 4), rep(0:1, each = 4))

  ra <- c(0, .5, 0, .5)
  SSa <- diag(ra) - outer(ra, ra)
  rb <- c(.25, .25, .25, 0.25)
  SSb <- diag(rb) - outer(rb, rb)
  d <- ra - rb
  SS <- (SSa + SSb)/2

  expect_equal(sqrt(t(d) %*% (MASS::ginv(SS) %*% d)), smd(x, g, w)$estimate, check.attributes = FALSE)

  w <- rep(0:1, each = 8)
  ra <- c(0, 0, 0, 0)
  SSa <- diag(ra) - outer(ra, ra)
  rb <- c(.25, .25, .25, 0.25)
  SSb <- diag(rb) - outer(rb, rb)
  d <- ra - rb
  SS <- (SSa + SSb)/2

  expect_equal(sqrt(t(d) %*% (MASS::ginv(SS) %*% d)), smd(x, g, w)$estimate, check.attributes = FALSE)

})


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


test_that("smd() runs if g is an unsorted grouping variable", {
  x <- rnorm(40)
  g <- rep(c("A", "B"), times = 20)

  expect_is(smd(x = x, g = g), "data.frame")

})


test_that("smd() runs if g is unsorted factor (and character) levels", {
  create_g <- sample(0:1, 20, replace = TRUE)
  create_g_factor <- factor(create_g, labels = c("high", "low"))
  x <- rnorm(20)
  g <- create_g_factor
  expect_is(smd(x = x, g = g), "data.frame")
})


test_that("smd() runs with NA values", {
  g <- rep(c("A", "B"), each = 30)
  x <- rnorm(60)
  x[sample(1:60, 5)] <- NA
  expect_error(smd(x, g, na.rm = FALSE))
  expect_is(smd(x, g, na.rm = TRUE), "data.frame")

})


test_that("smd() works when changing gref (reference group)", {
  g <- rep(c("A", "B"), each = 30)
  x <- rnorm(60)
  expect_equal(smd(x, g, gref = 1)$estimate, -smd(x, g, gref = 2)$estimate)

  g <- rep(1:2, each = 30)
  x <- rnorm(60)
  expect_equal(smd(x, g, gref = 1)$estimate, -smd(x, g, gref = 2)$estimate)

  g <- rep(1:3, each = 20)
  x <- rnorm(60)
  expect_equal(smd(x, g, gref = 1)$estimate[2], -smd(x, g, gref = 3)$estimate[1])
})


for(i in c(1,3:length(dg))){
  # Skipping the integer check: this gives a check_for_two_levels warning()
  # TODO: how to do I want to handle this case?
  test_that(sprintf("smd() runs for various data type %s", dg[[i]]$type), {
    dt <- data.frame(
      g = rep(c("A", "B", "C"), each = 20),
      x = dg[[i]]$x
    )

   expect_is(smd(dt$x, dt$g, gref = 1), "data.frame")
   expect_is(smd(dt$x, dt$g, gref = 2), "data.frame")
   expect_is(smd(dt$x, dt$g, gref = 3), "data.frame")
  })

}

test_that("smd() when factor has one level returns 0", {
  x <- factor(rep("No", 10))
  g <- factor(rep(c("Control", "Treat"), 5))
  w <- rep(c(3.12, 1.47), 5)
  expect_equal(smd(x, g, w)$estimate, 0)
})
