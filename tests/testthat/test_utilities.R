# context("Testing utilities")

# Check for two levels
x1 <- c(1, 2) # Should return warning
x2 <- c(NA, 1, 2) # Should return warning
x3 <- c(1, 2, 3) # Should return nothing
x4 <- c(1,1,1,1) # Should return nothing

test_that("check_for_two_levels output as expected", {
  expect_warning(check_for_two_levels(x1))
  expect_warning(check_for_two_levels(x2))
  expect_warning(check_for_two_levels(x3), NA)
  expect_warning(check_for_two_levels(x4), NA)
})


# Check Apply a function pairwise along a list

x1 <- c(1, 2, 3, 4)
x2 <- c(1, 2, 3, NA)

test_that("check lapplyFUNpairwise output as expected", {
  expect_equal(lapplyFUNpairwise(x1, `+`, 2), list(3, 5, 6))
  expect_equal(lapplyFUNpairwise(x1, min, 2), list(1, 2, 2))
  expect_error(lapplyFUNpairwise(x2, `+`, 2))
})
