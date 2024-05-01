## Comparing to other packages ####

compare_packages <- function(data) {
  smdval <- abs(smd(x = data$x, g = data$g)$estimate)
  # compare to TableOne package
  # NOTE: for boolean variables in small datasets, tableone and smd will have
  # small differences due the way the variance is computed.
  expect_equal(
    smdval,
    tableone::ExtractSmd(tableone::CreateTableOne("x", "g", data)),
    tolerance = 0.01,
    check.attributes = FALSE
  )

  skip_if(
    is.character(data$x),
    "On R 4.0 (it seems), stddiff() results in error when x is character"
    # stddiff::stddiff.numeric(data, "g", "x")
    # stats:::aggregate.default(temp[, 2], by = list(temp[, 1]), FUN = mean)
    # stats::aggregate.data.frame(as.data.frame(x), ...)
  )

  # compare stddiff package
  expect_equal(
    round(smdval, 3), # Not sure why stddiff rounds, but it does
    stddiff::stddiff.numeric(data, "g", "x")[7],
    tolerance = 0.01
  )
}

set.seed(123)
dg <- list(
  list(
    type = "numeric",
    x = rnorm(60)
  ),
  list(
    type = "integer",
    x = rep(rep(1L:3L, each = 10), times = 2)
  ),
  list(
    type = "factor",
    x = factor(rep(rep(c("x", "y", "z"), each = 10), times = 2))
  ),
  list(
    type = "factor_unsorted",
    x = factor(rep(rep(c("x", "y", "z"), times = 10), times = 2))
  ),
  list(
    type = "character",
    x = rep(rep(c("x", "y", "z"), each = 10), times = 2)
  ),
  list(
    type = "boolean",
    x = as.logical(sample(rbinom(60, size = 1, prob = .5)))
  )
)
