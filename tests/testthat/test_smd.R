context("Testing the smd() functions")

# TODO: tests to add
# compare to TableOne package
# compare to stddiff package

test_that("smd() works as expected for numeric values", {

  dt <- data.frame(
    g = rep(c("A", "B"), each = 50),
    x = rnorm(100)
  )

  smdval <- abs(smd(dt$x, dt$g))

  # compare to TableOne package
  expect_equal(
    smdval,
    tableone::ExtractSmd(tableone::CreateTableOne("x", "g", dt)),
    check.attributes = FALSE
  )

  # compare stddiff package
  expect_equal(
    round(smdval, 3), #Not sure why stddiff rounds, but it does
    stddiff::stddiff.numeric(dt,"g","x")[7])
})
