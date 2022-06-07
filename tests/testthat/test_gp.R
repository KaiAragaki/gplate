test_that("only well args of standard size make gps", {
  expect_equal(wells(gp(wells = 96)), 96)
  expect_equal(gp(wells = 96)$ncol, 12)
  expect_equal(gp(wells = 96)$nrow, 8)
  expect_error(gp(wells = 22), "wells is not of a known format")
})

test_that("dims <1 error descriptively", {
  expect_error(gp(rows = 0, cols = 12), "Dimensions must be positive integers")
})

