test_that("section cannot exceed parent dimension", {
  expect_error(gp(2, 5) |> gp_sec("test", nrow = 3), "Child section exceeds dimensions of its parent, and wrap = FALSE")
})
