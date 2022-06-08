test_that("section cannot exceed parent dimension", {
  expect_error(gp(2, 5) |> gp_sec("test", nrow = 3), "Child section exceeds dimensions of its parent, and wrap = FALSE")
})


gp_tl <- gp(2, 2) |> gp_sec("test")
gp_br <- gp(2, 2) |> gp_sec("test", start_corner = "br")


test_that("absolute dimensions flip only when start_corner differs", {
  expect_equal(gp_tl$col_unit$.col_sec_rel, gp_tl$col_unit$.col_sec)
  expect_false(all(gp_br$col_unit$.col_sec_rel == gp_br$col_unit$.col_sec))
})
