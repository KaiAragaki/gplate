
my_gp <- gp(2, 2)
by_row <- my_gp |> arrange_by_dim("row") |> gp_serve()
by_col <- my_gp |> arrange_by_dim("col") |> gp_serve()

test_that("arranging works", {
  expect_equal(by_row$.row[2], 1)
  expect_equal(by_col$.row[2], 2)
})

gp_tl <- gp(2, 2) |> gp_sec("test")
gp_tr <- gp(2, 2) |> gp_sec("test", start_corner = "tr")
gp_bl <- gp(2, 2) |> gp_sec("test", start_corner = "bl")
gp_br <- gp(2, 2) |> gp_sec("test", start_corner = "br")

test_that("correct corners are considered 'forward'", {
  skip_on_cran() # >.5s, high chance of success
  expect_true(is_fwd(gp_tl, "row"))
  expect_true(is_fwd(gp_tl, "col"))
  expect_true(is_fwd(gp_tr, "row"))
  expect_false(is_fwd(gp_tr, "col"))
  expect_false(is_fwd(gp_bl, "row"))
  expect_true(is_fwd(gp_bl, "col"))
  expect_false(is_fwd(gp_br, "row"))
  expect_false(is_fwd(gp_br, "col"))
})


test_that("relative dimensions are flipped", {
  expect_equal(flip_dim(gp_tr, ".row"), c(2, 2, 1, 1))
})
