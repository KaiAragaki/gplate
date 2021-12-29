
dev_plot_gp <- function(gp, name) {


  wd <- gp$well_data

  ggplot(wd, aes(x = col, y = row, color = as.factor({{ name }}))) +
    geom_point(size = 10) +
    scale_y_reverse()

}

dev_plot_wd <- function(wd, name) {

  ggplot(wd, aes(x = col, y = row, color = as.factor({{ name }}))) +
    geom_point(size = 10) +
    scale_y_reverse()
}

# new_gp(nrow = 8L, ncol = 12L) |>
#   gp_sec("hi", start_corner = "bl", flow = "col",  nrow = 5, ncol = 5) |>
#   gp_sec("bye", start_corner = "tr", flow = "row", nrow = 2, ncol = 5) |>
#   dev_plot_gp(lane_v)
#
#
#
# # w96 <- new_gp(rows = 8L, cols = 12L)
# #
# # a <- new_gp(rows = 8L, cols = 12L) |>
# #   gp_sec(name = "my_section", start_corner = "br", flow = "row", wrap = TRUE, nrow = 5, ncol = 5) |>
# #   gp_sec(name = "sec_sec", start_corner = "tl", flow = "row", wrap = FALSE, nrow = 2, ncol = 2) |>
# #   dev_plot_gp(sec)
# #
# #
# # a <- new_gp(rows = 8L, cols = 12L) |>
# #   gp_sec(name = "my_sec1", start_corner = "br", flow = "col", nrow = 3, ncol = 5) |>
# #   gp_sec(name = "my_sec2", start_corner = "br", flow = "col", nrow = 2, ncol = 3) |>
# #   dev_plot_gp(sec)
# #
# # wd <- a$well_data
# #
# # nrow <- 3
# # ncol <- 3
# #
# # # Relative row gives the row the sections sees as it fills it - that is, the
# # # order in which it fills it per section.
# #
# # # I need to switch this to the order that we see it: always left top to bottom
# # # right.
# #
# # # We're still talking in terms of the section, but now we need to 'turn' the coordinates.
# #
# # #
# # # wd_2 <- wd |>
# # #   group_by(my_section) |>
# # #   mutate(row_sec_rel = ((.data$row_rel - 1) %% nrow) + 1,
# # #          col_sec_rel = ((.data$col_rel - 1) %% ncol) + 1,
# # #          row_sec = -row_sec_rel + nrow + 1,
# # #          col_sec = col_sec_rel)
# # # This flips it, but should only happen when it needs to be flipped.
# # # That is, it's not general
# #
# # # 2021-12-27
# #
# # # Current problem:
# #
# # # lane_v/h needs to refer to parent section information but axes must be
# # # relative first. This is going to take a lot of copy/paste that I don't feel
# # # like doing rn (and eventual factorization once I see a pattern emerge)
