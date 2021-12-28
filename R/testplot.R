
dev_plot_gp <- function(gp, name) {


  wd <- gp$well_data

  ggplot(wd, aes(x = col, y = row, color = as.factor({{ name }}))) +
    geom_point(size = 10) +
    scale_y_reverse()

}
#
# a <- new_gp(rows = 8L, cols = 12L) |>
#   gp_sec("hi", start_corner = "bl", flow = "col",  nrow = 3, ncol = 3) |>
#   gp_sec("bye", start_corner = "tl", flow = "row")
#

## Current problem:

# Suppose we have this:

new_gp(rows = 8L, cols = 12L) |>
  gp_sec(name = "my_section", start_corner = "bl", flow = "col", nrow = 3, ncol = 3) |>
  dev_plot_gp(sec)

# If we want to add an additional layer that starts from, say, tl, how do we
# inform - generally - this layer that there are missing rows?

# One solution: Always let the sections bleed out to a full integer and keep
# those data, then crop to size whenever presented.

# Another: Swap coordinate systems. At the tail end of producing one section,
# define (from ul corner, regardless of start_corner) rows and cols.

# Frankly unsure how to implement (haven't thought about it yet) but it seems
# possible and the best option

a <- new_gp(rows = 8L, cols = 12L) |>
  gp_sec(name = "my_sec1", start_corner = "br", flow = "col", nrow = 3, ncol = 5) |>
  gp_sec(name = "my_sec2", start_corner = "br", flow = "col", nrow = 2, ncol = 3) |>
  dev_plot_gp(sec)

wd <- a$well_data

nrow <- 3
ncol <- 3

# Relative row gives the row the sections sees as it fills it - that is, the
# order in which it fills it per section.

# I need to switch this to the order that we see it: always left top to bottom
# right.

# We're still talking in terms of the section, but now we need to 'turn' the coordinates.

#
# wd_2 <- wd |>
#   group_by(my_section) |>
#   mutate(row_sec_rel = ((.data$row_rel - 1) %% nrow) + 1,
#          col_sec_rel = ((.data$col_rel - 1) %% ncol) + 1,
#          row_sec = -row_sec_rel + nrow + 1,
#          col_sec = col_sec_rel)
# This flips it, but should only happen when it needs to be flipped.
# That is, it's not general

# 2021-12-27

# Current problem:

# lane_v/h needs to refer to parent section information but axes must be
# relative first. This is going to take a lot of copy/paste that I don't feel
# like doing rn (and eventual factorization once I see a pattern emerge)
