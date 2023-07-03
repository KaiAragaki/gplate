# Slice out a rectangle from a gp by coordinates
# top, left, bottom, and right are all inclusive.
# Keep in mind that top left is 1,1

# Example:
# mp <- gplate::example_plate |> as_gp() #nolint
# gp_excise(mp, 2, 2, 4, 4)

#'  Slice out a smaller gp from a gp via coordinates
#'
#' @param gp A `gp`
#' @param top,left,bottom,right Integer. The coordinates of the corners.
#' Remember that TOP left is (1, 1)
#' @return A `gp` with top left coords = (1, 1)
#' @export
#' @examples
#' gplate::example_plate |> as_gp() |> gp_excise(2, 2, 4, 4)

gp_excise <- function(gp, top, left, bottom, right) {
  rows <- bottom - top + 1
  cols <- right - left + 1
  wd <- well_data(gp)
  ex <- dplyr::filter(wd,
                      .row >= top, .row <= bottom,
                      .col >= left, .col <= right) |>
    dplyr::select(!dplyr::matches("^(\\.col|\\.row|\\.sec)"))

  new_wd <- tidyr::expand_grid(.row = seq_len(rows), .col = seq_len(cols)) |>
    cbind(ex)

  gp(rows = rows, cols = cols,
     data = new_wd,
     tidy = TRUE)
}
