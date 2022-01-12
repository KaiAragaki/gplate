#' Extract useful, tidy data from a gp object
#'
#' @param gp a gp
#'
#' @return a `tibble`, with `.row` and `.col`, as well as any created section names and data values.
#' @export
#'
#' @examples
gp_serve <- function(gp) {
  gp$well_data |>
    dplyr::select(-dplyr::starts_with("."), .row, .col) |>
    dplyr::relocate(.row, .col)
}
