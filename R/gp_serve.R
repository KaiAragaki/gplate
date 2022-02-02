#' Extract useful, tidy data from a gp object
#'
#' @param gp a gp
#'
#' @return a `tibble`, with `.row` and `.col`, as well as any created section names and data values.
#' @export
#'
#' @examples
#'
#' gp(16, 24) |>
#'   gp_sec("my_sec", nrow = 9, ncol = 7, labels = c("sample_1", "sample_2", "sample_3")) |>
#'   gp_serve()
gp_serve <- function(gp) {
  gp$well_data |>
    dplyr::select(-dplyr::starts_with("."), .row, .col) |>
    dplyr::relocate(.row, .col)
}
