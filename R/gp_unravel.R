#' Turn data from 'plate form' to 'tidy form'
#'
#' @param matrix
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' test_plate <- matrix(sample(1:10, 96, replace = TRUE), nrow = 8, ncol = 12)
#'
#' gp_unravel(test_plate)
gp_unravel <- function(df) {
  df |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    setNames(as.character(1:ncol(df))) |>
    dplyr::mutate(row = 1:nrow(df)) |>
    dplyr::relocate(row) |>
    tidyr::pivot_longer(cols = -row, names_to = "col")
}
