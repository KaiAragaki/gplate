#' Turn data from 'plate form' to 'tidy form'
#'
#' @param df The data.frame in plate-form to be tidied
#' @param rownames Optional character. If there is a colname that specifies the
#'   row index, it will be arranged by this column, then dropped.
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' test_plate <- matrix(sample(1:10, 96, replace = TRUE), nrow = 8, ncol = 12)
#'
#' gp_unravel(test_plate)
gp_unravel <- function(df, rownames = NULL) {

  if(!is.null(rownames)) {
    df <- df |>
      dplyr::arrange(rownames) |>
      dplyr::select(-rownames)
  }

  df |>
    dplyr::as_tibble(.name_repair = "minimal") |>
    setNames(as.character(seq_len(ncol(df)))) |>
    dplyr::mutate(.row = seq_len(nrow(df))) |>
    dplyr::relocate(.row) |>
    tidyr::pivot_longer(cols = -.row, names_to = ".col") |>
    dplyr::mutate(.col = as.integer(.col))
}

gp_reravel <- function(df, row_name = ".row", col_name = ".col", values = "value") {
  df |>
    dplyr::select(row_name, col_name, values) |>
    tidyr::pivot_wider(names_from = col_name, values_from = values)
}
