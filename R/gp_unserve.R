#' Turn a tidy `data.frame` into a `gp`
#'
#' An opposite to `serve()`
#'
#' @param x A tidy `data.frame` representation of plate data
#' @param row Character. The column that represents the rows. Should be numeric, with the lowest number representing the topmost row.
#' @param col Character. The name of the column that represents columns. Should be numeric, with the lowest number representing the leftmost column.
#' @param nrow Optional numeric, denoting the number of rows in the plate. If not supplied, will be imputed from the largest number in the 'row' column
#' @param ncol Optional numeric, denoting the number of columns in the plate. If not supplied, will be imputed from the largest number in the 'col' column
#'
#' @return a `gp`
#' @export
gp_unserve <- function(x, row = ".row", col = ".col", nrow = NULL, ncol = NULL) {

  if (sum(duplicated(data.frame(x[[row]], x[[col]]))) > 0) {
    rlang::abort(c("Duplicate entries for a single well, aborting",
                   "i" = "Consider pivoting to a wider format, or using dplyr::distict()"))
  }

  if (!is.numeric(x[[row]])) {
    rlang::inform(glue::glue("{row} is not numeric, coercing."))
    x[[row]] <- as.numeric(x[[row]])
  }

  if (!is.numeric(x[[col]])) {
    rlang::inform(glue::glue("{col} is not numeric, coercing."))
    x[[col]] <- as.numeric(x[[col]])
  }

  if (is.null(nrow)) {
    nrow <- max(x[[row]], na.rm = TRUE)
    rlang::inform(glue::glue("`nrow` was NULL. Assuming there are {nrow} rows in this plate."))
  }

  if (is.null(ncol)) {
    ncol <- max(x[[col]], na.rm = TRUE)
    rlang::inform(glue::glue("`nrow` was NULL. Assuming there are {ncol} cols in this plate."))
  }

  if (!all(x[[row]] %in% 1:nrow)) {
    rlang::inform("Some rows are not present in the data - filling with NA")
  }

  if (!all(x[[col]] %in% 1:ncol)) {
    rlang::inform("Some cols are not present in the data - filling with NA")
  }

  data <- tidyr::expand_grid(.row = 1:nrow, .col = 1:ncol) |>
    dplyr::left_join(x, by = c(.row = row, .col = col))

  gp::gp(nrow, ncol, data, tidy = TRUE)

}
