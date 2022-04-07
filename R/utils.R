#' Recycle a df a non-integer number of times
#'
#' @param df `data.frame` to be replicated
#' @param measure Length-out of replication (not number of times). Can be a numeric,
#'   a vector, or a df. If a df, will use number of rows. If a vector, will use
#'   length.
#'
#' @return a `data.frame` with nrow `measure`
non_int_replicate <- function(df, measure) {

  if (!is.numeric(measure)) {
    if (is.data.frame(measure)) {
      measure <- nrow(measure)
    } else {
      measure <- length(measure)
    }
  }

  len <- (measure %/% nrow(df)) + 1

  replicate(len, df, simplify = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::slice_head(n = measure)

}

#' Check if axis moves in the canonical direction
#'
#' 'Forwards' is thought of 'left to right' when thinking about moving across
#' columns and 'top to bottom' when moving across rows
#' @param gp A `gp`
#' @param dim Character. A dimension, either "row" or "col".
#'
#' @return logical.
is_fwd <- function(gp, dim) {
  ifelse(dim == "row",
         gp$start_corner %in% c("tl", "tr"),
         gp$start_corner %in% c("tl", "bl"))
}

#' Take a set of numbers and flip them around on a number line
#'
#' @param gp A `gp`
#' @param dim Symbol. Column to flip. Should be the name of a column that exists in `gp$well_data`
#'
#' @return A vector.
#'
#' @details
#' This assumes the beginning is 1 and the end is maximum length of the
#' dimension. Note that this does NOT simply check the maximum value of the
#' supplied vector, but searches the supplied `gp` for a matching `ndim` column
#' that definitively tells how long the given dimension is supposed to be. This
#' is important, as sometimes a dimension is given that is longer than its
#' parent dimension, so the maximum numbers would not show in the vector.
flip_dim <- function(gp, dim) {
  # the dim is a symbol, so it needs special (arcane) treatment
  dim <- rlang::as_name(rlang::quo(!!dim))

  # rm leading dot
  no_dot <- substr(dim, 2, nchar(dim))

  n_dim <- gp[[paste0("n", no_dot)]]

  gp$well_data[[dim]] * -1 + 1 + n_dim
}

#' Performs `flip_dim` if necessary
#'
#' @param gp A `gp`
#' @param dim Symbol. Column to conditionally flip. Should be the name of a column that exists in `gp$well_data`
#' @param rel Character. Column to check `if_fwd` on. If TRUE, return `dim` as is. If FALSE (`rel` is backwards), flip the column.
#'
#' @return A vector that is flipped (see `flip_dim`) if `rel` is backwards (see `is_fwd`)
rel_dim <- function(gp, dim, rel) {
  if (is_fwd(gp, rel)) {
    gp$well_data[[dim]]
  } else {
    flip_dim(gp, dim)
  }
}
