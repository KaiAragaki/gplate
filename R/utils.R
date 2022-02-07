#' Recycle a df a non-integery number of times
#'
#' @param df `data.frame` to be replicated
#' @param measure Length of replication (not number of times). Can be a numeric,
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
