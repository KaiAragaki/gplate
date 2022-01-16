#' Unwrap a plate into a single long lane
#'
#' @param wd Well data
#' @param flow either "row" or "col"
#' @param nrow_sec The number of rows of a section, including margins
#' @param ncol_sec The number of columns of a section, including margins
#'
#' @return A tibble
#' @details This returns a plate that IS nrow_sec tall or ncol_sec wide.
#'   Therefore, it MUST be converted back into its normal plate form using its
#'   cognate function
wd_unwrap <- function(wd, flow, nrow_sec, ncol_sec) {
  if (flow == "row") {
    wd$.col <- wd$.col + (max(wd$.col) * ((wd$.row - 1) %/% nrow_sec))
    wd$.row <- ((wd$.row - 1) %% nrow_sec) + 1
    return(wd)
  }

  if (flow == "col") {
    wd$.row <- wd$.row + (max(wd$.row) * ((wd$.col - 1) %/% ncol_sec))
    wd$.col <- ((wd$.col - 1) %% ncol_sec) + 1
    return(wd)
  }
}
