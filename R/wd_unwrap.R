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
gp_unwrap <- function(gp, flow) {

  sec <- ifelse(flow == "row", gp$nrow_sec,  gp$ncol_sec)
  psec <- ifelse(flow == "row", gp$ncol_sec_par, gp$nrow_sec_par)
  wd <- gp$well_data

  if (flow == "row") {
    lane_index <- ((wd$.row_sec - 1) %/% sec)
    wd$.col_sec <- wd$.col_sec + (psec * lane_index)
    wd$.row_sec <- ((wd$.row_sec - 1) %% sec) + 1
  }

  if (flow == "col") {
    lane_index <- ((wd$.col_sec - 1) %/% sec)
    wd$.row_sec <- wd$.row_sec + (psec * lane_index)
    wd$.col_sec <- ((wd$.col_sec - 1) %% sec) + 1
  }
  gp$well_data <- wd
  gp
}

gp_rewrap <- function(gp, flow) {

}
