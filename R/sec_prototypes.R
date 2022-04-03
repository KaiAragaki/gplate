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
         gp$start_corner %in% c("tl", "bl"),
         gp$start_corner %in% c("tl", "tr"))
}

unroll_sec_dim_along_parent <- function(gp, dim) {

  dim_sec_par <- ifelse(dim == "row", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  ndim_sec <- ifelse(dim == "row", gp$ncol_sec, gp$nrow_sec)
  ndim_sec_par <- ifelse(dim == "row", gp$ncol_sec_par, gp$nrow_sec_par)
  index_name <- ifelse(dim == "row", ".index_col", ".index_row")
  if(dim == "row") {
    section_prototype <- gp[["row_unit"]]
  } else {
    section_prototype <- gp[["col_unit"]]
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.data$.sec, {{ dim_sec_par }}) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, \(x) {cbind(non_int_replicate(section_prototype, x), x)}))

  if(is_fwd(gp, dim)) {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }})
  } else {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }} * -1 + 1 + ndim_sec_par)
  }

  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ index_name }} := (temp - 1) %/% ndim_sec + 1) |>
    dplyr::select(-temp) |>
    tidyr::unnest(cols = data) |>
    dplyr::ungroup()

  gp
}
