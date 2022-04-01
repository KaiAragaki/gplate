is_backwards <- function(dim, start_corner) {

  if (dim == "row") {
    start_corner %in% c("tr", "br")
  } else {
    start_corner %in% c("bl", "br")
  }

}

arrange_by_rel_dim <- function(gp, dim = c("row", "col"), start_corner) {

  dim <- rlang::arg_match(dim)

  is_bkwd <- is_backwards(start_corner, dim)

  dim_sec_par <- ifelse(dim == "row", rlang::expr(.row_sec_rel_par), rlang::expr(.col_sec_rel_par))

  if (!is_bkwd) {
    gp$well_data <- gp$well_data |> dplyr::arrange({{ dim_sec_par }})
  } else {
    gp$well_data <- gp$well_data |> dplyr::arrange(dplyr::desc({{ dim_sec_par }}))
  }

  gp

}

unroll_sec_dim_along_parent <- function(gp, dim, start_corner, section_prototype) {

  dim_sec_par <- ifelse(dim == "row", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  ndim_sec <- ifelse(dim == "row", gp$ncol_sec, gp$nrow_sec)
  index_name <- ifelse(dim == "row", ".index_col", ".index_row")

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.data$.sec, {{ dim_sec_par }}) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, \(x) {cbind(non_int_replicate(section_prototype, x), x)})) |>
    dplyr::mutate(temp := ({{ dim_sec_par }} - 1) %/% ndim_sec + 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(max_temp = max(temp, na.rm = TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate({{ index_name }} := ifelse(is_backwards(dim, start_corner), temp * -1  + 1 + max_temp, temp)) |>
    dplyr::select(-temp, -max_temp) |>
    tidyr::unnest(cols = data) |>
    dplyr::ungroup()

  gp

}
