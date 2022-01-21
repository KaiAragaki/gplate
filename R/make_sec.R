make_sec <- function(gp, flow) {

  if (flow == "row") {
    gp$well_data <- dplyr::arrange(gp$well_data, .row_sec_par_rel, .col_sec_par_rel, .row_sec, .col_sec)
  } else {
    gp$well_data <- dplyr::arrange(gp$well_data, .col_sec_par_rel, .row_sec_par_rel, .col_sec, .row_sec)
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.row_sec, .col_sec) |>
    dplyr::mutate(.sec = 1:dplyr::n())

  if (flow == "row") {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.sec = ifelse(.col_sec_rel == 1, .sec, NA_integer_))
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.sec = ifelse(.row_sec_rel == 1, .sec, NA_integer_))
  }

  gp$well_data <- gp$well_data |>
    dplyr::ungroup() |>
    tidyr::fill(.sec)

  gp
}
