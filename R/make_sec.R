make_sec <- function(gp, flow) {

  if (flow == "row") {
    gp$well_data <- dplyr::arrange(gp$well_data, .sec_par, .row_sec_par_rel, .col_sec_par_rel, .row_sec, .col_sec)
  } else {
    gp$well_data <- dplyr::arrange(gp$well_data, .sec_par, .col_sec_par_rel, .row_sec_par_rel, .col_sec, .row_sec)
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.sec_par, .row_sec_rel, .col_sec_rel) |>
    dplyr::mutate(.sec = 1:dplyr::n())

  if (flow == "row") {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.sec = ifelse(.col_sec_rel == 1, .sec, NA_integer_))
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.sec = ifelse(.row_sec_rel == 1, .sec, NA_integer_))
  }



  fake_sec <- tidyr::expand_grid(.row_sec_par = gp$row_coord_map$.row_sec_par, .col_sec_par = gp$col_coord_map$.col_sec_par)

  fake_sec_full <- fake_sec |>  dplyr::left_join(gp$row_coord_map) |> dplyr::left_join(gp$col_coord_map)

  fake_sec_full <- fake_sec_full |>
    dplyr::group_by(.row_sec_rel, .col_sec_rel) |>
    dplyr::arrange(.row_sec_par_rel, .col_sec_par_rel) |>
    dplyr::mutate(.sec = 1:dplyr::n(),
                  .sec = ifelse(.col_sec_rel != 1, NA_integer_, .sec)) |>
    dplyr::ungroup() |>
    tidyr::fill(.sec)

  gp$well_data <- dplyr::select(gp$well_data, -contains(colnames(fake_sec_full)), .row_sec_par, .col_sec_par)
  gp$well_data <- gp$well_data |> dplyr::left_join(fake_sec_full) |> dplyr::ungroup()

  gp
}
