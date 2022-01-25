make_sec <- function(gp, flow, wrap) {

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


  gp$well_data <- dplyr::ungroup(gp$well_data)


  fake_sec <- tidyr::expand_grid(.row_sec_par = gp$row_coord_map$.row_sec_par, .col_sec_par = gp$col_coord_map$.col_sec_par)

  fake_sec_full <- fake_sec |> dplyr::left_join(gp$row_coord_map) |> dplyr::left_join(gp$col_coord_map)

  if (wrap) {
    if (flow == "row") {
      length <- sum(fake_sec_full$.row_sec_rel == 1, na.rm = TRUE)
      n_blocks <- (length %/% gp$ncol_sec) + 1
      times <- length(unique(fake_sec_full$.row_sec_rel))
      col <- rep_len(seq_len(gp$ncol_sec), length) |> rep_len(nrow(fake_sec_full))
      sec <- rep(seq_len(n_blocks), each = gp$ncol_sec, length = length) |> rep_len(nrow(fake_sec_full))
      fake_sec_full <- fake_sec_full |>
        dplyr::arrange(.data$.row_sec_rel) |>
        dplyr::mutate(.col_sec_rel = col,
                      .sec = sec,
                      .col_sec = .col_sec_rel) # TESTING ONLY!

    }
  } else {
    fake_sec_full <- fake_sec_full |>
      dplyr::group_by(.row_sec_rel, .col_sec_rel) |>
      dplyr::arrange(.row_sec_par_rel, .col_sec_par_rel) |>
      dplyr::mutate(.sec = 1:dplyr::n(),
                    .sec = ifelse(.col_sec_rel != 1, NA_integer_, .sec)) |>
      dplyr::ungroup() |>
      tidyr::fill(.sec)
  }

  gp$well_data <- dplyr::select(gp$well_data, -contains(colnames(fake_sec_full)), .row_sec_par, .col_sec_par)
  gp$well_data <- gp$well_data |> dplyr::left_join(fake_sec_full) |> dplyr::ungroup()

  gp

}
