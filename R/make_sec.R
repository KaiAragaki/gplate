make_sec <- function(gp, flow, wrap, nrow, ncol) {

  template_sec <-
    tidyr::expand_grid(.row_sec_par = gp$row_coord_map$.row_sec_par, .col_sec_par = gp$col_coord_map$.col_sec_par) |>
    dplyr::left_join(gp$row_coord_map) |>
    dplyr::left_join(gp$col_coord_map) |>
    dplyr::mutate(.is_margin = .col_is_margin | .row_is_margin)

  if (wrap) {
    if (flow == "row") {
      n_sec_nonflow <- ncol
      dim_flow_sec_rel <- template_sec$.row_sec_rel
      dim_flow_sec_rel_name <- rlang::expr(.row_sec_rel)
      dim_nonflow_sec_rel <- rlang::expr(.col_sec_rel)
    } else {
      n_sec_nonflow <- nrow
      dim_flow_sec_rel <- template_sec$.col_sec_rel
      dim_nonflow_sec_rel <- ".row_sec_rel"
    }

    n_first_dim <- sum(dim_flow_sec_rel == 1 & !template_sec$.is_margin, na.rm = TRUE)
    n_secs <- (n_first_dim %/% n_sec_nonflow) + 1
    template_sec <- template_sec |>
      dplyr::group_by({{dim_flow_sec_rel_name}}, .is_margin) |>
      dplyr::mutate({{dim_nonflow_sec_rel}} := rep_len(seq_len(n_sec_nonflow), n_first_dim) |> rep_len(dplyr::n()),
                    .sec = rep(seq_len(n_secs), each = n_sec_nonflow, length = n_first_dim) |> rep_len(dplyr::n())) |>
      dplyr::ungroup() |>
      dplyr::mutate(.col_sec = .col_sec_rel) |> # TESTING ONLY!
      dplyr::mutate(.sec = ifelse(.is_margin, NA_integer_, .sec))

  } else {
    template_sec <- template_sec |>
      dplyr::group_by(.row_sec_rel, .col_sec_rel)

    if(gp$start_corner == "tl") {
      template_sec <- dplyr::arrange(template_sec, .row_sec_par_rel, .col_sec_par_rel)
    } else if (gp$start_corner == "tr") {
      template_sec <- dplyr::arrange(template_sec, .row_sec_par_rel, desc(.col_sec_par_rel))
    } else if (gp$start_corner == "bl") {
      template_sec <- dplyr::arrange(template_sec, desc(.row_sec_par_rel), .col_sec_par_rel)
    } else {
      template_sec <- dplyr::arrange(template_sec, desc(.row_sec_par_rel), desc(.col_sec_par_rel))
    }

    template_sec <- template_sec |>
      dplyr::mutate(.sec = 1:dplyr::n(),
                    .sec = ifelse(.col_sec_rel != 1, NA_integer_, .sec)) |>
      dplyr::ungroup() |>
      tidyr::fill(.sec) |>
      dplyr::mutate(.sec = ifelse(.is_margin, NA_integer_, .sec))
  }

  gp$well_data <- dplyr::select(gp$well_data, -contains(colnames(template_sec)), .row_sec_par, .col_sec_par)
  gp$well_data <- gp$well_data |> dplyr::left_join(template_sec) |> dplyr::ungroup()

  gp

}
