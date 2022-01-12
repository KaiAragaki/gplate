gp_serve <- function(gp) {
  gp$well_data |>
    dplyr::select(-dplyr::starts_with("."), .row, .col) |>
    dplyr::relocate(.row, .col)
}
