#' Create coordinates for the axis that map back to the parents
#'
#' @param gp a `gp`
#' @param type Character, either row or col
#' @param margin margin, a named list of integers describing the margins for each side of the section
#'
#' @return a `gp`
#' @noRd
coord_map <- function(gp, type = c("row", "col"), margin) {

  type <- rlang::arg_match(type)

  if (type == "row") {
    dim_sec_par <- gp$nrow_sec_par
    sec_par <- seq_len(gp$nrow_sec_par)
    backwards_corners <- c("bl", "br")
  } else {
    dim_sec_par <- gp$ncol_sec_par
    sec_par <- seq_len(gp$ncol_sec_par)
    backwards_corners <- c("tr", "br")
  }

  if (gp$start_corner_par %in% backwards_corners) sec_par_rel <- rev(sec_par) else sec_par_rel <- sec_par

  # FIXME If tt is shorter than coord map it's going to repeat .sec. Will need
  # to group, arrange, then relabel or something. Or mutate the .sec col on
  # replicate.
  tt <- replicate((length(sec_par) %/% nrow(tt)) + 1, tt, simplify = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::slice_head(n = length(sec_par))

  coord_map <-
    dplyr::tibble(sec_par, sec_par_rel) |>
    dplyr::bind_cols(tt) |>
    dplyr::arrange(sec_par) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x)) # Don't rename upstream? Or filter for those that don't already have a .dim prefix?

  if(type == "row") gp$row_coord_map <- coord_map else gp$col_coord_map <- coord_map

  gp
}
