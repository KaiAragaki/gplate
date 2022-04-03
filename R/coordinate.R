#' Create a dimension pattern 'unit' of a section
#'
#' @param gp a `gp`
#' @param type Character. Either 'row' or 'col'.
#' @param margin Named list of integers defining the size of margins at each edge.
#'
#' @return a tibble
coordinate <- function(gp, type = c("row", "col"), margin) {

  type <- rlang::arg_match(type)

  if (type == "row") {
    dim_sec <- gp$nrow_sec
    margin_head <- margin$bottom
    margin_tail <- margin$top
  } else {
    dim_sec <- gp$ncol_sec
    margin_head <- margin$right
    margin_tail <- margin$left
  }

  dim_unit <- purrr::pmap(list(margin_head, dim_sec, margin_tail),
              ~ tibble::tibble(n = c(..1, ..2, ..3),
                               is_margin = c(T, F, T))) |>
    dplyr::bind_rows(.id = "map_sec") |> # Map sec refers to the 'section within the section' if a vector with length > 1 was supplied as an arg to nrow/ncol
    dplyr::mutate(my_data = purrr::map(.data$n, ~ tibble::tibble(sec = seq_len(.x))),
                  sec_rel = purrr::map(.data$my_data, \(x){if(is_fwd(gp, type)) x$sec else rev(x$sec)})) |>
    tidyr::unnest(c(.data$my_data, .data$sec_rel)) |>
    dplyr::mutate(sec = ifelse(.data$is_margin, NA_integer_, .data$sec),
                  sec_rel = ifelse(.data$is_margin, NA_integer_, .data$sec_rel)) |>
    dplyr::select(-.data$n) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  gp[[paste0(type, "_unit")]] <- dim_unit

  gp
}
