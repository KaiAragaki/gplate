coordinate <- function(gp, type = c("row", "col"), margin) {

  type <- rlang::arg_match(type)

  if (type == "row") {
    dim_sec <- gp$nrow_sec
    backwards_corners <- c("bl", "br")
    margin_head <- margin$bottom
    margin_tail <- margin$top
  } else {
    dim_sec <- gp$ncol_sec
    backwards_corners <- c("tr", "br")
    margin_head <- margin$right
    margin_tail <- margin$left
  }

  is_backwards <- gp$start_corner %in% backwards_corners

  tt <-
    purrr::pmap(list(margin_head, dim_sec, margin_tail), ~ tibble::tibble(n = c(..1, ..2, ..3), is_margin = c(T, F, T))) |>
    dplyr::bind_rows(.id = "map_sec") |>
    dplyr::mutate(my_data = purrr::map(n, ~ tibble::tibble(sec = seq_len(.x))),
                  sec_rel = purrr::map(my_data, \(x){if(is_backwards) rev(x$sec) else x$sec})) |>
    tidyr::unnest(c(my_data, sec_rel)) |>
    dplyr::mutate(sec = ifelse(is_margin, NA_integer_, sec),
                  sec_rel = ifelse(is_margin, NA_integer_, sec_rel)) |>
    dplyr::select(-n) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  if(type == "row") gp$row_coord <- coord_map else gp$col_coord <- coord_map

  gp
}
