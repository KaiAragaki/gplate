coord_map <- function(gp, type = c("row", "col"), margin) {

  type <- rlang::arg_match(type)

  if (type == "row") {
    dim_sec_par <- gp$nrow_sec_par
    dim_sec <- gp$nrow_sec
    backwards_corners <- c("bl", "br")
    margin_head_size <- margin$bottom
    margin_tail_size <- margin$top
    sec_par <- seq_len(gp$nrow_sec_par)
  } else {
    dim_sec_par <- gp$ncol_sec_par
    dim_sec <- gp$ncol_sec
    backwards_corners <- c("tr", "br")
    margin_head_size <- margin$right
    margin_tail_size <- margin$left
    sec_par <- seq_len(gp$ncol_sec_par)
  }

  is_backwards <- gp$start_corner %in% backwards_corners

  tt <-
    purrr::pmap(list(mh = margin_head_size, ds = dim_sec, mt = margin_tail_size), ~ tibble::tibble(n = c(..1, ..2, ..3), is_margin = c(T, F, T))) |> # TODO Should add section index at this point
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::mutate(my_data = purrr::map2(n, is_margin, ~ tibble::tibble(seq = seq_len(.x), my_margin = rep(.y, .x))),
                  my_data = purrr::map(my_data, \(x){if(is_backwards) rev(x$seq) else x$seq})) |> # TODO Should ADD a rel col instead of just changing it to one
    tidyr::unnest(my_data) |>
    dplyr::mutate(my_data = if_else(is_margin, NA_integer_, my_data)) |>
    dplyr::select(-n)

  if (gp$start_corner_par %in% backwards_corners) sec_par_rel <- rev(sec_par) else sec_par_rel <- sec_par

  tt <- replicate((nrow(coord_map) %/% nrow(tt)) + 1, tt, simplify = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::slice_head(n = nrow(coord_map))

  coord_map <-
    dplyr::tibble(sec_par, sec_par_rel) |>
    dplyr::bind_cols(tt) |>
    dplyr::arrange(sec_par) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  if(type == "row") gp$row_coord_map <- coord_map else gp$col_coord_map <- coord_map

  gp
}
