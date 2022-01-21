add_map <- function(gp, type = c("row", "col")) {

  len_sec_par <- ifelse(type == "row", gp$nrow_sec_par, gp$ncol_sec_par)
  if (type == "row") map <- gp$row_coord_map else map <- gp$col_coord_map
  order2 <- ifelse(type == "row", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  orderowt <- ifelse(type == "col", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  order1 <- ifelse(type == "row", rlang::expr(.col_rel), rlang::expr(.row_rel))

  len <- (gp$wells %/% len_sec_par) + 1
  map_to_size <- replicate(len, map, simplify = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::slice_head(n = gp$wells)

  gp$well_data <- gp$well_data |>
    dplyr::arrange(.sec, rlang::eval_tidy(order1), rlang::eval_tidy(order2)) |>
    dplyr::select(-dplyr::contains(colnames(map_to_size)), rlang::as_string(orderowt))

  gp$well_data <- gp$well_data |>
    dplyr::left_join(map, by = rlang::as_string(orderowt))
  gp

}
