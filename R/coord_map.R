coord_map <- function(gp, type = c("row", "col"), start_corner, margin, nrow, ncol) {

  # When getting parents that have margins, these margins need to be persistent
  # so that the child section is aware of them as well

  # OR it needs to be such that the parents only pass on the non-margin rows
  # Getting from option A to option B is pretty easy (filter).

  # When we set wrap = TRUE, and flow = "row" (default),
  # we only need to change .col_sec

  type <- rlang::arg_match(type)
  dim_sec_par <- ifelse(type == "row", gp$nrow_sec_par, gp$ncol_sec_par)
  dim_sec <- ifelse(type == "row", nrow, ncol)

  backwards_corners <- if(type == "row") c("bl", "br") else c("tr", "br")
  is_backwards <- start_corner %in% backwards_corners



  margin_head_size <- if (type == "row") margin$bottom else margin$left
  margin_tail_size <- if (type == "row") margin$top else margin$right

  dim_sec_mar <- dim_sec + margin_head_size + margin_tail_size

  margin_head <- (dim_sec_mar + 1 - margin_head_size):(dim_sec_mar + 1)
  margin_tail <- 0:margin_tail_size



  sec_par <- if (type == "row") gp$nrow_sec_par else gp$ncol_sec_par
  sec_par <- seq_len(sec_par)



  temp <-
    dplyr::tibble(sec_par) |>
    dplyr::mutate(sec = rep_len(seq_len(dim_sec + margin_head_size + margin_tail_size), dim_sec_par)) |>
    dplyr::rowwise() |>
    dplyr::mutate(is_margin = (sec %in% margin_head) | (sec %in% margin_tail),
                  sec = ifelse(is_margin, NA_integer_, sec)) |>
    dplyr::group_by(is_margin) |>
    dplyr::mutate(sec = (sec - sec[1] + 1L + dim_sec) %% dim_sec,
                  sec = ifelse(sec == 0, dim_sec, sec)) |>
    dplyr::rowwise() |>
    dplyr::mutate(sec_rel = ifelse(is_backwards, sec %% dim_sec, sec),
                  sec_rel = ifelse(sec_rel == 0, dim_sec, sec_rel)) |>
    dplyr::ungroup() |>
    dplyr::mutate(sec = sec - min(sec, na.rm = TRUE) + 1,
                  sec_rel = sec_rel - min(sec_rel, na.rm = TRUE) + 1) |> # Set lowest value to 1 after margin removal
    dplyr::arrange(sec_par) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  if(type == "row") gp$row_coord_map <- temp else gp$col_coord_map <- temp

  gp
}
