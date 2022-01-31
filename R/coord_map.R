coord_map <- function(gp, type = c("row", "col"), margin) {

  # When getting parents that have margins, these margins need to be persistent
  # so that the child section is aware of them as well

  # OR it needs to be such that the parents only pass on the non-margin rows
  # Getting from option A to option B is pretty easy (filter).

  # When we set wrap = TRUE, and flow = "row" (default),
  # we only need to change .col_sec

  type <- rlang::arg_match(type)

  dim_sec_par <- ifelse(type == "row", gp$nrow_sec_par, gp$ncol_sec_par)
  dim_sec <- ifelse(type == "row", gp$nrow_sec, gp$ncol_sec)
  backwards_corners <- if(type == "row") c("bl", "br") else c("tr", "br")
  is_backwards <- gp$start_corner %in% backwards_corners
  margin_head_size <- if (type == "row") margin$bottom else margin$right
  margin_tail_size <- if (type == "row") margin$top else margin$left

  dim_sec_mar <- dim_sec + margin_head_size + margin_tail_size

  margin_head <- (dim_sec_mar + 1 - margin_head_size):(dim_sec_mar + 1)
  margin_tail <- 0:margin_tail_size

  sec_par <- if (type == "row") gp$nrow_sec_par else gp$ncol_sec_par
  sec_par <- seq_len(sec_par)

  if (gp$start_corner_par %in% backwards_corners) sec_par_rel <- rev(sec_par) else sec_par_rel <- sec_par

  temp <-
    dplyr::tibble(sec_par, sec_par_rel)

  if (is_backwards) {
    temp <- arrange(temp, desc(sec_par))
  }

  temp <- temp |>
    dplyr::mutate(sec_rel = rep_len(seq_len(dim_sec + margin_head_size + margin_tail_size), dim_sec_par)) |>
    dplyr::rowwise() |>
    dplyr::mutate(is_margin = (sec_rel %in% margin_head) | (sec_rel %in% margin_tail),
                  sec_rel = ifelse(is_margin, NA_integer_, sec_rel)) |>
    dplyr::group_by(is_margin) |>
    dplyr::mutate(sec_rel = (sec_rel - sec_rel[1] + 1L + dim_sec) %% dim_sec,
                  sec_rel = ifelse(sec_rel == 0, dim_sec, sec_rel)) |>
    dplyr::rowwise() |>
    dplyr::mutate(sec = ifelse(is_backwards, -sec_rel + dim_sec + 1, sec_rel)) |>
    dplyr::ungroup() |>
    dplyr::mutate(sec_rel = sec_rel - min(sec_rel, na.rm = TRUE) + 1,
                  sec = sec - min(sec, na.rm = TRUE) + 1) |> # Set lowest value to 1 after margin removal
    dplyr::arrange(sec_par) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  if(type == "row") gp$row_coord_map <- temp else gp$col_coord_map <- temp

  gp
}
