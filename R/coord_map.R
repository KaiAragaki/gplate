coord_map <- function(gp, type = c("row", "col"), margin) {

  # ?new margin strat?
  # use nrow, then just pop in margins before and after as NAs
  # If ncol = c(3, 2, 1)
  # and margin = 1
  # NA 3 3 3 NA NA 2 2 NA NA 1 NA NA 3 NA
  #
  # Something like
  # c(rbind(margin))

  type <- rlang::arg_match(type)

  dim_sec_par <- ifelse(type == "row", gp$nrow_sec_par, gp$ncol_sec_par)
  dim_sec <- if(type == "row") gp$nrow_sec else gp$ncol_sec
  backwards_corners <- if(type == "row") c("bl", "br") else c("tr", "br")
  is_backwards <- gp$start_corner %in% backwards_corners
  margin_head_size <- if (type == "row") margin$bottom else margin$right
  margin_tail_size <- if (type == "row") margin$top else margin$left

  margin_machine <- function(dim_sec, margin_head_size, margin_tail_size) {
    # For every item in dim_sec...
    # Put the margin head before it
    # Put the dim_sec item in
    # Put the margin tail after it
    # Add a little column denoting whether it's a margin or a dim along the way
    # A little c(n, is_margin) rbind magic may suffice
  }


  dim_sec_mar <- dim_sec + margin_head_size + margin_tail_size

  margin_head <- (dim_sec_mar + 1 - margin_head_size):(dim_sec_mar + 1)
  margin_tail <- 0:margin_tail_size

  sec_par <- if (type == "row") gp$nrow_sec_par else gp$ncol_sec_par
  sec_par <- seq_len(sec_par)

  if (gp$start_corner_par %in% backwards_corners) sec_par_rel <- rev(sec_par) else sec_par_rel <- sec_par

  seq2 <- Vectorize(seq.default, vectorize.args = "from")

  coord_map <-
    dplyr::tibble(sec_par, sec_par_rel) |>
    dplyr::arrange(sec_par) |>
    dplyr::mutate(sec_rel = rep_len(unlist(seq2(dim_sec + margin_head_size + margin_tail_size)), dim_sec_par))

  if (is_backwards) {
    coord_map <- dplyr::mutate(coord_map, sec_rel = rev(sec_rel))
  }

  coord_map <- coord_map |>
    dplyr::rowwise() |>
    dplyr::mutate(is_margin = sec_rel %in% c(margin_head, margin_tail),
                  sec_rel = ifelse(is_margin, NA_integer_, sec_rel)) |>
    dplyr::ungroup() |>
    dplyr::mutate(sec_rel = sec_rel - min(sec_rel, na.rm = TRUE) + 1) |> # Set lowest value to 1 after margin removal
    dplyr::rowwise() |>
    dplyr::mutate(sec = ifelse(is_backwards, -sec_rel + dim_sec + 1, sec_rel)) |>
    dplyr::ungroup() |>
    dplyr::arrange(sec_par) |>
    dplyr::rename_with(~ paste0(".", type, "_", .x))

  if(type == "row") gp$row_coord_map <- coord_map else gp$col_coord_map <- coord_map

  gp
}
