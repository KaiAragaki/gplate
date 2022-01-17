row_coord_map <- function(gp, start_corner) {

  gp$row_coord_map <- tibble(
    sec_par = seq_len(gp$nrow_sec_par),
    sec = rep_len(seq_len(gp$nrow_sec), gp$nrow_sec_par),
    sec_rel = case_when(start_corner %in% c("bl", "br") ~ rev(.data$sec),
                            TRUE ~ .data$sec),
    rel_child_sec_par = case_when(start_corner %in% c("bl", "br") ~ rev(.data$sec_par),
                                      TRUE ~ .data$sec_par)
  )

  gp
}

col_coord_map <- function(gp, start_corner) {

  gp$col_coord_map <- tibble(
    sec_par = seq_len(gp$ncol_sec_par),
    sec = rep_len(seq_len(gp$ncol_sec), gp$ncol_sec_par),
    sec_rel = case_when(start_corner %in% c("tl", "bl") ~ rev(.data$sec),
                            TRUE ~ .data$sec),
    rel_child_sec_par = case_when(start_corner %in% c("tl", "bl") ~ rev(.data$sec_par),
                                      TRUE ~ .data$sec_par)
  )

  gp
}
