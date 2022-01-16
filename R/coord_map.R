row_coord_map <- function(map, start_corner, nrow) {
  map |>
    mutate(row_sec_par = .data$row_sec,
           row_sec_par_rel = .data$row_sec_rel,
           row_sec = rep_len(seq_len(nrow), nrow(map)),
           row_sec_rel = case_when(start_corner %in% c("bl", "tl") ~ rev(.data$row_sec),
                                   TRUE ~ .data$row_sec),
           row_rel_child_sec_par = case_when(start_corner %in% c("bl", "tl") ~ rev(.data$row_sec_par),
                                             TRUE ~ .data$row_sec_par))
}
