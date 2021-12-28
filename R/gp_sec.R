#' Add a section to a `gp` object.
#'
#' @param gp a `gp` object
#' @param name Character. Name of the section.
#' @param nrow Numeric. Section height. If `NULL`, will fill width of parent
#'   section.
#' @param ncol Numeric. Section width. If `NULL`, will fill width of parent
#'   section.
#' @param start_corner Corner of section to place first item.
#' @param flow Direction that subsequent items should be placed relative to first corner.
#' @param padding Border width INSIDE the section that will be unfilled. Can
#'   take an argument of one (same border all around), two (top/bottom,
#'   left/right), three (top, left/right, bottom), or four (top, right, bottom,
#'   left).
#' @param margin Border width OUTSIDE the section that will remain unfilled. See
#'   above for possible argument lengths.
#' @param break_sections If a section would need to be wrapped around the edge
#'   of the parent section (default parent section is the plate), should it
#'   instead shift the flow direction far enough so that it can fit without
#'   wrapping? Will error if either `nrow` or `ncol` exceeds the parent
#'   dimension.
#'
#' @return
#' @export
#'
#' @examples
gp_sec <- function(gp, name,
                   nrow = NULL, ncol = NULL,
                   start_corner = c("tl", "tr", "bl", "br"),
                   flow = c("row", "col"),
                   padding = 0, margin = 0,
                   wrap = FALSE) {

  flow <- rlang::arg_match(flow)
  start_corner <- rlang::arg_match(start_corner)

  if (is.null(nrow)) nrow <- gp$cur_sec_nrow
  if (is.null(ncol)) ncol <- gp$cur_sec_ncol

  gp_sec_fits(gp, nrow, ncol, wrap)
  gp$cur_sec_ncol <- ncol
  gp$cur_sec_nrow <- nrow

  wd <- gp$well_data

  wd <- wd |>
    dplyr::mutate(row_sec_par = row_sec,
                  row_sec_par_rel = row_sec_rel,
                  col_sec_par = col_sec,
                  col_sec_par_rel = col_sec_rel)

  wd <- gp_make_rel_ax(wd, start_corner, flow)
  wd <- gp_make_child_rel_sec_par_ax(wd, start_corner, flow)
  wd <- wd |>
    dplyr::mutate(row_sec_rel = ((.data$temp_row_sec_par - 1) %% nrow) + 1,
                  col_sec_rel = ((.data$temp_col_sec_par - 1) %% ncol) + 1,
                  lane_h = ((.data$temp_col_sec_par - 1) %/% ncol) + 1,
                  lane_v = ((.data$temp_row_sec_par - 1) %/% nrow) + 1)

  if (start_corner == "tl") {
    wd$col_sec <- wd$col_sec_rel
    wd$row_sec <- wd$row_sec_rel
  }

  if (start_corner == "tr") {
    wd$col_sec <- -wd$col_sec_rel + ncol + 1
    wd$row_sec <- wd$row_sec_rel
  }

  if (start_corner == "bl") {
    wd$col_sec <- wd$col_sec_rel
    wd$row_sec <- -wd$row_sec_rel + nrow + 1
  }

  if (start_corner == "br") {
    wd$col_sec <- -wd$col_sec_rel + ncol + 1
    wd$row_sec <- -wd$row_sec_rel + nrow + 1
  }

  wd <- wd |>
    dplyr::group_by(.data$lane_h, .data$lane_v) |>
    dplyr::rowwise() |>
    dplyr::mutate(sec = paste0(.data$lane_h, .data$lane_v) |> forcats::fct_inorder()) |>
    dplyr::ungroup() |>
    dplyr::mutate(sec = as.integer(.data$sec),
                  {{name}} := sec)

  gp$well_data <- wd

  gp
}

gp_sec_fits <- function(gp, nrow, ncol, wrap) {

  error <- FALSE

  if (gp$cur_sec_nrow < nrow & !wrap) {
    sec_size <- nrow
    parent_size <- gp$cur_sec_nrow
    big <- "wide"
    error <- TRUE
  }

  if (gp$cur_sec_ncol < ncol & !wrap) {
    sec_size <- ncol
    parent_size <- gp$cur_sec_ncol
    big <- "tall"
    error <- TRUE
  }

  if (error) {
    header <- glue::glue("Section {id} is too {big} for parent section.")
    body <- glue::glue("Section {id} is {sec_size} {big}, but the parent section is only {parent_size}")
    rlang::abort(c(header, x = body))
  }
}

gp_make_rel_ax <- function(wd, start_corner, flow) {
  wd |>
    gp_arrange_for_rel_ax(start_corner, flow) |>
    dplyr::mutate(row_rel = .data$row |> as.character() |> forcats::fct_inorder() |> as.integer(),
                  col_rel = .data$col |> as.character() |> forcats::fct_inorder() |> as.integer())
}

gp_make_child_rel_sec_par_ax <- function(wd, start_corner, flow) {
  wd |>
    gp_arrange_for_rel_ax(start_corner, flow) |>
    dplyr::mutate(temp_row_sec_par = .data$row_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer(),
                  temp_col_sec_par = .data$col_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer())
}

gp_arrange_for_rel_ax <- function(wd, start_corner, flow) {
  if (start_corner == "tl") {
    if (flow == "row") return(wd)
    else return(dplyr::arrange(wd, .data$col, .data$row))
  }

  if (start_corner == "tr") {
    if (flow == "row") return(dplyr::arrange(wd, .data$row, dplyr::desc(.data$col)))
    else return(dplyr::arrange(wd, dplyr::desc(.data$col), .data$row))
  }

  if (start_corner == "bl") {
    if (flow == "row") return(dplyr::arrange(wd, dplyr::desc(.data$row), .data$col))
    else return(dplyr::arrange(wd, .data$col, dplyr::desc(.data$row)))
  }

  if (start_corner == "br") {
    if (flow == "row") return(dplyr::arrange(wd, dplyr::desc(.data$row), dplyr::desc(.data$col)))
    else return(dplyr::arrange(wd, dplyr::desc(.data$col), dplyr::desc(.data$row)))
  }
}
