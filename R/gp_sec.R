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
                   wrap = FALSE,
                   break_sections = TRUE) {

  # Checks ---------------------------------------------------------------------
  flow <- rlang::arg_match(flow)
  start_corner <- rlang::arg_match(start_corner)
  stopifnot(is.numeric(nrow) | is.null(nrow),
            is.numeric(ncol) | is.null(ncol))

  # Child becomes parent -------------------------------------------------------
  gp$nrow_sec_par  <- gp$nrow_sec
  gp$ncol_sec_par  <- gp$ncol_sec
  gp$wells_sec_par <- gp$wells_sec

  wd <- gp$well_data

  wd$sec_par         <- wd$sec
  wd$row_sec_par     <- wd$row_sec
  wd$col_sec_par     <- wd$col_sec
  wd$row_sec_par_rel <- wd$row_sec_rel
  wd$col_sec_par_rel <- wd$col_sec_rel

  # If nrow/ncol not set, inherit parents --------------------------------------
  if (!is.null(nrow)) {
    gp$nrow_sec <- nrow
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol
  }

  # Update metadata for child --------------------------------------------------
  gp$wells_sec <- gp$nrow_sec * gp$ncol_sec

  # Section demarcation --------------------------------------------------------

  # Make axes relative to flow direction and start corner
  wd <- gp_make_rel_ax(wd, start_corner, flow)

  # Make sec_rel axes, but relative to the CHILD's orientation of the PARENT's sections
  wd <- gp_make_child_rel_sec_par_ax(wd, start_corner, flow)

  # Make sec_rel axes
  wd <- wd |>
    dplyr::mutate(row_sec_rel = ((.data$row_rel_child_sec_par - 1) %% gp$nrow_sec) + 1,
                  col_sec_rel = ((.data$col_rel_child_sec_par - 1) %% gp$ncol_sec) + 1)
  # Define lanes
  wd <- gp_define_lanes(wd, flow, wrap, gp$nrow_sec, gp$ncol_sec, gp$wells)

  wd <- gp_rel_sec_to_sec(wd, start_corner, gp$nrow_sec, gp$ncol_sec)

  if (wrap) {
    gp$well_data <- wd
    return(gp)
  }

  wd <- wd |>
    dplyr::group_by(.data$lane_h, .data$lane_v) |>
    dplyr::rowwise() |>
    dplyr::mutate(sec = paste0(.data$lane_h, .data$lane_v) |> forcats::fct_inorder()) |>
    dplyr::ungroup() |>
    dplyr::mutate(sec = as.integer(.data$sec),
                  {{name}} := as.factor(sec))

  if (!break_sections) {
    wd <- wd |>
      dplyr::group_by(sec) |>
      dplyr::mutate(n = dplyr::n()) |>
      dplyr::mutate(sec = ifelse(n < gp$wells_sec, NA_integer_, sec),
                    {{name}} := as.factor(sec))
  }

  gp$well_data <- wd

  gp
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
    dplyr::mutate(row_rel_child_sec_par = .data$row_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer(),
                  col_rel_child_sec_par = .data$col_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer())
}

gp_arrange_for_rel_ax <- function(wd, start_corner, flow) {
  if (start_corner == "tl") {
    if (flow == "row") return(dplyr::arrange(wd, sec, .data$row_sec_par, .data$col_sec_par))
    else return(dplyr::arrange(wd, sec, .data$col_sec_par, .data$row_sec_par))
  }

  if (start_corner == "tr") {
    if (flow == "row") return(dplyr::arrange(wd, sec, .data$row_sec_par, dplyr::desc(.data$col_sec_par)))
    else return(dplyr::arrange(wd, sec, dplyr::desc(.data$col_sec_par), .data$row_sec_par))
  }

  if (start_corner == "bl") {
    if (flow == "row") return(dplyr::arrange(wd, sec, dplyr::desc(.data$row_sec_par), .data$col_sec_par))
    else return(dplyr::arrange(wd, sec, .data$col_sec_par, dplyr::desc(.data$row_sec_par)))
  }

  if (start_corner == "br") {
    if (flow == "row") return(dplyr::arrange(wd, sec, dplyr::desc(.data$row_sec_par), dplyr::desc(.data$col_sec_par)))
    else return(dplyr::arrange(wd, sec, dplyr::desc(.data$col_sec_par), dplyr::desc(.data$row_sec_par)))
  }
}


gp_define_lanes <- function(wd, flow, wrap, nrow, ncol, wells) {

  if (wrap) {
    if (flow == "row") {
      wd <- wd |>
        dplyr::mutate(lane_h = ((.data$row_rel_child_sec_par - 1) %/% nrow) + 1,
                      lane_v = 1) |>
        dplyr::arrange(row_sec_rel) |>
        dplyr::group_by(row_sec_rel) |>
        dplyr::mutate(sec = rep(1:99, each = ncol, length.out = dplyr::n())) |>
        dplyr::ungroup() |>
        dplyr::arrange(row_sec_rel, lane_h) |>
        dplyr::group_by(row_sec_rel) |>
        dplyr::mutate(col_sec_rel = rep(1:ncol, length.out = max(col)*max(lane_h))) |>
        dplyr::ungroup()
      return(wd)
    }
    # KAINOTE(Need to store parent metadata - ncol, nrow)
    # Honestly should just dupe object at this point.

    if (flow == "col") {
      wd <- wd |>
        dplyr::mutate(lane_h = 1,
                      lane_v = ((.data$col_rel_child_sec_par - 1) %/% ncol) + 1) |>
        dplyr::arrange(col_sec_rel) |>
        dplyr::group_by(col_sec_rel) |>
        dplyr::mutate(sec = rep(1:99, each = nrow, length.out = dplyr::n()))
      return(wd)
    }
  }

  wd <- wd |>
    dplyr::mutate(lane_h = ((.data$col_rel_child_sec_par - 1) %/% ncol) + 1,
                  lane_v = ((.data$row_rel_child_sec_par - 1) %/% nrow) + 1)
}

gp_rel_sec_to_sec <- function(wd, start_corner, nrow, ncol) {
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
  wd
}
