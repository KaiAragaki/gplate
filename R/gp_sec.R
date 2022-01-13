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
#' @param margin Border width outside the section that will be unfilled. Can
#'   take an argument of one (same border all around), two (top/bottom,
#'   left/right), three (top, left/right, bottom), or four (top, right, bottom,
#'   left).
#' @param break_sections Should partial sections be allowed?
#'
#' @return
#' @export
#'
#' @examples
gp_sec <- function(gp, name, labels = NULL,
                   nrow = NULL, ncol = NULL,
                   start_corner = c("tl", "tr", "bl", "br"),
                   flow = c("row", "col"),
                   margin = 0,
                   wrap = FALSE,
                   break_sections = TRUE) {

  # Checks ---------------------------------------------------------------------
  flow <- rlang::arg_match(flow)
  start_corner <- rlang::arg_match(start_corner)
  stopifnot(is.numeric(nrow) | is.null(nrow),
            is.numeric(ncol) | is.null(ncol),
            is.numeric(margin),
            is.logical(wrap),
            is.logical(break_sections))

  margin <- get_margin(margin)

  # Child becomes parent -------------------------------------------------------
  gp <- make_child_parent(gp)

  wd <- gp$well_data

  # If nrow/ncol not set, inherit parents --------------------------------------
  if (!is.null(nrow)) {
    gp$nrow_sec <- nrow + margin$top + margin$bottom
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol + margin$left + margin$right
  }

  # Update metadata for child --------------------------------------------------
  gp$wells_sec <- nrow * ncol

  # Section demarcation --------------------------------------------------------

  # Make axes relative to flow direction and start corner
  wd <- gp_make_rel_ax(wd, start_corner, flow)

  # Make sec_rel axes, but relative to the CHILD's orientation of the PARENT's sections
  wd <- gp_make_child_rel_sec_par_ax(wd, start_corner, flow)

  # Make sec_rel axes ----------------------------------------------------------
  wd <- wd |>
    dplyr::mutate(.row_sec_rel = ((.data$.row_rel_child_sec_par - 1) %% gp$nrow_sec) + 1,
                  .col_sec_rel = ((.data$.col_rel_child_sec_par - 1) %% gp$ncol_sec) + 1)

  # Define lanes ---------------------------------------------------------------
  wd <- gp_define_lanes(wd, flow, wrap, gp$nrow_sec, gp$ncol_sec, gp$wells)

  # Make absolute section axes from relative section axes ----------------------
  wd <- gp_rel_sec_to_sec(wd, start_corner, gp$nrow_sec, gp$ncol_sec)

  # Mark if something is a margin ----------------------------------------------
  wd <- wd |>
    dplyr::mutate(.is_margin = FALSE,
                  .is_margin =
                    .is_margin |
                    .col_sec %in% 0:margin$left |
                    .col_sec %in% (max(.col_sec, na.rm = TRUE) + 1):(max(.col_sec, na.rm = TRUE) + 1 - margin$right) |
                    .row_sec %in% 0:margin$top |
                    .row_sec %in% (max(.row_sec, na.rm = TRUE) + 1):(max(.row_sec, na.rm = TRUE) + 1 - margin$bottom))

  # If you're a margin, you're not part of a section. The counter should only
  # start at 1 with items that aren't margins.
  wd <- wd |>
    dplyr::mutate(.col_sec = dplyr::if_else(.is_margin, NA_integer_, as.integer(.col_sec))) |>
    dplyr::arrange(.col_sec) |>
    dplyr::mutate(.col_sec = as.character(.col_sec) |> forcats::fct_inorder() |> as.numeric()) |>
    dplyr::mutate(.row_sec = dplyr::if_else(.is_margin, NA_integer_, as.integer(.row_sec))) |>
    dplyr::arrange(.row_sec) |>
    dplyr::mutate(.row_sec = as.character(.row_sec) |> forcats::fct_inorder() |> as.numeric())


  if (wrap) {
    wd <- dplyr::mutate(wd, sec = dplyr::if_else(is.na(.col_sec) | is.na(.row_sec), NA_integer_, .sec))
    gp$well_data <- wd
    return(gp)
  }

  wd <- wd |>
    dplyr::group_by(.data$.lane_h, .data$.lane_v) |>
    dplyr::rowwise() |>
    dplyr::mutate(.sec = paste0(.data$.lane_h, .data$.lane_v) |> forcats::fct_inorder()) |>
    dplyr::ungroup() |>
    dplyr::mutate(.sec = as.integer(.data$.sec),
                  .sec = dplyr::if_else(is.na(.col_sec) | is.na(.row_sec), NA_integer_, .sec),
                  {{name}} := as.factor(.sec))

  if (!break_sections) {
    wd <- wd |>
      dplyr::group_by(.sec) |>
      dplyr::mutate(.n = dplyr::n()) |>
      dplyr::mutate(.sec = ifelse(.n < gp$wells_sec, NA_integer_, .data$.sec) |> as.factor(),
                    .sec = as.numeric(.sec), # No idea why this has to be a separate line to work, but it does
                    {{name}} := as.factor(.sec)) |>
      dplyr::ungroup() |>
      dplyr::mutate(.row_sec = ifelse(is.na(.sec), NA_integer_, .row_sec),
                    .col_sec = ifelse(is.na(.sec), NA_integer_, .col_sec))
  }

  if (!is.null(labels)) {
    length(labels) <- length(levels(wd[[name]]))
    wd <- wd |>
      dplyr::mutate({{name}} := factor(.data[[name]], levels = levels(.data[[name]]), labels = labels))
  }

  gp$well_data <- wd

  gp
}

gp_make_rel_ax <- function(wd, start_corner, flow) {
  wd |>
    gp_arrange_for_rel_ax(start_corner, flow) |>
    dplyr::mutate(.row_rel = .data$.row |> as.character() |> forcats::fct_inorder() |> as.integer(),
                  .col_rel = .data$.col |> as.character() |> forcats::fct_inorder() |> as.integer())
}

gp_make_child_rel_sec_par_ax <- function(wd, start_corner, flow) {
  wd |>
    gp_arrange_for_rel_ax(start_corner, flow) |>
    dplyr::mutate(.row_rel_child_sec_par = .data$.row_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer(),
                  .col_rel_child_sec_par = .data$.col_sec_par |> as.character() |> forcats::fct_inorder() |> as.integer())
}

gp_arrange_for_rel_ax <- function(wd, start_corner, flow) {
  if (start_corner == "tl") {
    if (flow == "row") return(dplyr::arrange(wd, .data$.sec, .data$.row_sec_par, .data$.col_sec_par))
    else return(dplyr::arrange(wd, .data$.sec, .data$.col_sec_par, .data$.row_sec_par))
  }

  if (start_corner == "tr") {
    if (flow == "row") return(dplyr::arrange(wd, .data$.sec, .data$.row_sec_par, dplyr::desc(.data$.col_sec_par)))
    else return(dplyr::arrange(wd, .data$.sec, dplyr::desc(.data$.col_sec_par), .data$.row_sec_par))
  }

  if (start_corner == "bl") {
    if (flow == "row") return(dplyr::arrange(wd, .data$.sec, dplyr::desc(.data$.row_sec_par), .data$.col_sec_par))
    else return(dplyr::arrange(wd, .data$.sec, .data$.col_sec_par, dplyr::desc(.data$.row_sec_par)))
  }

  if (start_corner == "br") {
    if (flow == "row") return(dplyr::arrange(wd, .data$.sec, dplyr::desc(.data$.row_sec_par), dplyr::desc(.data$.col_sec_par)))
    else return(dplyr::arrange(wd, .data$.sec, dplyr::desc(.data$.col_sec_par), dplyr::desc(.data$.row_sec_par)))
  }
}


gp_define_lanes <- function(wd, flow, wrap, nrow, ncol, wells) {

  if (wrap) {
    if (flow == "row") {
      wd <- wd |>
        dplyr::mutate(.lane_h = ((.data$.row_rel_child_sec_par - 1) %/% nrow) + 1,
                      .lane_v = 1) |>
        dplyr::arrange(.row_sec_rel) |>
        dplyr::group_by(.row_sec_rel) |>
        dplyr::mutate(.sec = rep(1:99, each = ncol, length.out = dplyr::n())) |>
        dplyr::ungroup() |>
        dplyr::arrange(.row_sec_rel, .lane_h) |>
        dplyr::group_by(.row_sec_rel) |>
        dplyr::mutate(.col_sec_rel = rep(1:ncol, length.out = max(.data$.col)*max(.data$.lane_h))) |>
        dplyr::ungroup()
      return(wd)
    }

    if (flow == "col") {
      wd <- wd |>
        dplyr::mutate(.lane_h = 1,
                      .lane_v = ((.data$.col_rel_child_sec_par - 1) %/% ncol) + 1) |>
        dplyr::arrange(.data$.col_sec_rel) |>
        dplyr::group_by(.data$.col_sec_rel) |>
        dplyr::mutate(.sec = rep(1:99, each = nrow, length.out = dplyr::n()))
      return(wd)
    }
  }

  wd <- wd |>
    dplyr::mutate(.lane_h = ((.data$.col_rel_child_sec_par - 1) %/% ncol) + 1,
                  .lane_v = ((.data$.row_rel_child_sec_par - 1) %/% nrow) + 1)
}

gp_rel_sec_to_sec <- function(wd, start_corner, nrow, ncol) {
  if (start_corner == "tl") {
    wd$.col_sec <- wd$.col_sec_rel
    wd$.row_sec <- wd$.row_sec_rel
  }

  if (start_corner == "tr") {
    wd$.col_sec <- -wd$.col_sec_rel + ncol + 1
    wd$.row_sec <- wd$.row_sec_rel
  }

  if (start_corner == "bl") {
    wd$.col_sec <- wd$.col_sec_rel
    wd$.row_sec <- -wd$.row_sec_rel + nrow + 1
  }

  if (start_corner == "br") {
    wd$.col_sec <- -wd$.col_sec_rel + ncol + 1
    wd$.row_sec <- -wd$.row_sec_rel + nrow + 1
  }
  wd
}

make_child_parent <- function(gp) {
  gp$nrow_sec_par  <- gp$nrow_sec
  gp$ncol_sec_par  <- gp$ncol_sec
  gp$wells_sec_par <- gp$wells_sec

  gp$well_data$.sec_par         <- gp$well_data$.sec
  gp$well_data$.row_sec_par     <- gp$well_data$.row_sec
  gp$well_data$.col_sec_par     <- gp$well_data$.col_sec
  gp$well_data$.row_sec_par_rel <- gp$well_data$.row_sec_rel
  gp$well_data$.col_sec_par_rel <- gp$well_data$.col_sec_rel

  gp
}

get_margin <- function(m) {

  if (length(m) == 1) {
    return(list(top = m[1], right = m[1], bottom = m[1], left = m[1]))
  }

  if (length(m) == 2) {
    return(list(top = m[1], right = m[2], bottom = m[1], left = m[2]))
  }

  if (length(m) == 3) {
    return(list(top = m[1], right = m[2], bottom = m[3], left = m[2]))
  }

  if (length(m) == 4) {
    return(list(top = m[1], right = m[2], bottom = m[3], left = m[4]))
  }

}
