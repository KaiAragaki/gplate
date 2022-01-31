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
gp_sec <- function(gp, name, nrow = NULL, ncol = NULL, labels = NULL,
                   start_corner = c("tl", "tr", "bl", "br"),
                   flow = c("row", "col"),
                   margin = 0,
                   wrap = FALSE,
                   break_sections = TRUE) {

  # Checks ---------------------------------------------------------------------
  stopifnot(is.numeric(nrow) | is.null(nrow),
            is.numeric(ncol) | is.null(ncol),
            is.numeric(margin),
            is.logical(wrap),
            is.logical(break_sections))

  flow <- rlang::arg_match(flow)
  start_corner <- rlang::arg_match(start_corner)
  check_has_name(name)
  check_break_if_wrap(wrap, break_sections)
  check_if_flow_and_custom_dims(flow, nrow, ncol)
  if ((wrap & length(margin) > 1) || (wrap & margin != 0)) {
    rlang::abort(message = c("wrapping with margins not currently supported"))
  }
  # ----------------------------------------------------------------------------
  margin <- get_margin(margin)
  gp <- make_child_parent(gp)

  # Internalize user arguments into gp object ----------------------------------
  gp$start_corner <- start_corner

  # Get sec dims + margin
  # If no nrow/ncol, use parents
  if (!is.null(nrow)) {
    gp$nrow_sec <- nrow
    gp$nrow_sec_mar <- nrow + margin$top + margin$bottom
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol
    gp$ncol_sec_mar <- ncol + margin$left + margin$right
  }

  # Make sections --------------------------------------------------------------

  gp <- gp |>
    coord_map("row", margin) |>
    coord_map("col", margin) |>
    add_map("row") |>
    add_map("col")

  gp <- make_sec(gp, flow, wrap, nrow, ncol)

  if (!break_sections) {
    gp$well_data <- gp$well_data |>
      dplyr::group_by(.sec) |>
      dplyr::mutate(.n = dplyr::n()) |>
      dplyr::mutate(.sec = ifelse(.n < nrow * ncol, NA_integer_, .data$.sec) |> as.factor(),
                    .sec = as.numeric(.sec)) |>  # No idea why this has to be a separate line to work, but it does
      dplyr::ungroup() |>
      dplyr::mutate(.row_sec = ifelse(is.na(.sec), NA_integer_, .row_sec),
                    .col_sec = ifelse(is.na(.sec), NA_integer_, .col_sec))
  }

  # if (!is.null(labels)) {
  #   length(labels) <- length(levels(wd[[name]]))
  #   wd <- wd |>
  #     dplyr::mutate({{name}} := factor(.data[[name]], levels = levels(.data[[name]]), labels = labels))
  # }

  gp
}

# TODO
# {{arg_name}} := as.factor(.sec)
# AKA User named section col in data

# TODO
# rewrap unwrapped plates
# Maybe do this whole thing at the section level
# Maybe unwrap the section maps? Can this be done at an individual map level or
# does it have to be at the merged level?

# TODO
# Add label functionality back in

# TODO Need to essentially build one section on top of the other's results for
# relative and nonrelative axes. Those two axes need to stay. The rest can be
# tacked on in the correct order. Or, somehow, the identity of whether or not a
# PARENT is 'backwards'

make_child_parent <- function(gp) {
  gp$start_corner_par <- gp$start_corner
  gp$nrow_sec_par  <- gp$nrow_sec
  gp$nrow_sec_par_mar <- gp$nrow_sec
  gp$ncol_sec_par  <- gp$ncol_sec
  gp$ncol_sec_par_mar <- gp$ncol_sec

  gp$well_data$.sec_par         <- gp$well_data$.sec
  gp$well_data$.row_sec_rel_par <- gp$well_data$.row_sec_rel
  gp$well_data$.col_sec_rel_par <- gp$well_data$.col_sec_rel
  gp$well_data$.row_sec_par     <- gp$well_data$.row_sec
  gp$well_data$.col_sec_par     <- gp$well_data$.col_sec

  gp
}

