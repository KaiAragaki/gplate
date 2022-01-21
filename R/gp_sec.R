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
  # ----------------------------------------------------------------------------
  margin <- get_margin(margin)
  gp <- make_child_parent(gp)

  # Get sec dims + margin
  # If no nrow/ncol, use parents
  if (!is.null(nrow)) {
    gp$nrow_sec <- nrow + margin$top + margin$bottom
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol + margin$left + margin$right
  }

  # Update child metadata
  gp$wells_sec <- nrow * ncol

  if (wrap) {
    gp <- gp_unwrap(gp, flow)
    if (flow == "row") {
      gp$ncol_sec_par <- max(gp$well_data$.col_sec)
      gp$nrow_sec_par <- nrow
    }

    if (flow == "col") {
      gp$nrow_sec_par <- max(gp$well_data$.row_sec)
      gp$ncol_sec_par <- ncol
    }
  }

  # Probably temporary.
  # gp$well_data <- gp$well_data |>
  #   dplyr::select(.row, .col)

  gp <- gp |>
    coord_map("row", start_corner, margin) |>
    coord_map("col", start_corner, margin) |>
    add_map("row") |>
    add_map("col")

  gp$well_data <- gp$well_data |>
    dplyr::mutate(.is_margin = .col_is_margin | .row_is_margin)

  gp <- make_sec(gp, flow)

  if (!break_sections) {
    gp$well_data <- gp$well_data |>
      dplyr::group_by(.sec) |>
      dplyr::mutate(.n = dplyr::n()) |>
      dplyr::mutate(.sec = ifelse(.n < gp$wells_sec, NA_integer_, .data$.sec) |> as.factor(),
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

# TODO
# Add label functionality back in

# TODO
# Make it work with multiple layers
# Right now it seems like just .sec isn't working ... .col_sec and .row_sec appear to work

# TODO
# I ultimately feel like a join would be better for .sec....the 'stamp' idea....still figuring out how to implement

make_child_parent <- function(gp) {
  gp$nrow_sec_par  <- gp$nrow_sec
  gp$ncol_sec_par  <- gp$ncol_sec
  gp$wells_sec_par <- gp$wells_sec

  gp$well_data$.sec_par         <- gp$well_data$.sec
  gp$well_data$.row_sec_par     <- gp$well_data$.row_sec
  gp$well_data$.col_sec_par     <- gp$well_data$.col_sec

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
