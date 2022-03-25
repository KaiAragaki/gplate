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
#' @param labels Optional. What should the labels of each section be?
#' @param wrap Should the sections that go off the edge continue on the next row/column?
#'
#' @return a `gp`
#' @export
#'
#' @examples
#'
#' gp(16, 24) |> gp_sec("section 1", ncol = 3)
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
    gp$nrow_sec_mar <- sum(nrow + margin$top + margin$bottom) # Need sum because nrow is not necessarily a single digit
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol
    gp$ncol_sec_mar <- sum(ncol + margin$left + margin$right)
  }

  # Make sections --------------------------------------------------------------

  rr <- gp |>
    coordinate("row", margin)

  # TODO What if wrap = TRUE?
  # TODO DRY

  is_backwards_row <- start_corner %in% c("tr", "br")

  if (!is_backwards_row) {
    gp$well_data <- gp$well_data |>
      dplyr::arrange(.row_sec_par)
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::arrange(desc(.row_sec_par))
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.sec, .col_sec_par) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(rr))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, \(x) {cbind(non_int_replicate(rr, x), x)})) |>
    tidyr::unnest(cols = data) |>
    dplyr::ungroup()

  cc <- gp |>
    coordinate("col", margin)

  is_backwards_col <- start_corner %in% c("bl", "br")

  if (!is_backwards_col) {
    gp$well_data <- gp$well_data |>
      dplyr::arrange(.col_sec_par)
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::arrange(desc(.col_sec_par))
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.sec, .row_sec_par) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(cc))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(data, \(x) {cbind(non_int_replicate(cc, x), x)})) |>
    dplyr::mutate(.test_index = dplyr::cur_group_id() %/% gp$ncol_sec + 1) |>
    tidyr::unnest(cols = data) |>
    dplyr::ungroup()

# Instead of adding a test index, make non_int_replicate step bring its own id?



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

  gp$well_data <- dplyr::mutate(gp$well_data, {{name}} := .sec)

  if (!is.null(labels)) {
    length(labels) <- length(levels(as.factor(gp$well_data[[name]])))
    gp$well_data <- gp$well_data |>
      dplyr::mutate({{name}} := factor(.data[[name]], levels = levels(as.factor(.data[[name]])), labels = labels))
  }

  gp
}


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

