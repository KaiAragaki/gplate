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


  # Checks
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

  gp <- row_coord_map(gp, start_corner)
  gp <- col_coord_map(gp, start_corner)


  # This creates a 'section stamp'...now we just need to figure out how to use it
  # Perhaps if we join directly to wd, we don't have to create an intermediate stamp
  stamp <- expand_grid(row_sec_par = gp$row_coord_map$sec_par, col_sec_par = gp$col_coord_map$sec_par) |>
    left_join(gp$row_coord_map, by = c(row_sec_par = "sec_par")) |>
    left_join(gp$col_coord_map, by = c(col_sec_par = "sec_par"), suffix = c("_row", "_col"))

  # Section demarcation --------------------------------------------------------
  wd <- wd |>
    ## Mark if something is a margin -------------------------------------------
    add_is_margin(margin) |>
    ## Margins aren't part of sections. Nums should start with non-margins -----
    exclude_margin_from_sec() |>
    define_sec(wrap, flow, arg_name = {{name}})

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

add_is_margin <- function(wd, margin) {
  wd |>
    dplyr::mutate(.is_margin = FALSE,
                  .is_margin =
                    .is_margin |
                    .col_sec %in% 0:margin$left |
                    .col_sec %in% (max(.col_sec, na.rm = TRUE) + 1):(max(.col_sec, na.rm = TRUE) + 1 - margin$right) |
                    .row_sec %in% 0:margin$top |
                    .row_sec %in% (max(.row_sec, na.rm = TRUE) + 1):(max(.row_sec, na.rm = TRUE) + 1 - margin$bottom))
}

exclude_margin_from_sec <- function(wd) {
  wd |>
    dplyr::mutate(.col_sec = dplyr::if_else(.is_margin, NA_integer_, as.integer(.col_sec))) |>
    dplyr::arrange(.col_sec) |>
    dplyr::mutate(.col_sec = as.character(.col_sec) |> forcats::fct_inorder() |> as.numeric()) |>
    dplyr::mutate(.row_sec = dplyr::if_else(.is_margin, NA_integer_, as.integer(.row_sec))) |>
    dplyr::arrange(.row_sec) |>
    dplyr::mutate(.row_sec = as.character(.row_sec) |> forcats::fct_inorder() |> as.numeric())

}

define_sec <- function(wd, wrap, flow, arg_name) {
  if (wrap) {
    if (flow == "row") {
      wd <- wd |>
        dplyr::arrange(.data$.row_sec_rel) |>
        dplyr::group_by(.data$.row_sec_rel) |>
        dplyr::mutate(.sec = rep(1:99, each = ncol, length.out = dplyr::n())) |>
        dplyr::ungroup()
    }

    if (flow == "col") {
      wd <- wd |>
        dplyr::arrange(.data$.col_sec_rel) |>
        dplyr::group_by(.data$.col_sec_rel) |>
        dplyr::mutate(.sec = rep(1:99, each = nrow, length.out = dplyr::n())) |>
        dplyr::ungroup()
    }
  } else {
    wd <- wd |>
      dplyr::group_by(.data$.lane_h, .data$.lane_v) |>
      dplyr::rowwise() |>
      dplyr::mutate(.sec = paste0(.data$.lane_h, .data$.lane_v) |> forcats::fct_inorder()) |>
      dplyr::ungroup()
  }

  wd <- wd |> dplyr::mutate(.sec = as.integer(.data$.sec),
                      .sec = dplyr::if_else(is.na(.col_sec) | is.na(.row_sec), NA_integer_, .sec),
                      {{arg_name}} := as.factor(.sec))
  wd
}

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
