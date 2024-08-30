#' Add a section to a `gp` object.
#'
#' @param gp A `gp` object
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
#' @param advance Should this section be a child or sibling of the one before it? If TRUE (default), it will be a child.
#'
#' @return a `gp`
#' @export
#'
#' @examples
#'
#' gp(16, 24) |> gp_sec("section 1", ncol = 3)
#'
#' pq <- gp(8, 12, protein_quant) |> gp_sec("has_sample", 3, 19, wrap = TRUE, labels = "sample")
#'
#' # Sections can be used to label things for tidying
#' pq |> gp_serve()
#'
#' # They can also be used for plotting:
#' pq |> gp_plot(has_sample)
#'
#' @importFrom rlang .data `:=`
gp_sec <- function(gp, ...) {
  UseMethod("gp_sec")
}

#' @export
#' @rdname gp_sec
gp_sec.gp <- function(gp, name, nrow = NULL, ncol = NULL, labels = NULL,
                   start_corner = c("tl", "tr", "bl", "br"),
                   flow = c("row", "col"),
                   margin = 0,
                   wrap = FALSE,
                   break_sections = TRUE,
                   advance = TRUE) {

  # Checks ---------------------------------------------------------------------
  stopifnot(is.numeric(nrow) | is.null(nrow),
            is.numeric(ncol) | is.null(ncol),
            is.numeric(margin),
            is.logical(wrap),
            is.logical(break_sections))

  flow <- rlang::arg_match(flow)
  start_corner <- rlang::arg_match(start_corner)
  check_has_name(name)
  check_if_flow_and_custom_dims(flow, nrow, ncol)
  # ----------------------------------------------------------------------------

  margin <- get_margin(margin)
  non_flow <- setdiff(c("row", "col"), flow)

  # Internalize user arguments into gp object ----------------------------------
  gp$start_corner <- start_corner

  # Get sec dims + margin
  # If no nrow/ncol, use parents
  if (!is.null(nrow)) {
    gp$nrow_sec <- nrow
    gp$nrow_sec_mar <- sum(nrow + margin$top + margin$bottom) # Need sum because nrow is not necessarily a single digit
  } else {
    gp$nrow_sec <- gp$nrow_sec_par
    gp$nrow_sec_mar <- sum(gp$nrow_sec + margin$top + margin$bottom)
  }

  if (!is.null(ncol)) {
    gp$ncol_sec <- ncol
    gp$ncol_sec_mar <- sum(ncol + margin$left + margin$right)
  } else {
    gp$ncol_sec <- gp$ncol_sec_par
    gp$ncol_sec_mar <- sum(gp$ncol_sec + margin$left + margin$right)
  }

  if (!wrap & ((min(gp$nrow_sec_par) < max(gp$nrow_sec_mar)) | (min(gp$ncol_sec_par) < max(gp$ncol_sec_mar)))) {
    stop("Child section exceeds dimensions of its parent, and wrap = FALSE")
  }

  # Make sections --------------------------------------------------------------
  gp <- gp |>
    coordinate("row", margin) |>
    coordinate("col", margin) |>
    arrange_by_dim(flow) |>
    unroll_sec_dim_along_parent(flow, wrap = FALSE) |>
    arrange_by_dim(flow) |>
    unroll_sec_dim_along_parent(non_flow, wrap)

  if (wrap) {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.sec = ifelse(.data$.row_is_margin | .data$.col_is_margin | is.na(.data$.sec_par), NA_integer_, .data$.sec)) |>
      dplyr::select(-dplyr::contains(".index"))
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::select(-".sec") |>
      dplyr::group_by(.data$.index_row, .data$.index_col) |>
      tidyr::nest() |>
      dplyr::ungroup() |>
      dplyr::mutate(.sec = dplyr::row_number()) |>
      tidyr::unnest("data") |>
      dplyr::mutate(.sec = ifelse(.data$.row_is_margin | .data$.col_is_margin | is.na(.data$.sec_par), NA_integer_, .data$.sec)) |>
      dplyr::select(-c(".index_col", ".index_row"))
  }

  if (!break_sections) {
    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, .data$.sec_par) |>
      dplyr::mutate(.n = dplyr::n()) |>
      dplyr::mutate(.sec = ifelse(.data$.n < gp$nrow_sec * gp$ncol_sec | is.na(.data$.sec) | is.na(.data$.sec_par), NA_integer_, .data$.sec) |> as.factor(),
                    .sec = as.integer(.data$.sec)) |>  # No idea why this has to be a separate line to work, but it does
      dplyr::ungroup() |>
      dplyr::mutate(.row_sec = ifelse(is.na(.data$.sec), NA_integer_, .data$.row_sec),
                    .col_sec = ifelse(is.na(.data$.sec), NA_integer_, .data$.col_sec))
  }

  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ name }} := dplyr::if_else(is.na(.data$.sec_par), NA_integer_, .data$.sec))


  if (is.null(labels)) {
    secs <- unique(gp$well_data$.sec)
    labels <- seq_along(secs[!is.na(secs)])
  }

  usr_labels_len <- length(labels)

  if (length(nrow) > 1 | length(ncol) > 1) {
    map_sec <- paste0(".", non_flow, "_map_sec")
    gp$well_data <- gp$well_data |>
      dplyr::rowwise() |>
      dplyr::mutate(.sec = dplyr::if_else(.data$.sec > usr_labels_len | is.na(.data$.sec_par), NA_integer_, as.integer(.data[[map_sec]])))
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::mutate(.temp = .data$.sec |> as.factor() |> as.numeric()) |>
      dplyr::rowwise() |>
      dplyr::mutate(.sec = dplyr::if_else(.data$.temp > usr_labels_len | is.na(.data$.temp), NA_real_, .data$.temp),
                    .sec = as.factor(.data$.sec))
  }

  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ name }} := .data$.sec)

  length(labels) <- length(unique(stats::na.omit(gp$well_data$.sec)))
  gp$well_data <- gp$well_data |>
    dplyr::ungroup() |>
    dplyr::mutate({{ name }} := factor(.data[[name]], levels = levels(as.factor(.data[[name]])), labels = labels))

  if (advance) {
    gp <- make_child_parent(gp)
  }

  gp
}


make_child_parent <- function(gp) {
  gp$start_corner_par <- gp$start_corner
  gp$nrow_sec_par     <- gp$nrow_sec
  gp$nrow_sec_par_mar <- gp$nrow_sec
  gp$ncol_sec_par     <- gp$ncol_sec
  gp$ncol_sec_par_mar <- gp$ncol_sec

  gp$well_data$.sec_par         <- gp$well_data$.sec
  gp$well_data$.row_sec_par     <- gp$well_data$.row_sec
  gp$well_data$.col_sec_par     <- gp$well_data$.col_sec

  gp
}

