#' A constructor for a gp object
#'
#' @param nrow Integer. Number of rows of the plate.
#' @param ncol Integer. Number of columns of the plate.
#' @param data An optional dataframe including plate data
#' @param tidy Is the supplied data already tidy, or should it be tidied?
#'
#' @return a `gp` object
#'
#' @export
#'
#' @examples
#' new_gp(nrow = 8L, ncol = 16L)
new_gp <- function(nrow = 1L, ncol = 1L, data = data.frame(), tidy = FALSE){

  stopifnot(is.integer(nrow), is.integer(ncol), is.data.frame(data) | is.matrix(data))

  wd <- tidyr::expand_grid(.row = seq_len(nrow), .col = seq_len(ncol))
  wd$.sec <- wd$.sec_par <- 1L
  wd$.row_rel <- wd$.row_sec <- wd$.row_sec_par <- wd$.row
  wd$.col_rel <- wd$.col_sec <- wd$.col_sec_par <- wd$.col
  wd$.row_is_margin <- wd$.col_is_margin <- FALSE

  has_size <- nrow(data) > 0 & ncol(data) > 0

  if (has_size & !tidy) {
    data <- gp_unravel(data)
  }

  if (has_size) {
    wd <- dplyr::left_join(wd, data, by = c(".row", ".col"))
  }

  structure(list(nrow = nrow,
                 ncol = ncol,
                 well_data = wd,
                 start_corner = "tl",
                 start_corner_par = "tl",
                 nrow_sec = nrow,
                 nrow_sec_mar = nrow,
                 nrow_sec_par = nrow,
                 nrow_sec_par_mar = nrow,
                 ncol_sec = ncol,
                 ncol_sec_mar = ncol,
                 ncol_sec_par = ncol,
                 ncol_sec_par_mar = ncol),
            class = "gp")
}

#' Make a gp object
#'
#' @param rows Numeric. The number of rows the plate should have.
#' @param cols Numeric. The number of columns the plate should have.
#' @param wells Numeric. The number of wells the plate has. If this is
#'   specified, rows and cols must be null - they are inferred from common form
#'   factors of plates.
#' @param data An optional data.frame of well data the same dimensions as the plate to be described
#' @param tidy Are the data supplied tidy?
#'
#' @return a `gp` object
#'
#' @details
#'
#' A `gp` object has the following components:
#'
#' - `nrow`/`ncol`: Number of plate rows/cols. This is static and will not be
#' changed by adding layers.
#'
#' - `wells`: Number of plate wells. Static.
#'
#' - `well_data`: Somewhat transient data used to define plotting coordinates
#' for layers. See below for more information.
#'
#' - `nrow_sec`/`ncols_sec`: The number of rows/cols of the current section.
#' When creating a plate, that number is the number of rows/cols of the plate
#' (the plate is the section).
#'
#' - `nrow_sec_par`/`ncol_sec_par`: The number of rows/cols of the parent
#' section. When creating a plate, it has no parent, so defaults to being its
#' own parent.
#'
#' `well_data` consists of many columns. The variable names can be broken down
#' as follows:
#'
#' - `row`/`col`: `row` is always the y axis, `col` is always the x axis. By
#' convention, plates start at 1, 1 in the top left corner.
#'
#' - `sec`: Short for 'section'. A section is a rectangular field of wells.
#' `sec` alone refers to the number of the section itself. `sec` combined with
#' `row` or `col` (eg `row_sec`) refers to the coordinates of a given well
#' relative to it's section corners, with the top left corner of a given section
#' always being (1, 1).
#'
#' - `rel`: Short for 'relative'. This means that the numbering is relative to
#' the `starting_corner` argument used to create the given section. For
#' instance, a section created with a `starting_corner` of `br` (bottom right)
#' will have a `row_sec_rel` and `col_sec_rel` of 1 in each of its bottom right
#' corners. A column labelled `row_rel` in this example would refer to the
#' bottom right of the _plate_ as 1, but not necessarily the bottom right of
#' each section.
#'
#' - `par`: Short for 'parent'. These columns are all the data from the previous
#' layer.
#'
#' - `lane`: These specify (usually) multiwell strips only defined in one
#' direction. `lane_h` and `lane_v` in tandem from checkerboard-like patterns,
#' where each intersection is a section. This is a bit more complicated when
#' `wrap = TRUE`, so the simile does not hold for all cases.
#'
#' - `col/row_rel_child_sec_par`: These are odd columns. They represent the axes
#' of parental sections, but using the child's relative coordinate system. For
#' example, if a parent defined sections starting in the upper left, and the
#' child defined sections starting in the upper right, `col_rel_child_sec_par`
#' would be a reversed `col_sec_par_rel`, while `row_rel_child_sec_par` would be
#' identical to `row_sec_par_rel`
#'
#' @export
#'
#' @examples
#'
#' # If you specify wells, rows and columns are derived from a standard plate sizes:
#'
#' gp(wells = 96)
#'
#' # As such, you cannot use the wells argument if you want to create more exotic plates:
#'
#' try(gp(wells = 102))
#'
#' # For that, you'll need to specify wells and cols:
#'
#' gp(rows = 6, cols = 17)
#'
gp <- function(rows = NULL, cols = NULL, data = NULL, wells = NULL, tidy = FALSE){

  plate_formats <- gp::plate_formats

  if (all(is.null(wells), is.null(cols), is.null(rows))) {
    stop("Either wells or cols + rows must be set")
  }

  if (!is.null(wells) && (!is.null(rows) || !is.null(cols))) {
    stop("wells or cols + rows can be set, not both")
  }

  if (!is.null(wells) && !wells %in% plate_formats$wells) {
    stop("wells is not of a known format (6, 12, 24, 48, 96, 384, 1536, or 3456)")
  }

  # Calculate one set of arguments from the other
  if (!is.null(wells)) {
    cols <- plate_formats[plate_formats$wells == wells,]$cols
    rows <- plate_formats[plate_formats$wells == wells,]$rows
  }

  if (is.null(data)) {
    data <- data.frame()
  }

  new_gp(nrow = as.integer(rows), ncol = as.integer(cols), data = data, tidy = tidy)
}

#' Coerce object to gp
#'
#' @param x Object to coerce
#'
#' @param ... Unused
#'
#' @export
as_gp <- function(x, ...){
  UseMethod("as_gp")
}

#' @export
#' @rdname as_gp
as_gp.default <- function(x, ...){
  gp(x, ...)
}

#' @export
#' @rdname as_gp
as_gp.list <- function(x, ...) {
  gp(x, ...)
}

#' Calculate wells of gp object
#'
#' @param x a `gp`
#' @param ... Unused
#' @export
wells <- function(x, ...){
  UseMethod("wells")
}

#' @export
#' @rdname wells
wells.gp <- function(x, ...){
  x$nrow * x$ncol
}
