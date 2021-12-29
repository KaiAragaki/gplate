#' A constructor for a gp object
#'
#' @param data
#' @param wells
#' @param dimensions
#'
#' @return
#' @export
#'
#' @examples
new_gp <- function(nrow = 1L, ncol = 1L){

  stopifnot(is.integer(nrow), is.integer(ncol))

  wells <- nrow * ncol

  nrow_sec      <- nrow
  nrow_sec_par  <- nrow
  ncol_sec      <- ncol
  ncol_sec_par  <- ncol
  wells_sec     <- wells
  wells_sec_par <- wells

  well_data <- tidyr::expand_grid(row = seq_len(nrow), col = seq_len(ncol))

  well_data$sec             <- 1L
  well_data$sec_par         <- 1L

  well_data$row_rel         <- well_data$row
  well_data$row_sec         <- well_data$row
  well_data$row_sec_rel     <- well_data$row
  well_data$row_sec_par     <- well_data$row
  well_data$row_sec_par_rel <- well_data$row

  well_data$col_rel         <- well_data$col
  well_data$col_sec         <- well_data$col
  well_data$col_sec_rel     <- well_data$col
  well_data$col_sec_par     <- well_data$col
  well_data$col_sec_par_rel <- well_data$col

  structure(list(nrow = nrow,
                 ncol = ncol,
                 wells = wells,
                 well_data = well_data,
                 nrow_sec = nrow_sec,
                 nrow_sec_par = nrow_sec_par,
                 ncol_sec = ncol_sec,
                 ncol_sec_par = ncol_sec_par,
                 wells_sec = wells_sec,
                 wells_sec_par = wells_sec_par),
            class = "gp")
}

# if any two of the arguments (rows, cols, wells) are supplied, should be able
# to calculate the third

#' Make gp
#'
#' @param data
#' @param wells
#' @param cols
#' @param rows
#'
#' @return
#' @export
#'
#' @examples
gp <- function(data = data.frame(), wells = NULL, cols = NULL, rows = NULL){

  if (all(missing(wells), missing(cols), missing(rows))) {
    stop("Either wells or cols + rows must be set")
  }

  if (!missing(wells) && (!missing(rows) || !missing(cols))) {
    stop("wells or cols + rows can be set, not both")
  }

  if (!missing(wells) && !wells %in% plate_formats$wells) {
    stop("wells is not of a known format (6, 12, 24, 48, 96, 384, 1536, or 3456)")
  }

  # Calculate one set of arguments from the other
  if (missing(wells)) {
    wells <- cols * rows
  } else {
    cols <- plate_formats[plate_formats$wells == wells,]$cols
    rows <- plate_formats[plate_formats$wells == wells,]$rows
  }

  structure(list(data = data,
                 rows = rows,
                 cols = cols,
                 wells = wells),
            class = "gp")
}

#' Coerce object to gp
#'
#' @param x
#'
#' @param ...
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
