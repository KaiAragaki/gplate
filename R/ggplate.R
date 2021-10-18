#' A constructor for gplates
#'
#' @param data
#' @param wells
#' @param dimensions
#'
#' @return
#' @export
#'
#' @examples
new_gplate <- function(data = list(), rows = double(), cols = double(), wells = double()){
  stopifnot(is.list(data),
            is.double(rows),
            is.double(cols),
            is.double(wells))

  structure(list(data = data,
                 rows = rows,
                 cols = cols,
                 wells = wells),
            class = "gplate")
}

gplate <- function(data, wells = NULL, cols = NULL, rows = NULL){

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
            class = "gplate")
}
