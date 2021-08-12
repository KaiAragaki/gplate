#' Scrape object generic
#'
#' @param x Object to have layers removed from it
#' @param positions numeric or character vector specifying the positions or names of layers to be removed.
#' @param negate logical. If TRUE, values will be kept instead of removed.
#'
#' @return
#' @export
#'
#' @examples
setGeneric("scrape", function(x, positions = NULL, negate = FALSE) standardGeneric("scrape"))

#' @describeIn Plate remove (or keep) layers by character or numeric vector.
#' @param positions numeric or character vector specifying the positions or names of layers to be removed.
#' @param negate logical. If TRUE, values will be kept instead of removed.
setMethod("scrape", "Plate", function(x, positions = NULL, negate = FALSE) {

  if (class(positions) == "character") {
    positions <- which(names(x@layers) %in% positions)
  }

  if (negate) {
    x@layers <- x@layers[positions]
  } else {
    x@layers <- x@layers[-positions]
  }
  x
})
