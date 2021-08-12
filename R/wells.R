#' Wells accessor generic
#'
#' @param x Object to access wells from.
#'
#' @return numeric, number of wells in the object.
#' @export
#'
#' @examples
setGeneric("wells", function(x) standardGeneric("wells"))

#' @describeIn Plate access number of wells on plate.
#' @param x a Plate object.
setMethod("wells", "Plate", function(x) {
  x@wells
})


#' Wells setter generic
#'
#' @param x Object to set wells.
#' @param value numeric. Number of wells to set.
#'
#' @return x
#' @export
#'
#' @examples
setGeneric("wells<-", function(x, value) standardGeneric("wells<-"))

#' @describeIn Plate set well number on a plate object.
#' @param x a Plate object.
#' @param value numeric. Number of wells to set.
setMethod("wells<-", "Plate", function(x, value) {
  if (length(x@layers) > 0) {
    stop("Cannot set wells on a plate that has layers.")
  }
  x@wells <- value
  methods::validObject(x)
  dims <- get_dims_from_wells(x)
  x@rows <- dims$rows
  x@cols <- dims$cols
  x
})
