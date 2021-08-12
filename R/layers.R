#' Layer accessor generic
#'
#' @param x Object to access layers of
#' @param n optional, layer position to access. If ommited, returns all layers
#'
#' @return
#' @export
#'
#' @examples
setGeneric("layers", function(x, n) standardGeneric("layers"))

#' @describeIn Plate accessor for plate layers
#' @param n optional, layer position to access. If ommited, returns all layers
setMethod("layers", "Plate", function(x, n = NULL) {
  if (is.null(n)) {
    x@layers
  } else {
    x@layers[n]
  }
})

#' Layer setter generic
#'
#' @param x Object to set layers of
#' @param position layer position to add or modify
#' @param value Value to set as layers
#'
#' @return
#' @export
#'
#' @examples
setGeneric("layers<-", function(x, position, value) standardGeneric("layers<-"))

#' @describeIn Plate add/overwrite a plate layer
#' @param position layer position to add or modify
setMethod("layers<-", "Plate", function(x, position = NULL, value) {

  if (has_wells(x)) {
    if (x@rows != nrow(value) | x@cols != ncol(value)) {
      stop("`wells` and `layer` dimensions must agree")
    }
  }

  if(!has_layers(x)) {
    wells(x) <- nrow(value) * ncol(value)
  }

  name <- deparse(substitute(value))

  if(is.null(position)) {
    curr_names <- names(x@layers)
    x@layers[[length(x@layers) + 1]] <- value
    names(x@layers) <- c(curr_names, name)
  } else {
    x@layers[[position]] <- value
    names(x@layers)[position] <- name
  }
  methods::validObject(x)
  x
})

#' Layer accessor generic
#'
#' @param x Object to be accessed
#' @param n optional, character name or numeric position of layer to access
#'
#' @return
#' @export
#'
#' @examples
setGeneric("spill", function(x, n = NULL) standardGeneric("spill"))

#' @describeIn Plate Alternative and cuter method to access layers
#' @param n optional, character name or numeric position of layer to access
setMethod("spill", "Plate", function(x, n = NULL) {
  if (is.null(n)) {
    x@layers
  } else {
    x@layers[n]
  }
})
