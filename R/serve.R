#' Serve generic
#'
#' @param x Object to be served
#'
#' @return
#' @export
#'
#' @examples
setGeneric("serve", function(x) standardGeneric("serve"))

#' @describeIn Plate method to convert a plate object into a tidy tibble
setMethod("serve", "Plate", function(x) {

  x@layers |>
    lapply(as.matrix) |>
    lapply(as.vector) |>
    purrr::reduce(dplyr::bind_cols) |>
    stats::setNames(names(x@layers)) |>
    dplyr::bind_cols(x@meta)

})
