#' An S4 class to represent microwell plate data
#'
#' @slot layers a list of matrices
#' @slot meta a named list of characteristics that refer to the whole plate
#' @slot wells a numeric that describes how many wells are in the plate
#' @slot rows a numeric denoting the number of rows on the plate
#' @slot cols a numeric denoting the number of cols on the plate
#'
#' @name Plate-class
#' @rdname Plate-class
#' @export
#' @include zzz.R

setClass("Plate",
         slots = c(
           layers = "list_or_null",
           meta = "list_or_null",
           wells = "numeric_or_null",
           rows = "numeric_or_null",
           cols = "numeric_or_null"),
         prototype = c(
           layers = list(),
           meta = NULL,
           wells = NULL,
           rows = NULL,
           cols = NULL
         )
)

## Layers ----------------------------------------------------------------------

# Should it check to see if the dims are of a normal form factor? Might be difficult with portioning
# That also raises the question is well# should be limited by a list
# Maybe a 'side-dish' class should be added that allows for either nonstandard wells/dims
# Or one that strips those attributes altogether (though that seems less useful)

# Verification -----------------------------------------------------------------

setValidity("Plate", function(object) {
  is_valid_form_factor(object)
})

