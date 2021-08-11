# Class definition of Plate object

setClassUnion("list_or_null", c("list", "NULL"))
setClassUnion("numeric_or_null", c("numeric", "NULL"))


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


# Verification -----------------------------------------------------------------

has_wells <- function(plate) {
  !is.null(plate@wells)
}

is_valid_form_factor <- function(plate) {
  if (has_wells(plate) && !(plate@wells %in% plate_dims$wells)) {
    return("`wells` does not match a valid form factor (6, 12, 24, 48, 96, 384, 1536, or 3456)")
  }
}

has_multiple_layers <- function(plate) {
  length(object@layers) > 1
}

layers_are_correct_dim <- function(plate) {
  if (has_multiple_layers(plate)) {

  }
}

setValidity("Plate", function(object) {
  is_valid_form_factor(object)
})


# Utilities --------------------------------------------------------------------

plate_dims <- tibble(rows =  c(2, 3,  4,  6,  8,  16,  32,   48),
                     cols =  c(3, 4,  6,  8,  12, 24,  48,   72),
                     wells = c(6, 12, 24, 48, 96, 384, 1536, 3456))

get_dims_from_wells <- function(plate) {
  ind <- which(plate_dims$wells == plate@wells)
  list(rows = plate_dims$rows[ind], cols = plate_dims$cols[ind])
}



# Setters ----------------------------------------------------------------------

## Wells -----------------------------------------------------------------------

### Getter ---------------------------------------------------------------------
setGeneric("wells", function(x) standardGeneric("wells"))
setMethod("wells", "Plate", function(x) {
  x@wells
})

### Setter ---------------------------------------------------------------------
setGeneric("wells<-", function(x, ...) standardGeneric("wells<-"))
setMethod("wells<-", "Plate", function(x, value) {
  x@wells <- value
  validObject(x)
  dims <- get_dims_from_wells(x)
  x@rows <- dims$rows
  x@cols <- dims$cols
  x
})


