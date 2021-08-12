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

# Constructor ------------------------------------------------------------------

Plate <- function(layers = list(), wells = NULL, meta = NULL) {

  if (any(class(layers) %in% c("matrix", "data.frame"))) {
    name <- deparse(substitute(layers))
    layers <- list(layers)
    names(layers) <- name
  }
  rows <- NULL
  cols <- NULL
  if (length(layers) > 0) {
    if (length(layers) > 1) {
      # Check that all dims are equal, stop if not

      row_dims <- layers |>
        lapply(nrow) |>
        unlist() |>
        unique()
      if (length(row_dims) > 1) return("All layers must have the same dimensions")

      col_dims <- layers |>
        lapply(ncol) |>
        unlist() |>
        unique()
      if (length(col_dims) > 1) return("All layers must have the same dimensions")

    }
    if (!is.null(wells)) {
      # Check that wells and layers dims agree, stop if not
      if (row_dims * col_dims != wells) return("If both `layers` and `wells` are specified, dimensions must agree.")
    } else {
      # Add wells and dim from layer data
      layer <- layers[[1]]
      wells <- nrow(layer) * ncol(layer)
      ind <- which(plate_dims$wells == wells)
      rows <- plate_dims$rows[ind]
      cols <- plate_dims$cols[ind]
    }
  }
  if (!is.null(wells)) {
    ind <- which(plate_dims$wells == wells)
    rows <- plate_dims$rows[ind]
    cols <- plate_dims$cols[ind]
  }

  new("Plate", layers = layers, wells = wells, meta = meta, rows = rows, cols = cols)
}




# Verification -----------------------------------------------------------------

has_wells <- function(plate) {
  !is.null(plate@wells)
}

has_layers <- function(plate) {
  !is.null(plate@layers)
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

plate_dims <- tibble::tibble(
  rows =  c(2, 3,  4,  6,  8,  16,  32,   48),
  cols =  c(3, 4,  6,  8,  12, 24,  48,   72),
  wells = c(6, 12, 24, 48, 96, 384, 1536, 3456)
)

get_dims_from_wells <- function(plate) {
  ind <- which(plate_dims$wells == plate@wells)
  list(rows = plate_dims$rows[ind], cols = plate_dims$cols[ind])
}

get_wells_from_layers <- function(plate) {
  layer <- plate@layers[[1]]
  wells(plate) <- nrow(layer) * ncol(layer)
}


# Getters/Setters --------------------------------------------------------------

## Wells -----------------------------------------------------------------------

### Getter ---------------------------------------------------------------------
setGeneric("wells", function(x) standardGeneric("wells"))
setMethod("wells", "Plate", function(x) {
  x@wells
})


### Setter ---------------------------------------------------------------------
setGeneric("wells<-", function(x, ...) standardGeneric("wells<-"))
setMethod("wells<-", "Plate", function(x, value) {
  if (length(x@layers) > 0) {
    stop("Cannot set wells on a plate that has layers.")
  }
  x@wells <- value
  validObject(x)
  dims <- get_dims_from_wells(x)
  x@rows <- dims$rows
  x@cols <- dims$cols
  x
})


## Layers ----------------------------------------------------------------------

### Getter ---------------------------------------------------------------------

# There are two ways to do this because "layers" makes more sense by far, but
# "spill" is much cuter.

setGeneric("spill", function(x, ...) standardGeneric("spill"))
setMethod("spill", "Plate", function(x, n = NULL) {
  if (is.null(n)) {
    x@layers
  } else {
    x@layers[n]
  }
})

setGeneric("layers", function(x, ...) standardGeneric("layers"))
setMethod("layers", "Plate", function(x, n = NULL) {
  if (is.null(n)) {
    x@layers
  } else {
    x@layers[n]
  }
})


### Setter ---------------------------------------------------------------------
setGeneric("layers<-", function(x, ...) standardGeneric("layers<-"))
setMethod("layers<-", "Plate", function(x, value, position = NULL) {

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
  validObject(x)
  x
})


# User Facing Functions --------------------------------------------------------

## serve -----------------------------------------------------------------------
setGeneric("serve", function(x) standardGeneric("serve"))

setMethod("serve", "Plate", function(x) {
  x@layers |>
    lapply(as.matrix) |>
    lapply(as.vector) |>
    purrr::reduce(dplyr::bind_cols) |>
    setNames(names(x@layers))
})

## scrape ----------------------------------------------------------------------
setGeneric("scrape", function(x, ...) standardGeneric("scrape"))

setMethod("scrape", "Plate", function(x, positions, negate = FALSE) {

  if (class(positions) == "character") {
    positions <- which(names(x@layers) == positions)
  }

  if (negate) {
    x@layers <- x@layers[positions]
  } else {
    x@layers <- x@layers[-positions]
  }
  x
})


# Should it check to see if the dims are of a normal form factor? Might be difficult with portioning
# That also raises the question is well# should be limited by a list
# Maybe a 'side-dish' class should be added that allows for either nonstandard wells/dims
# Or one that strips those attributes altogether (though that seems less useful)
