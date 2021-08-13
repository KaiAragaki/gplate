#' Plate constructor
#'
#' @param layers a matrix or dataframe, or a list of matrices/dataframes
#' @param wells optional numeric of number of wells the plate contains. Useful
#'   when instantiating a plate without layers. Must match layer dimensions, if
#'   provided.
#' @param meta an optional named list of whole-plate descriptors
#'
#' @return a `Plate` object
#' @export
#'
#' @examples
Plate <- function(layers = list(), wells = NULL, meta = NULL) {

  if (any(class(layers) %in% c("matrix", "data.frame"))) {
    layer_name <- deparse(substitute(layers))
    layers <- make_layers_from_names(layers, layer_name)
  }

  if (class(layers) == "list") {
    layers <- layers |>
      purrr::map2(names(layers), make_layers_from_names) |>
      purrr::flatten()
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

  methods::new("Plate", layers = layers, wells = wells, meta = meta, rows = rows, cols = cols)
}

