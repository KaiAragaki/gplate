setClassUnion("list_or_null", c("list", "NULL"))
setClassUnion("numeric_or_null", c("numeric", "NULL"))


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
  length(plate@layers) > 1
}

layers_are_correct_dim <- function(plate) {
  if (has_multiple_layers(plate)) {

  }
}

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
