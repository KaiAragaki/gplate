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

make_layers_from_names <- function(x, names = NULL) {

  if (!is.null(names)){
    arg_name <- names
  } else {
    arg_name <- deparse(substitute(x))
  }

  rows <- NULL
  cols <- NULL

  values <- x |>
    as.matrix() |>
    unname()

  # Check if rownames are default
  if (!is.null(rownames(x)) && !any(rownames(x) == as.character(1:nrow(x)))) {
    rows <-
      rep(rownames(x), times = ncol(x)) |>
      matrix(nrow(x), ncol(x))
  }

  # Check if colnames exist
  if (!is.null(colnames(x))) {
    cols <-
      rep(colnames(x), each = nrow(x)) |>
      matrix(nrow(x), ncol(x))
  }

  out <- list(values, rows, cols)
  names(out) <- c(arg_name, paste0(arg_name, "_rows"), paste0(arg_name, "_cols"))

  # Remove NULLs
  null_list <- out |>
    lapply(is.null) |>
    unlist()

  out <- out[!null_list]

  out
}
