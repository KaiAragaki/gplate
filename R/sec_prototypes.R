#' Check if axis moves in the canonical direction
#'
#' 'Forwards' is thought of 'left to right' when thinking about moving across
#' columns and 'top to bottom' when moving across rows
#' @param gp A `gp`
#' @param dim Character. A dimension, either "row" or "col".
#'
#' @return logical.
is_fwd <- function(gp, dim) {
  ifelse(dim == "row",
         gp$start_corner %in% c("tl", "tr"),
         gp$start_corner %in% c("tl", "bl"))
}

#' Repeat one dimension of a child section across a dimension of a parent section
#'
#'
#' @param gp A `gp`
#' @param dim Either "row" or "col"
#'
#' @return A `gp`
#'
unroll_sec_dim_along_parent <- function(gp, dim, wrap) {

  non_dim <- setdiff(c("row", "col"), dim)

  non_dim_sec     <- rlang::sym(paste0(".", non_dim, "_sec"))
  non_dim_sec_par <- rlang::sym(paste0(".", non_dim, "_sec_par"))

  dim_sec     <- rlang::sym(paste0(".", dim, "_sec"))
  dim_sec_par <- rlang::sym(paste0(".", dim, "_sec_par"))

  n_non_dim_sec <- ifelse(dim == "row", gp$ncol_sec_mar, gp$nrow_sec_mar)
  n_dim_sec <- ifelse(dim == "row", gp$nrow_sec_mar, gp$ncol_sec_mar)
  n_non_dim_sec_par <- ifelse(dim == "row", gp$ncol_sec_par_mar, gp$nrow_sec_par_mar)

  index_name <- ifelse(dim == "row", ".index_col", ".index_row")

  section_prototype <- gp[[paste0(dim, "_unit")]]

  gp$well_data <- gp$well_data |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype)))

  if (wrap) {
    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, {{ non_dim_sec }})
  } else {
    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, {{ non_dim_sec_par }})
  }

  gp$well_data <- gp$well_data |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)}))

  if (wrap) {
    gp$well_data <- gp$well_data |>
      dplyr::rowwise() |>
      dplyr::mutate(max_sec = nrow(.data$data) %/% n_dim_sec + 1,
                    .sec = list(rep(1:max_sec, each = n_dim_sec, length.out = nrow(.data$data)))) |>
      tidyr::unnest(cols = dplyr::everything()) |>
      dplyr::select(-max_sec)
  } else {
    # Reducible if I make rel flipper (see below)
    if(is_fwd(gp, non_dim)) {
      gp$well_data$temp <- gp$well_data[[non_dim_sec_par]]
    } else {
      gp$well_data$temp <- flip_dim(gp, non_dim_sec_par)
    }

    gp$well_data <- gp$well_data |>
      dplyr::mutate({{ index_name }} := (.data$temp - 1) %/% n_non_dim_sec + 1)|>
      dplyr::select(-.data$temp) |>
      tidyr::unnest(cols = dplyr::everything())
  }

  # Reducible I think
  if (!is_fwd(gp, dim)) {
    gp$well_data[[dim_sec]] <- flip_dim(gp, dim_sec)
  }

  gp$well_data <- dplyr::ungroup(gp$well_data)

  gp

}

arrange_by_rel_dim <- function(gp, dim) {

  non_dim <- setdiff(c("row", "col"), dim)

  dim_sec_par     <- rlang::sym(paste0(".", dim, "_sec_par"))
  non_dim_sec_par <- rlang::sym(paste0(".", non_dim, "_sec_par"))

  if (is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, {{ non_dim_sec_par }})
  } else if (is_fwd(gp, dim) & !is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, dplyr::desc({{ non_dim_sec_par }}))
  } else if (!is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), {{ non_dim_sec_par }})
  } else {
    gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), dplyr::desc({{ non_dim_sec_par }}))
  }

  gp
}

flip_dim <- function(gp, dim) {
  # the dim is a symbol, so it needs special (arcane) treatment
  dim <- rlang::as_name(rlang::quo(!!dim))

  # rm leading dot
  no_dot <- substr(dim, 2, nchar(dim))

  n_dim <- gp[[paste0("n", no_dot)]]

  gp$well_data[[dim]] * -1 + 1 + n_dim
}

# Make a conditional flipper - wrapper around flip_dim that checks if is_fwd and only acts if !is_fwd
