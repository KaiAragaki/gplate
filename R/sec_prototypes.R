#' Check if axis moves in the canonical direction
#'
#' 'Forwards' is thought of 'left to right' when thinking about moving across
#' columns and 'top to bottom' when moving across rows
#' @param gp A `gp`
#' @param dim Character. A dimension, either "row" or "col".
#'
#' @return logical.
is_fwd <- function(gp, dim, flow) {
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
unroll_sec_dim_along_parent <- function(gp, dim, flow, wrap) {

  # Row sec number is off
  # Col sec rel is flipped when it shouldn't be

  non_flow <- setdiff(c("row", "col"), flow)

  flow_sec         <- rlang::sym(paste0(".", flow, "_sec"))
  flow_sec_rel     <- rlang::sym(paste0(".", flow, "_sec_rel"))
  flow_sec_par     <- rlang::sym(paste0(".", flow, "_sec_par"))

  non_flow_sec         <- rlang::sym(paste0(".", non_flow, "_sec"))
  non_flow_sec_rel     <- rlang::sym(paste0(".", non_flow, "_sec_rel"))
  non_flow_sec_par     <- rlang::sym(paste0(".", non_flow, "_sec_par"))

  n_flow_sec         <- ifelse(flow == "row", gp$nrow_sec_mar, gp$ncol_sec_mar)
  n_non_flow_sec     <- ifelse(flow == "row", gp$ncol_sec_mar, gp$nrow_sec_mar)
  n_flow_sec_par     <- ifelse(flow == "row", gp$nrow_sec_par_mar, gp$ncol_sec_par_mar)
  n_non_flow_sec_par <- ifelse(flow == "row", gp$ncol_sec_par_mar, gp$nrow_sec_par_mar)

  index_name <- ifelse(dim == "row", ".index_col", ".index_row")

  non_dim <- setdiff(c("row", "col"), dim)

  non_dim_sec <- rlang::sym(paste0(".", non_dim, "_sec"))
  non_dim_sec_rel <- rlang::sym(paste0(".", non_dim, "_sec_rel"))
  non_dim_sec_par <- rlang::sym(paste0(".", non_dim, "_sec_par"))

  dim_sec <- rlang::sym(paste0(".", dim, "_sec"))
  dim_sec_rel <- rlang::sym(paste0(".", dim, "_sec_rel"))
  dim_sec_par <- rlang::sym(paste0(".", dim, "_sec_par"))

  n_non_dim_sec <- ifelse(dim == "row", gp$ncol_sec_mar, gp$nrow_sec_mar)
  n_dim_sec <- ifelse(dim == "row", gp$nrow_sec_mar, gp$ncol_sec_mar)
  n_non_dim_sec_par <- ifelse(dim == "row", gp$ncol_sec_par_mar, gp$nrow_sec_par_mar)

  index_name <- ifelse(dim == "row", ".index_col", ".index_row")

  section_prototype <- gp[[paste0(dim, "_unit")]]

  if (wrap) {

    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, {{ non_dim_sec_rel }}) |>
      dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype)))

    # Create desc as needed using some kind of 'relativize' function?

    if (is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ flow_sec_par }}, {{ non_flow_sec_par }})
    } else if (!is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ flow_sec_par }}, dplyr::desc({{ non_flow_sec_par }}))
    } else if (is_fwd(gp, dim) & !is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ flow_sec_par }}), {{ non_flow_sec_par }})
    } else {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ flow_sec_par }}), dplyr::desc({{ non_flow_sec_par }}))
    }

    gp$well_data <- gp$well_data |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)})) |>
      dplyr::rowwise() |>
      dplyr::mutate(max_sec = nrow(.data$data) %/% n_dim_sec + 1,
                    .sec = list(rep(1:max_sec, each = n_dim_sec, length.out = nrow(.data$data)))) |>
      tidyr::unnest(cols = c(.data$data, .data$.sec)) |>
      dplyr::mutate({{ non_flow_sec_rel }} := {{ non_flow_sec }}) |>
      dplyr::select(-max_sec)

    if (!is_fwd(gp, non_dim)) {
      gp$well_data <- gp$well_data |>
        dplyr::mutate({{ non_flow_sec }} := {{ non_flow_sec }} * -1 + 1 + n_dim_sec)
    }

    return(gp)
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.data$.sec, {{ non_dim_sec_par }}) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)}))

  if(is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ non_dim_sec_par }})
  } else {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ non_dim_sec_par }} * -1 + 1 + n_non_dim_sec_par)
  }

  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ index_name }} := (.data$temp - 1) %/% n_non_dim_sec + 1)|>
    dplyr::select(-.data$temp) |>
    tidyr::unnest(cols = .data$data)

  # Really, the prototype column should be named dim_sec_rel, and then dim_sec should be made here.
  # Or, a different arrangement strategy should be made so that the non-relative is correct.

  if (!is_fwd(gp, dim)) {
    gp$well_data <- gp$well_data |>
      dplyr::mutate({{ dim_sec_rel }} := {{ dim_sec }},
                    {{ dim_sec }} := {{ dim_sec }} * -1 + 1 + n_dim_sec)
  }
  gp$well_data <- gp$well_data |>
    dplyr::ungroup()

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

