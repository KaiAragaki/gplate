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
         gp$start_corner %in% c("tl", "bl"),
         gp$start_corner %in% c("tl", "tr"))
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

  dim_sec_par <- ifelse(dim == "row", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  dim_sec_rel_par <- ifelse(dim == "row", rlang::expr(.col_sec_rel_par), rlang::expr(.row_sec_rel_par))
  non_dim_sec_par <- ifelse(dim == "row", rlang::expr(.row_sec_par), rlang::expr(.col_sec_par))
  non_dim_sec_rel_par <- ifelse(dim == "row", rlang::expr(.row_sec_rel_par), rlang::expr(.col_sec_rel_par))
  ndim_sec <- ifelse(dim == "row", gp$ncol_sec_mar, gp$nrow_sec_mar)
  ndim_sec_par <- ifelse(dim == "row", gp$ncol_sec_par_mar, gp$nrow_sec_par_mar)
  index_name <- ifelse(dim == "row", ".index_col", ".index_row")
  dim_sec <- ifelse(dim == "row", rlang::expr(.col_sec), rlang::expr(.row_sec))
  dim_sec_rel <- ifelse(dim == "row", rlang::expr(.col_sec_rel), rlang::expr(.row_sec_rel))
  non_dim_sec <- ifelse(dim == "row", rlang::expr(.row_sec), rlang::expr(.col_sec))
  non_dim_sec_rel <- ifelse(dim == "row", rlang::expr(.row_sec_rel), rlang::expr(.col_sec_rel))
  if(dim == "row") {
    section_prototype <- gp[["row_unit"]]
  } else {
    section_prototype <- gp[["col_unit"]]
  }

  if (wrap) {
    dim_sec_rel <- ifelse(dim == "row", rlang::expr(.col_sec_rel), rlang::expr(.row_sec_rel)) # Need rel?
    non_dim_sec_rel <- ifelse(dim == "row", rlang::expr(.row_sec_rel), rlang::expr(.col_sec_rel)) # Need rel?


    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, {{ dim_sec_rel }}) |>
      dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype)))

    if (is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, {{ non_dim_sec_par }})
    } else if (!is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), {{ non_dim_sec_par }})
    } else if (is_fwd(gp, dim) & !is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, dplyr::desc({{ non_dim_sec_par }}))
    } else {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), dplyr::desc({{ non_dim_sec_par }}))
    }

    gp$well_data <- gp$well_data |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)})) |>
      dplyr::rowwise() |>
      dplyr::mutate(max_sec = nrow(.data$data) %/% ndim_sec + 1,
                    .sec = list(rep(1:max_sec, each = ndim_sec, length.out = nrow(.data$data)))) |>
      tidyr::unnest(cols = c(.data$data, .data$.sec)) |>
      dplyr::mutate({{ non_dim_sec_rel }} := {{ non_dim_sec }}) |>
      dplyr::select(-max_sec)

    if (!is_fwd(gp, non_dim)) {
      gp$well_data <- gp$well_data |>
        dplyr::mutate({{ non_dim_sec }} := {{ non_dim_sec }} * -1 + 1 + ndim_sec)
    }



    return(gp)
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.data$.sec, {{ dim_sec_par }}) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)}))

  if(is_fwd(gp, dim)) {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }})
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec }})
  } else {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }} * -1 + 1 + ndim_sec_par)
  }

# flowing dim doesn't seem to be working?
  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ index_name }} := (.data$temp - 1) %/% ndim_sec + 1)|>
    dplyr::select(-.data$temp) |>
    tidyr::unnest(cols = .data$data)

  if (!is_fwd(gp, non_dim)) {
    gp$well_data <- gp$well_data |>
      dplyr::mutate({{ non_dim_sec_rel }} := {{ non_dim_sec }},
                    {{ non_dim_sec }} := {{ non_dim_sec }} * -1 + 1 + ndim_sec)
  }
  gp$well_data <- gp$well_data |>
    dplyr::ungroup()

  gp
}

arrange_by_rel_dim <- function(gp, dim) {

  dim_sec_par <- ifelse(dim == "row",
                        rlang::expr(.row_sec_rel_par),
                        rlang::expr(.col_sec_rel_par))

    if (is_fwd(gp, dim)) {
      gp$well_data <- gp$well_data |> dplyr::arrange({{ dim_sec_par }})
    } else {
      gp$well_data <- gp$well_data |> dplyr::arrange(dplyr::desc({{ dim_sec_par }}))
    }

  gp
}

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
         gp$start_corner %in% c("tl", "bl"),
         gp$start_corner %in% c("tl", "tr"))
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

  dim_sec_par <- ifelse(dim == "row", rlang::expr(.col_sec_par), rlang::expr(.row_sec_par))
  dim_sec_rel_par <- ifelse(dim == "row", rlang::expr(.col_sec_rel_par), rlang::expr(.row_sec_rel_par))
  non_dim_sec_par <- ifelse(dim == "row", rlang::expr(.row_sec_par), rlang::expr(.col_sec_par))
  non_dim_sec_rel_par <- ifelse(dim == "row", rlang::expr(.row_sec_rel_par), rlang::expr(.col_sec_rel_par))
  ndim_sec <- ifelse(dim == "row", gp$ncol_sec_mar, gp$nrow_sec_mar)
  non_ndim_sec <- ifelse(dim == "row", gp$nrow_sec_mar, gp$ncol_sec_mar)
  ndim_sec_par <- ifelse(dim == "row", gp$ncol_sec_par_mar, gp$nrow_sec_par_mar)
  index_name <- ifelse(dim == "row", ".index_col", ".index_row")
  dim_sec <- ifelse(dim == "row", rlang::expr(.col_sec), rlang::expr(.row_sec))
  dim_sec_rel <- ifelse(dim == "row", rlang::expr(.col_sec_rel), rlang::expr(.row_sec_rel))
  non_dim_sec <- ifelse(dim == "row", rlang::expr(.row_sec), rlang::expr(.col_sec))
  non_dim_sec_rel <- ifelse(dim == "row", rlang::expr(.row_sec_rel), rlang::expr(.col_sec_rel))
  if(dim == "row") {
    section_prototype <- gp[["row_unit"]]
  } else {
    section_prototype <- gp[["col_unit"]]
  }

  if (wrap) {
    dim_sec_rel <- ifelse(dim == "row", rlang::expr(.col_sec_rel), rlang::expr(.row_sec_rel)) # Need rel?
    non_dim_sec_rel <- ifelse(dim == "row", rlang::expr(.row_sec_rel), rlang::expr(.col_sec_rel)) # Need rel?


    gp$well_data <- gp$well_data |>
      dplyr::group_by(.data$.sec, {{ dim_sec_rel }}) |>
      dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype)))

    if (is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, {{ non_dim_sec_par }})
    } else if (!is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), {{ non_dim_sec_par }})
    } else if (is_fwd(gp, dim) & !is_fwd(gp, non_dim)) {
      gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, dplyr::desc({{ non_dim_sec_par }}))
    } else {
      gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), dplyr::desc({{ non_dim_sec_par }}))
    }

    gp$well_data <- gp$well_data |>
      tidyr::nest() |>
      dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)})) |>
      dplyr::rowwise() |>
      dplyr::mutate(max_sec = nrow(.data$data) %/% non_ndim_sec + 1,
                    .sec = list(rep(1:max_sec, each = non_ndim_sec, length.out = nrow(.data$data)))) |>
      tidyr::unnest(cols = c(.data$data, .data$.sec)) |>
      dplyr::mutate({{ non_dim_sec_rel }} := {{ non_dim_sec }}) |>
      dplyr::select(-max_sec)

    if (!is_fwd(gp, non_dim)) {
      gp$well_data <- gp$well_data |>
        dplyr::mutate({{ non_dim_sec }} := {{ non_dim_sec }} * -1 + 1 + ndim_sec)
    }



    return(gp)
  }

  gp$well_data <- gp$well_data |>
    dplyr::group_by(.data$.sec, {{ dim_sec_par }}) |>
    dplyr::select(setdiff(colnames(gp$well_data), colnames(section_prototype))) |>
    tidyr::nest() |>
    dplyr::mutate(data = purrr::map(.data$data, \(x) {cbind(non_int_replicate(section_prototype, x), x)}))

  if(is_fwd(gp, dim)) {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }})
    # gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec }})
  } else {
    gp$well_data <- dplyr::mutate(gp$well_data, temp = {{ dim_sec_par }} * -1 + 1 + ndim_sec_par)
  }

# flowing dim doesn't seem to be working?
  gp$well_data <- gp$well_data |>
    dplyr::mutate({{ index_name }} := (.data$temp - 1) %/% ndim_sec + 1)|>
    dplyr::select(-.data$temp) |>
    tidyr::unnest(cols = .data$data)

  if (!is_fwd(gp, non_dim)) {
    gp$well_data <- gp$well_data |>
      dplyr::mutate({{ non_dim_sec_rel }} := {{ non_dim_sec }},
                    {{ non_dim_sec }} := {{ non_dim_sec }} * -1 + 1 + ndim_sec)
  }
  gp$well_data <- gp$well_data |>
    dplyr::ungroup()

  gp
}

arrange_by_rel_dim <- function(gp, dim) {

  non_dim <- setdiff(c("row", "col"), dim)

  dim_sec_par <- ifelse(dim == "row",
                        rlang::expr(.row_sec_rel_par),
                        rlang::expr(.col_sec_rel_par))
  non_dim_sec_par <- ifelse(dim == "row",
                        rlang::expr(.col_sec_rel_par),
                        rlang::expr(.row_sec_rel_par))

  # I think these are wonky (see above too - copied)
  if (is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, {{ non_dim_sec_par }})
  } else if (!is_fwd(gp, dim) & is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, {{ non_dim_sec_par }})
  } else if (is_fwd(gp, dim) & !is_fwd(gp, non_dim)) {
    gp$well_data <- dplyr::arrange(gp$well_data, {{ dim_sec_par }}, dplyr::desc({{ non_dim_sec_par }}))
  } else {
    gp$well_data <- dplyr::arrange(gp$well_data, dplyr::desc({{ dim_sec_par }}), dplyr::desc({{ non_dim_sec_par }}))
  }

  gp
}

