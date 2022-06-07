#' Print a gp object
#'
#' @param x A gp object
#' @param ... Additional parameters to pass to `print`
#' @export
#' @keywords internal
print.gp <- function(x, ...) {

  if (sum(x$nrow_sec) > 50 | sum(x$ncol_sec) + 5 > cli::console_width()) {
    cli::cli_text(cli::col_silver("Section dimensions: "),  "{x$nrow_sec} x {x$ncol_sec}")
  } else {
    l_mar <- rep(" ", nchar(sum(x$nrow_sec_mar)) + 1)

    cat(cli::col_silver(c("\n", l_mar, "  ", rep("  ", sum(x$ncol_sec_mar) %/% 2),
          cli::col_none(sum(x$ncol_sec_mar)),
          "\n")), sep = "")

    o <- paste0(" ", cli::symbol$circle)

    cat(cli::col_silver(c(l_mar, " ", rep("__", sum(x$ncol_sec_mar)))), sep = "")
    cat(cli::col_silver(c("\n", rep(c(l_mar, "|", rep(o, sum(x$ncol_sec_mar)), "\n"), sum(x$nrow_sec_mar) %/% 2))), sep = "")
    cat(cli::col_silver(c(cli::col_none(sum(x$nrow_sec_mar)), c(" |", rep(o, sum(x$ncol_sec_mar)), "\n"))), sep = "")
    cat(cli::col_silver(c(rep(c(l_mar, "|", rep(o, sum(x$ncol_sec_mar)), "\n"), sum(x$nrow_sec_mar) - sum(x$nrow_sec_mar) %/% 2 - 1), "\n")), sep = "")
  }

  cli::cli_text(cli::col_silver("Start corner: "), "{x$start_corner}")
  cli::cli_text(cli::col_silver("Plate dimensions: "), "{x$nrow} x {x$ncol}")
}
