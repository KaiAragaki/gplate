#' Print a gp object
#'
#' @param x A gp object
#' @param ... Additional parameters to pass to `print`
#'
#' @export
print.gp <- function(x, ...) {

  l_mar <- rep(" ", nchar(x$nrow) + 1)

  cat(c("\n", l_mar, " ", rep(" ", x$ncol_sec %/% 2),
        x$ncol_sec,
        "\n"), sep = "")

  cat(c(l_mar, " ", rep("_", x$ncol_sec)), sep = "")
  cat(c("\n", rep(c(l_mar, "|", rep("o", x$ncol_sec), "\n"), x$nrow_sec %/% 2)), sep = "")
  cat(c(x$nrow, c(" |", rep("o", x$ncol_sec), "\n")), sep = "")
  cat(c(rep(c(l_mar, "|", rep("o", x$ncol_sec), "\n"), x$nrow_sec - x$nrow_sec %/% 2 - 1), "\n"), sep = "")

  cli::cli_text("Start corner: {x$start_corner}")
}
