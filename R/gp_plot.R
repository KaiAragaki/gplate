#' Plot a `gp` object
#'
#' @param gp A `gp` object
#' @param name Symbol. Name of a column in `gp$well_data` to use as a color.
#'
#' @return a `ggplot`
#' @export
#'
#' @examples
gp_plot <- function(gp, name) {
    wd <- gp$well_data
    ggplot2::ggplot(wd, ggplot2::aes(x = col, y = row, color = as.factor({{ name }}))) +
      ggplot2::geom_point(size = 4) +
      ggplot2::scale_y_reverse()
}
