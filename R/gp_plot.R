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

#' A theme for making little in-line plots
#'
#' @return
#' @export
gp_mini_theme <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#FFFFF8",
                                                          color = "#FFFFF8"),
                 plot.background = ggplot2::element_rect(fill = "#FFFFF8"),
                 strip.background = ggplot2::element_rect(fill = "#FFFFF8"),
                 legend.background = ggplot2::element_rect(fill = "#FFFFF8"),
                 legend.position = "none",
                 legend.key = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())
}
