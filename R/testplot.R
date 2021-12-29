
dev_plot_gp <- function(gp, name) {


  wd <- gp$well_data

  ggplot(wd, aes(x = col, y = row, fill = as.factor({{ name }}))) +
    geom_tile(size = 1, color = "black") +
    scale_y_reverse()

}


dev_plot_wd <- function(wd, name) {

  ggplot(wd, aes(x = col, y = row, color = as.factor({{ name }}))) +
    geom_point(size = 10) +
    scale_y_reverse()
}
