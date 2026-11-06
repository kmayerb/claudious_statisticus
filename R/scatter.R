#' Scatter plot of mtcars
#'
#' Plots car weight against fuel economy.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' plot_mtcars()
plot_mtcars <- function() {
  ggplot2::ggplot(datasets::mtcars, ggplot2::aes(x = .data$wt, y = .data$mpg)) +
    ggplot2::geom_point()
}

#' Scatter plot of iris
#'
#' Plots sepal length against sepal width, coloured by species.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' plot_iris()
plot_iris <- function() {
  ggplot2::ggplot(
    datasets::iris,
    ggplot2::aes(x = .data$Sepal.Length, y = .data$Sepal.Width, colour = .data$Species)
  ) +
    ggplot2::geom_point()
}

#' @importFrom ggplot2 .data
NULL
