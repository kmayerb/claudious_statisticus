#' Run a centered, scaled PCA on any dataset
#'
#' Runs principal component analysis with [stats::prcomp()] after centering
#' each variable to mean zero and scaling it to unit variance, so variables
#' measured in different units contribute equally.
#'
#' @param data A data frame or numeric matrix. Non-numeric columns are dropped
#'   with a message.
#' @param na.rm If `TRUE` (default), rows containing `NA` are dropped with a
#'   message. If `FALSE`, missing values raise an error.
#'
#' @return A `prcomp` object (with `center = TRUE`, `scale. = TRUE`).
#' @export
#' @examples
#' pca <- run_pca(mtcars)
#' summary(pca)
run_pca <- function(data, na.rm = TRUE) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }
  data <- as.data.frame(data)

  is_num <- vapply(data, is.numeric, logical(1))
  if (any(!is_num)) {
    message("Dropping non-numeric column(s): ",
            paste(names(data)[!is_num], collapse = ", "))
  }
  data <- data[, is_num, drop = FALSE]

  if (anyNA(data)) {
    if (!na.rm) stop("`data` contains missing values.", call. = FALSE)
    keep <- stats::complete.cases(data)
    message("Dropping ", sum(!keep), " row(s) with missing values.")
    data <- data[keep, , drop = FALSE]
  }

  if (ncol(data) < 2) {
    stop("PCA needs at least 2 numeric columns.", call. = FALSE)
  }
  if (nrow(data) < 2) {
    stop("PCA needs at least 2 complete rows.", call. = FALSE)
  }
  zero_var <- vapply(data, function(x) stats::var(x) == 0, logical(1))
  if (any(zero_var)) {
    stop("Cannot scale constant column(s): ",
         paste(names(data)[zero_var], collapse = ", "), call. = FALSE)
  }

  stats::prcomp(data, center = TRUE, scale. = TRUE)
}

#' Biplot of PC1 vs. PC2 with variable loadings
#'
#' Draws observations as points in PC1/PC2 space and each variable's loading
#' as an arrow from the origin. Arrows are coloured by the variable's
#' contribution (in percent) to PC1 and PC2 combined, weighted by the variance
#' each component explains, so the variables that drive the plane stand out.
#' Axis labels report the share of variance explained.
#'
#' @param pca A `prcomp` object, e.g. from [run_pca()].
#' @param label_points If `TRUE` (default), label observations with their row
#'   names (placed with 'ggrepel' to avoid overlaps); otherwise draw points
#'   only.
#' @param arrow_scale Optional multiplier for arrow length. By default arrows
#'   are stretched so the longest one reaches about 80% of the score range.
#'
#' @return A ggplot object.
#' @export
#' @examples
#' biplot_pca(run_pca(mtcars))
biplot_pca <- function(pca, label_points = TRUE, arrow_scale = NULL) {
  if (!inherits(pca, "prcomp")) {
    stop("`pca` must be a prcomp object, e.g. from run_pca().", call. = FALSE)
  }
  if (ncol(pca$rotation) < 2) {
    stop("`pca` must have at least 2 components.", call. = FALSE)
  }

  var_exp <- pca$sdev^2 / sum(pca$sdev^2)

  scores <- as.data.frame(pca$x[, 1:2, drop = FALSE])
  scores$label <- rownames(pca$x) %||% as.character(seq_len(nrow(scores)))

  loadings <- as.data.frame(pca$rotation[, 1:2, drop = FALSE])
  loadings$variable <- rownames(pca$rotation)
  # Contribution of each variable to PC1+PC2, weighted by eigenvalue (%).
  eig <- pca$sdev[1:2]^2
  contrib <- (pca$rotation[, 1]^2 * eig[1] + pca$rotation[, 2]^2 * eig[2]) /
    sum(eig)
  loadings$contribution <- 100 * contrib

  if (is.null(arrow_scale)) {
    score_range <- max(abs(unlist(scores[, 1:2])))
    load_range <- max(sqrt(loadings$PC1^2 + loadings$PC2^2))
    arrow_scale <- 0.8 * score_range / load_range
  }
  loadings$xend <- loadings$PC1 * arrow_scale
  loadings$yend <- loadings$PC2 * arrow_scale

  axis_lab <- function(i) sprintf("PC%d (%.1f%% of variance)", i, 100 * var_exp[i])

  p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data$PC1, y = .data$PC2)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
    ggplot2::geom_point(colour = "grey45", size = 1.8, alpha = 0.8)

  if (label_points) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$label),
      colour = "grey50", size = 2.6, max.overlaps = Inf,
      segment.colour = "grey80", seed = 1
    )
  }

  p +
    ggplot2::geom_segment(
      data = loadings,
      ggplot2::aes(x = 0, y = 0, xend = .data$xend, yend = .data$yend,
                   colour = .data$contribution),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm")),
      linewidth = 0.8
    ) +
    ggrepel::geom_label_repel(
      data = loadings,
      ggplot2::aes(x = .data$xend, y = .data$yend, label = .data$variable,
                   colour = .data$contribution),
      fontface = "bold", size = 3.6, show.legend = FALSE,
      fill = ggplot2::alpha("white", 0.85), label.size = NA,
      point.padding = 0, min.segment.length = Inf, seed = 1
    ) +
    ggplot2::scale_colour_gradient(
      name = "Contribution to\nPC1 + PC2 (%)", low = "#6baed6", high = "#08306b"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = axis_lab(1), y = axis_lab(2),
                  title = "PCA biplot (centered, scaled)") +
    ggplot2::theme_minimal()
}

`%||%` <- function(x, y) if (is.null(x)) y else x
