test_that("run_pca centers and scales mtcars", {
  pca <- run_pca(mtcars)
  expect_s3_class(pca, "prcomp")
  expect_equal(unname(pca$center), unname(colMeans(mtcars)))
  expect_equal(unname(pca$scale), unname(apply(mtcars, 2, stats::sd)))
  # Matches prcomp directly.
  ref <- stats::prcomp(mtcars, center = TRUE, scale. = TRUE)
  expect_equal(pca$sdev, ref$sdev)
  # Scaled PCA: eigenvalues sum to number of variables.
  expect_equal(sum(pca$sdev^2), ncol(mtcars))
})

test_that("run_pca drops non-numeric columns and NA rows", {
  expect_message(pca <- run_pca(iris), "Species")
  expect_equal(rownames(pca$rotation), names(iris)[1:4])

  df <- mtcars
  df$mpg[1:3] <- NA
  expect_message(pca <- run_pca(df), "3 row")
  expect_equal(nrow(pca$x), nrow(mtcars) - 3)
  expect_error(run_pca(df, na.rm = FALSE), "missing")
})

test_that("run_pca rejects bad input", {
  expect_error(run_pca(1:10), "data frame")
  expect_error(run_pca(mtcars[, "mpg", drop = FALSE]), "2 numeric")
  df <- mtcars
  df$const <- 1
  expect_error(run_pca(df), "const")
})

test_that("biplot_pca returns a renderable ggplot with loadings", {
  p <- biplot_pca(run_pca(mtcars))
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomPoint", "GeomSegment", "GeomTextRepel", "GeomLabelRepel") %in% geoms))
  expect_no_error(ggplot2::ggplot_build(p))

  seg <- p$layers[[which(geoms == "GeomSegment")]]$data
  expect_equal(seg$variable, names(mtcars))
  # Weighted contributions sum to 100%.
  expect_equal(sum(seg$contribution), 100)
  expect_match(p$labels$x, "^PC1 \\(\\d+\\.\\d% of variance\\)$")

  expect_no_error(ggplot2::ggplot_build(biplot_pca(run_pca(mtcars), label_points = FALSE)))
  expect_error(biplot_pca(mtcars), "prcomp")
})
