test_that("plots are ggplot scatter plots that render", {
  for (p in list(plot_mtcars(), plot_iris())) {
    expect_s3_class(p, "ggplot")
    expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})
