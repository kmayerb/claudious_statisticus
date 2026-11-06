# claudiousstatisticus

R package: `ggplot2` scatter plots of built-in datasets, plus PCA with biplots (labels placed with `ggrepel`).

```r
# install.packages("remotes")
remotes::install_github("kmayerb/claudious_statisticus")

library(claudiousstatisticus)
plot_mtcars()  # weight vs. mpg
plot_iris()    # sepal length vs. width, by species

# PCA on any numeric data (centered and scaled to unit variance)
pca <- run_pca(mtcars)  # non-numeric columns and NA rows are dropped with a message
summary(pca)
biplot_pca(pca)         # scores + loading arrows, coloured by contribution to PC1/PC2
```

## Development

```r
devtools::test()   # run testthat tests
devtools::check()  # full R CMD check
```

CI runs `R CMD check` on every push and pull request (`.github/workflows/R-CMD-check.yaml`).
