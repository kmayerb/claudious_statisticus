# claudiousstatisticus

Placeholder R package: two `ggplot2` scatter plots of built-in datasets.

```r
# install.packages("remotes")
remotes::install_github("kmayerb/claudious_statisticus")

library(claudiousstatisticus)
plot_mtcars()  # weight vs. mpg
plot_iris()    # sepal length vs. width, by species
```

## Development

```r
devtools::test()   # run testthat tests
devtools::check()  # full R CMD check
```

CI runs `R CMD check` on every push and pull request (`.github/workflows/R-CMD-check.yaml`).
