ignore_unused_imports <- function() {
  # this function is never actually used
  # https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
  # need to use unexported autoplot.princomp method from ggfortify in model_evaluation_biplot()
  ggfortify::ggbiplot
}
