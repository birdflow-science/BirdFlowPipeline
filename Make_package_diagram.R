library(BirdFlowPipeline)
library(pkgdepR)
library(htmlwidgets)

setwd('/home/yc85_illinois_edu/BirdFlowPipeline/')
p <- deps(pkg = "BirdFlowPipeline") |>
  plot(option = "D", alpha = 0.8, main = list(text = NULL))

saveWidget(p, file = "pkgdeps.html", selfcontained = TRUE)
