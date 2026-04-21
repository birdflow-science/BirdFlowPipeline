test_that("New model report renders", {
  html_file <- withr::local_tempfile(pattern = "new_model_report", fileext = ".html")  
  p <- list()
  p$model <-  system.file("rmd/example_data/sstspa_model.hdf5", package = "BirdFlowPipeline")
  p$bmtr <-  system.file("rmd/example_data/sstspa_bmtr.Rds", package = "BirdFlowPipeline")
  p$anim <-  system.file("rmd/example_data/sstspa_anim.gif", package = "BirdFlowPipeline")
  p$html <-  html_file <- withr::local_tempfile(pattern = "new_model_report", fileext = ".html")  
  p$routes <-  system.file("rmd/example_data/sstspa_routes.hdf5", package = "BirdFlowPipeline")
  p$prevlink <-  NA
  p$nextlink <-  NA
  p$test_metrics <-  system.file("rmd/example_data/sstspa_test_metrics.rds", package = "BirdFlowPipeline") |> readRDS()
  p$train_metrics <-  system.file("rmd/example_data/sstspa_train_metrics.rds", package = "BirdFlowPipeline")   |> readRDS()
  p$title <- "Example Species Report"
  p$cor_threshold <- 0.9
  p$cor_metric <- "min_dist_cor"
  p$mean_rating <- 4
  
  expect_no_error(
    do.call(make_new_report, args = p)
  )
  stopifnot(file.exists(html_file))

})
