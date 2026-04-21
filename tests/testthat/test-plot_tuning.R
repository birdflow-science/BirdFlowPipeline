test_that("plot tuning runs cleanly", {
  
  files <- c("sstspa_model.hdf5", 
             "sstspa_test_metrics.rds", 
             "sstspa_train_metrics.rds")
  paths <- lapply(paste0("rmd/example_data/", files), 
                  function(x) system.file(x, package = "BirdFlowPipeline"))
  
  names(paths) <- c("model", "test_metrics", "train_metrics")
  
  bf <- import_birdflow(paths$model)
  
  test_metrics <- readRDS(paths$test_metrics)
  train_metrics <- readRDS(paths$train_metrics)

  expect_no_error(
  p <- plot_tuning(train = train_metrics, test = test_metrics, bf, 0.9 )
  )
  expect_no_error(
    print(p)
  )
  
})
