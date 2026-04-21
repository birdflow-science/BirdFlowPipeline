test_that("plot_gridsearch_metrics runs cleanly", {
  # Setup data
  files <- c("sstspa_model.hdf5", 
             "sstspa_test_metrics.rds", 
             "sstspa_train_metrics.rds")
  paths <- lapply(paste0("rmd/example_data/", files), 
                  function(x) system.file(x, package = "BirdFlowPipeline"))
  names(paths) <- c("model", "test_metrics", "train_metrics")
  bf <- import_birdflow(paths$model)
  test_metrics <- readRDS(paths$test_metrics)
  train_metrics <- readRDS(paths$train_metrics)

  included_metrics <- c("mean_dist_cor", "mean_ll", "mean_energy_improvement",
    "weighted_mean_win_prob", "weighted_mean_win_distance",
    "weighted_mean_win_distance_fraction", "weighted_mean_ll",
    "weighted_energy_improvement", "pit_row", "pit_col")

  expect_no_error(
  p <- plot_grid_search_metrics(train = train_metrics, test = test_metrics, bf, 
                           include = included_metrics,  cor_threshold = 0.9, 
                           cor_metric = "min_dist_cor")
  )
  
  expect_no_error(print(p))
  })
