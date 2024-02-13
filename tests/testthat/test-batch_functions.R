test_that("refactor_hyperparams works", {
  expect_equal(refactor_hyperparams(2, 0.9, 5),
               list(dist_weight = 0.074074, ent_weight = 0.037037))
})


test_that("birdflow_modelfit_args_df() works", {
  params <- set_pipeline_params()

  params$species <- "amewoo"
  params$hdf_dir <- file.path(params$hdf_path, "testdir")
  
  args_df <- birdflow_modelfit_args_df(params)
  
  expect_snapshot(args_df[c(1:5, 30:32), -1])
  
})