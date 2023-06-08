test_that("evaluate_model works", {
  expect_no_error(
    {
      eval_obj <- evaluate_model(BirdFlowModels::rewbla, 'fake_modelname', list(
        obs_df = BirdFlowModels::rewbla_observations,
        int_df = BirdFlowModels::rewbla_intervals
      ))
    }
  )
  # when these 4 tests fail, it probably means we have new test model
  # can remove safe_numeric() calls in evaluate_model()
  expect_null(eval_obj$bf$metadata$hyperparameters$obs_weight)
  expect_null(eval_obj$bf$metadata$hyperparameters$ent_weight)
  expect_null(eval_obj$bf$metadata$hyperparameters$dist_weight)
  expect_null(eval_obj$bf$metadata$hyperparameters$dist_pow)
})
