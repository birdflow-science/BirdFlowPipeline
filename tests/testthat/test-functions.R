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

test_that("routes stats work", {
  # test route generation
  withr::with_seed(42,
                   {
                     expect_no_error({
                       rts <- BirdFlowR::route_migration(BirdFlowModels::rewbla, 100, 'prebreeding')
                     })
                   })
  # test route stats
  expect_no_error({
    stats <- rts_stats(rts)
  })
  expect_equal(stats,
               list(straightness = 0.823304236754776, sinuosity = 0.00184305321524582, 
                    length = 1463.41057052456, displacement = 845.890889687627))
})
