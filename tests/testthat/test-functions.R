test_that("evaluate_model works for no-tracking data species", {
  tempdir1 <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  on.exit({
    if (file.exists(tempdir1)) unlink(tempdir1, recursive = TRUE)
  })
  expect_no_error({
    eval_obj <- evaluate_model(
      BirdFlowModels::rewbla,
      'fake_modelname',
      list(
        obs_df = BirdFlowModels::rewbla_observations,
        int_df = BirdFlowModels::rewbla_intervals
      ),
      params = list(my_species = 'rewbla', output_path = tempdir1)
    )
  })
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
                       rts <- BirdFlowR::route(bf = BirdFlowModels::rewbla, n = 100, season = 'prebreeding', from_marginals = TRUE)
                     })
                   })
  # test route stats
  expect_no_error({
    stats <- rts_stats(rts)
  })
  expect_equal(stats,
               list(straightness = 0.823304236754776,
                    length = 1463.41057052456, displacement = 845.890889687627, n_stopovers = 0.78))
})

test_that("interval_log_likelihood correctly handles 0-row input", {
  expected_zero_row_output <- structure(list(BAND_TRACK = character(0), from = integer(0), 
                          to = integer(0), log_likelihood = numeric(0), null_ll = numeric(0), 
                          lag = numeric(0), exclude = logical(0), not_active = logical(0), 
                          dynamic_mask = logical(0), sparse = logical(0), same_timestep = logical(0), 
                          bad_date = logical(0)), row.names = integer(0), class = "data.frame")
  zero_row_track_info <- list(obs_df = structure(list(BAND = character(0), EVENT_TYPE = character(0), 
                                                      date = structure(numeric(0), class = "Date"), lat = numeric(0), 
                                                      lon = numeric(0), EBIRDST_CODE = character(0), BAND_TRACK = character(0), 
                                                      distance = numeric(0), days = integer(0), when = character(0), 
                                                      id = integer(0)), row.names = integer(0), class = "data.frame"), 
                              int_df = structure(list(BAND_TRACK = character(0), from = integer(0), 
                                                      to = integer(0)), row.names = integer(0), class = "data.frame"))
  zero_row_track_info_with_obs <- list(obs_df = structure(list(BAND = c("B08132304611", "B08132304611", 
    "B08152001250"), EVENT_TYPE = c("B", "E", "B"), date = structure(c(18771, 
    18772, 18962), class = "Date"), lat = c(45.5, 45.25, 30.5), lon = c(-72.5, 
    -72.08333, -91.5), EBIRDST_CODE = c("amewoo", "amewoo", "amewoo"
    ), BAND_TRACK = c("B08132304611_1", "B08132304611_1", "B08152001250_1"
    ), distance = c(NA, 42.8629243499647, NA), days = c(NA, 1L, NA
    ), when = c("from", "to", "from"), id = 1:3), row.names = c(NA, 
    -3L), class = "data.frame"), int_df = structure(list(BAND_TRACK = character(0), 
    from = integer(0), to = integer(0)), row.names = integer(0), class = "data.frame"))
  
  # zero-row everything
  expect_equal(BirdFlowR::interval_log_likelihood(zero_row_track_info$int_df,
                                                  zero_row_track_info$obs_df,
                                                  BirdFlowModels::amewoo),
               expected_zero_row_output
               )
  # zero-row intervals but some observations
  expect_equal(BirdFlowR::interval_log_likelihood(zero_row_track_info_with_obs$int_df,
                                                  zero_row_track_info_with_obs$obs_df,
                                                  BirdFlowModels::amewoo),
               expected_zero_row_output
  )
})
