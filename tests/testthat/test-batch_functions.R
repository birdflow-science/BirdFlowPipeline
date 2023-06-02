test_that("refactor_hyperparams works", {
  expect_equal(refactor_hyperparams(2, 0.9, 5),
               c(dist_weight = 0.074074, ent_weight = 0.037037))
})
