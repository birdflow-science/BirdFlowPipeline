testthat::test_that("find_xy works", {
  testthat::expect_equal(find_xy(2, 0.9, 5),
               c(x = 0.074074, y = 0.037037))
})
