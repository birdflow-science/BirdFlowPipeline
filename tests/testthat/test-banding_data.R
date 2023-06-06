test_that("banding_raw_file_urls test download", {
  skip_if_offline()
  readme_url <- banding_raw_file_urls[grepl('^README', banding_raw_file_urls$name), 'url']
  readme_lines <- NULL
  expect_no_error(
    readme_lines <- readLines(readme_url)
  )
  skip_if(is.null(readme_lines))
  expect_equal(readme_lines[1], "September 2022")
  expect_length(readme_lines, 1214)
})

