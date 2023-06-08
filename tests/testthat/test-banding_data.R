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

test_that("combine_together_list works", {
  suffix_vec <- sort(unique(banding::taxonomy_crosswalk$BBL_GRP))
  together_list <- list(c("_02", "_03"), c("_42", "_43"), c("_55", "_56"))
  test_result <- list("_01", c("_02", "_03"), "_04", "_05", "_06", "_07", "_08", 
                      "_09", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", 
                      "_18", "_19", "_20", "_21", "_22", "_23", "_24", "_25", "_26", 
                      "_27", "_28", "_29", "_30", "_31", "_32", "_33", "_34", "_35", 
                      "_36", "_37", "_38", "_39", "_40", "_41", c("_42", "_43"), 
                      "_44", "_45", "_46", "_47", "_48", "_49", "_50", "_51", "_52", 
                      "_53", "_54", c("_55", "_56"), "_57")
  expect_equal(combine_together_list(suffix_vec, together_list),
               test_result)
})
