# test_that("banding_raw_file_urls test download", {
#   skip_if_offline()
#   readme_url <- banding_raw_file_urls[grepl('^README', banding_raw_file_urls$name), 'url']
#   readme_lines <- NULL
#   expect_no_error(
#     readme_lines <- readLines(readme_url)
#   )
#   skip_if(is.null(readme_lines))
#   expect_equal(readme_lines[1], "September 2022")
#   expect_length(readme_lines, 1214)
# })

test_that("combine_together_list works", {
  suffix_vec <- sort(unique(BirdFlowPipeline::taxonomy_crosswalk$BBL_GRP))
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

test_that("preprocess_file_set works", {
  test_df <-
    structure(
      list(
        BAND = c(
          "B99320545845",
          "B99320545845",
          "B99320545845",
          "B99320545845",
          "B59305055335",
          "B59305055335",
          "B59305055335",
          "B59305055335"
        ),
        ORIGINAL_BAND = c(
          "B99320545845",
          "B29400272547",
          "B99400272322",
          "B09400272359",
          "B59305055335",
          "B59487553404",
          "B59487553259",
          "B99487553365"
        ),
        OTHER_BANDS = c("", "", "",
                        "", "", "", "", ""),
        EVENT_TYPE = c("B", "E", "E", "E", "B",
                       "E", "E", "E"),
        EVENT_DATE = c(
          "02/04/1972",
          "03/03/1972",
          "04/03/1972",
          "05/03/1972",
          "01/27/1961",
          "02/27/1961",
          "03/27/1961",
          "04/27/1961"
        ),
        EVENT_YEAR = c(1998L, 1971L, 1971L, 1971L, 1961L, 1961L, 1961L,
                       1961L),
        EVENT_MONTH = c(2L, 7L, 7L, 7L, 1L, 2L, 2L, 2L),
        EVENT_DAY = c(4L,
                      3L, 3L, 3L, 27L, 12L, 12L, 12L),
        ISO_COUNTRY = c("CA", "CA",
                        "CA", "CA", "AR", "AR", "AR", "AR"),
        ISO_SUBDIVISION = c("CA-NL",
                            "CA-NU", "CA-NU", "CA-NU", "", "", "", ""),
        LAT_DD = c(
          47.25,
          77.1,
          78.58333,
          79.58333,
          -51.75,
          -52.75,
          -53.75,
          -54.75
        ),
        LON_DD = c(
          -53.58333,
          -84.08333,
          -85.08333,
          -86.08333,
          -57.25,-58.25,
          -59.25,
          -58.25
        ),
        CP = c(10L, 10L, 10L, 10L, 10L,
               10L, 10L, 10L),
        BAND_TYPE = c("11", "11", "11", "11", "11",
                      "11", "11", "11"),
        SPECIES_ID = c(390L, 390L, 390L, 390L,
                       391L, 391L, 391L, 391L),
        BIRD_STATUS = c(3L, 3L, 3L, 3L,
                        3L, 3L, 3L, 3L),
        EXTRA_INFO = c(0L, 8L, 8L, 8L, 0L, 0L, 0L,
                       0L),
        AGE_CODE = c(1L, 0L, 0L, 0L, 4L, 4L, 4L, 4L),
        SEX_CODE = c(0L,
                     0L, 0L, 0L, 0L, 0L, 0L, 0L),
        PERMIT = c(
          "P7837147",
          "P8831030",
          "P8831030",
          "P8831030",
          "P9993099",
          "P9993099",
          "P9993099",
          "P9993099"
        ),
        BAND_STATUS = c("0", "0", "0", "0", "0", "0",
                        "0", "0"),
        HOW_OBTAINED = c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        WHO_OBTAINED = c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        REPORTING_METHOD = c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        PRESENT_CONDITION = c(
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_,
          NA_integer_
        ),
        MIN_AGE_AT_ENC = c(
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_
        ),
        RECORD_SOURCE = c("B",
                          "B", "B", "B", "B", "B", "B", "B")
      ),
      row.names = c(NA,-8L),
      class = "data.frame"
    )
  raw_path <- file.path(tempdir(), 'test_raw')
  rds_path <- file.path(tempdir(), 'test_rds')
  if (!dir.exists(raw_path)) dir.create(raw_path)
  if (!dir.exists(rds_path)) dir.create(rds_path)
  test_csv_path <- file.path(raw_path, 'test_csv.csv')
  on.exit({
    unlink(raw_path, recursive = TRUE)
    unlink(rds_path, recursive = TRUE)
  })
  write.csv(test_df, test_csv_path, row.names = FALSE)
  expect_no_error(utils::capture.output(
    process_file_set(collapse_list_item = test_csv_path,
                                   banding_raw_path = raw_path,
                                   banding_rds_path = rds_path)))
  expect_true(file.exists(file.path(rds_path, 'ivogul.rds')))
})
