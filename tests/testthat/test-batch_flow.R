test_that("batch_flow works", {
  # test fails using devtools::check(), for confusing reasons
  # this line makes this test run with devtools::test() but not devtools::check()
  skip_if_not(interactive())
  # skip if not linux
  skip_on_os(
    c("windows", "mac", "solaris")
  )
  # skip if not on Unity
  skip_if_not(
    system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu"
  )
  # test batch_flow()
  test_species <- "Wood Thrush"
  test_species <- ebirdst::get_species(test_species)
  test_res <- 700
  tempdir1 <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  tempdir2 <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  on.exit({
    if (file.exists(tempdir1)) unlink(tempdir1, recursive = TRUE)
    if (file.exists(tempdir2)) unlink(tempdir2, recursive = TRUE)
  })
  test_hdf_dir <- tempdir1
  test_output_path <- tempdir2
  suppressWarnings(
    expect_no_error(
      batch_flow(
        species = test_species,
        gpu_ram = 10,
        res = test_res,
        suffix = 'TEST',
        grid_search_type = 'new',
        grid_search_list = list(
          de_ratio = 8,
          obs_prop = c(0.95, 0.99),
          dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
          dist_weight = NA_real_,
          ent_weight = NA_real_),
        hdf_path = test_hdf_dir,
        base_output_path = test_output_path,
        season = 'prebreeding',
        truncate_season = FALSE,
        model_selection = 'real_tracking',
        clip = NULL,
        skip_quality_checks = FALSE,
        fit_only = FALSE
      )))
  expect_true(file.exists(test_hdf_dir))
  expect_true(file.exists(test_output_path))

})
