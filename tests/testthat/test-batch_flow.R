test_that("batch_flow works", {
  # skip if not linux
  skip_on_os(
    c("windows", "mac", "solaris")
  )
  # skip if not on Unity
  skip_if_not(
    system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu"
  )
  # test batch_flow()
  test_hdf_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf/amewoo_700km_TEST"
  test_output_path <- "/home/dslager_umass_edu/banding_output/amewoo_700km_TEST"
  on.exit({
    if (file.exists(test_hdf_dir)) unlink(test_hdf_dir, recursive = TRUE)
    if (file.exists(test_output_path)) unlink(test_output_path, recursive = TRUE)
  })
  expect_no_error(
    batch_flow(
      one_species = 'American Woodcock',
      params = list(
        my_species = character(0),
        gpu_ram = 10,
        my_res = 700,
        output_nickname = 'TEST',
        grid_search_type = 'new',
        grid_search_list = list(
          de_ratio = 8,
          obs_prop = c(0.95, 0.99),
          dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
          dist_weight = NA_real_,
          ent_weight = NA_real_),
        batch_hdf_path = the$batch_hdf_path,
        banding_output_path = the$banding_output_path
      )
    )
  )
  expect_true(file.exists(test_hdf_dir))
  expect_true(file.exists(test_output_path))
  test_output_files <- list.files(test_output_path)
  test_hdf_files <- list.files(test_hdf_dir, full.names = TRUE)
  expect_no_error(ll_df <- readRDS(file.path(test_output_path, 'll_df.rds')))
  # two file sizes -- unfitted hdf5 and fitted hdf5s
  expect_true(length(unique(file.size(test_hdf_files))) == 2)
  # nrow(ll_df) is same as number of modelfit files
  test_modelfit_files <- grep('km\\.hdf5', test_hdf_files, invert = TRUE, value = TRUE)
  expect_setequal(basename(test_modelfit_files), ll_df$model)
  # desirability graphs
  expect_true(
    all(
      c(paste0('desirability', 1:5, '.pdf'),
        'track_info.rds',
        'params.rds',
        'pca_evaluation.pdf'
      ) %in% test_output_files
    )
  )
})
