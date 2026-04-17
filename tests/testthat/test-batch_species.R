test_that("man/batch_species() works", {

  # test fails using devtools::check(), for confusing reasons
  # this line makes this test run with devtools::test() but not devtools::check()
  skip_if_not(interactive())
  
  # skip if not linux
  skip_on_os(
    c("windows", "mac", "solaris")
  )
  # skip if not on Unity
  skip_if_not(
    system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu",
    message = "Not on unity so not testing batch_species()"
  )
  
  # Note test failed in some contexts with tempdir() and also with dir
  # within ~ now hardcoding path within /work
  dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/plunkett/zzz_temp_batch_species_testing/"
  dir.create(dir, showWarnings = TRUE)
  dir <- normalizePath(dir, mustWork = TRUE)
  
  
  species <- "gowwar"
  
  trim_high_values <- TRUE # TRUE to run with a trim quantile
  trim_quantile <- 0.99  # See preprocess_species() trim_quantile argument
  
  res <- 200
  
 # Sys.getenv("APPTAINER_CACHEDIR")
  
  #------------------------------------------------------------------------------#
  # Launch
  #------------------------------------------------------------------------------#
  
  gpu_ram <- 5
  
  batch_species(species = species, 
                skip_quality_checks = TRUE, 
                res = res, 
                gpu_ram = gpu_ram,
                hdf_path = dir, 
                base_output_path = dir,
                trim_quantile = trim_quantile,
                grid_search_type = "old",
                dist_pow = 4.167e-01,
                dist_weight = 8.177e-03,
                ent_weight = 1.924e-03)
  
  output_file <- file.path(dir, "gowwar_200km", "gowwar_2023_200km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5")
  
  expect_true(file.exists(output_file))
  
  expect_no_error(bf <- import_birdflow(output_file))
  
  expect_no_error(validate_BirdFlow(bf, error = TRUE))
  expect_true(has_marginals(bf))
  
  unlink(dir, recursive = TRUE)
})
