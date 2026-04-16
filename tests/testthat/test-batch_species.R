test_that("man/batch_species() works", {
  
  dir <- withr::local_tempdir()
  dir <- "~/temp_batch_species_testing/"
  dir.create(dir, showWarnings = TRUE)
  
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
  
  unlink("~/temp_batch_species_testing/", recursive = TRUE)
})
