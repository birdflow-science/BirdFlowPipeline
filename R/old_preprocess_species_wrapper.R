
#' Wrapper for [BirdFlowR::preprocess_species()] that also prepares batch parameters
#' @param params A list of starting parameters, currently set up in [batch_flow()]
#' @returns The starting params list, modified to include additional information calculated during preprocessing. Also has side effects:
#'  * create directories as needed
#'  * write preprocessed hdf5 file
#' @seealso [batch_flow()]
#'
#' @export
old_preprocess_species_wrapper <- function(params, return_format = "new") {
  
  params$species <- ebirdst::get_species(params$species)
  
  pp_dir <- tempdir()
  suppressMessages(
    invisible(
      utils::capture.output(
        bf <- BirdFlowR::preprocess_species(
          species = params$species,
          out_dir = pp_dir,
          gpu_ram = params$gpu_ram,
          res = params$res,
          season = dplyr::if_else(params$truncate_season, params$season, 'all'),
          clip = params$clip,
          crs = params$crs,
          skip_quality_checks = params$skip_quality_checks, 
          trim_quantile = params$trim_quantile
        )
      )
    )
  )
  # Update parameters
  
  params$res <- BirdFlowR::res(bf)[1] / 1000
  params$ebirdst_year <- bf$metadata$ebird_version_year
  params$output_fullname <- paste0(params$species, '_', params$res, 'km', '_', params$suffix)
  params$hdf_dir <- file.path(params$hdf_path, paste0(params$species, '_', params$res, 'km'))
  params$output_path <- file.path(params$base_output_path, params$output_fullname)
  
  # Create directories
  
  dir.create(params$hdf_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(params$output_path, showWarnings = FALSE, recursive = TRUE)
  
  # move preprocessed file to hdf_dir 
  
  temp_preprocessed_file <- list.files(path = pp_dir,
                                       pattern = paste0('^', params$species, '.*', params$res, 'km.*\\.hdf5$'),
                                       full.names = TRUE)
  
  stopifnot(length(temp_preprocessed_file) == 1, 
            file.exists(temp_preprocessed_file))
  
  
  preprocess_file_name <- basename(temp_preprocessed_file)
  final_preprocess_file <- file.path(params$hdf_dir, preprocess_file_name)
  use_rename <- FALSE  
  # testing renaming instead of copying. It should be more efficient but
  # might not work if different drives.
  # Some tests failed with rename.
  if(use_rename){
    if(file.exists(final_preprocess_file))
      file.remove(final_preprocess_file)
    file.rename(temp_preprocessed_file, final_preprocess_file)
  } else {
    # Old way: copy and then delete original
    file.copy(temp_preprocessed_file, params$hdf_dir, overwrite = TRUE)
    if (file.exists(temp_preprocessed_file)) 
      file.remove(temp_preprocessed_file)
  } 
  stopifnot(file.exists(final_preprocess_file))
  
  params
}