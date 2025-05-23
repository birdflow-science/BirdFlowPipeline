#' Import a BirdFlow model, calculate flux, and save to an RDS file
#' 
#' `process_flux()` is a wrapper to [calc_flux()] that handles reading the input
#' model and writing the resulting flux to disk.
#' @param model_path Path to a BirdFlow model file, either an HDF5 
#' to be read with [BirdFlowR::import_birdflow()] or and RDS handled by 
#' [readRDS()].
#' 
#' @param flux_path The path to and RDS file (ending in `".rds"`) to write the
#' result to.
#' 
#' @return Flux is written to disk nothing is returned.
#' @export
process_flux <- function(model_path, flux_path) {
  
  if (!grepl("\\.rds$", flux_path, ignore.case = TRUE)) {
    stop("flux_path should end in \".rds\"")
  }
  gc()
  if (grepl("\\.rds$", model_path, ignore.case = TRUE)) {
    bf <- readRDS(model_path)
    BirdFlowR::validate_BirdFlow(bf)
  } else {
    bf <- BirdFlowR::import_birdflow(model_path)
  }
  flux <- BirdFlowR::calc_flux(bf, batch_size = 1e5 )
  saveRDS(flux, flux_path)
  gc()
}