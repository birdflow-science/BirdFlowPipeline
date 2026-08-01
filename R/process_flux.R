#' Import a BirdFlow model, calculate flux, and save to an RDS file
#' 
#' `process_flux()` is a wrapper to [BirdFlowR::calc_flux()] that handles reading the input
#' model and writing the resulting flux to disk.
#' @param model_path Path to a BirdFlow model file, either an HDF5 
#' to be read with [BirdFlowR::import_birdflow()] or and RDS handled by 
#' [readRDS()].
#' 
#' @param flux_path The path to and RDS file (ending in `".rds"`) to write the
#' result to.
#' @inheritParams BirdflowR::calc_bmtr
#' 
#' @return Flux is written to disk nothing is returned.
#' @export
process_flux <- function(model_path, flux_path, 
                         method = c("binary","continuous", 
                                    "continuous-spherical"), 
                         ...) {
  
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
  
  batch_size <- switch(method,
                       "binary" = 1e5,
                       "continuous" = 1e4,
                       "continuous-spherical" = 5e3,
                       5e5
  )
  flux <- BirdFlowR::calc_bmtr(bf, batch_size = batch_size, method = method, ...)
  
  if (!dir.exists(dirname(flux_path))){
    message("Creating directory for BMTR file at:", dirname(flux_path))
    dir.create(dirname(flux_path), recursive = TRUE)
  }
  saveRDS(flux, flux_path)
  
  gc()
}