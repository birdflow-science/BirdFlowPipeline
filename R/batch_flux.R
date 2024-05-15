

#' Calculate flux for multiple species
#' 
#' Batch calculate flux in parallel using slurm
#'
#' @param model_paths The path to one or more BirdFlow models saved either
#' as HDF5 or [RDS][base::readRDS] files.  
#' @param flux_paths Paths (ending in `".rds"`) to save flux to. 
#' It should be the same length as `model_paths` or `NULL` in which case 
#' `"_flux.rds"` will be appended to `model_paths` after removing the extension.
#' @param base_path The directory in which the batch tools registry directory
#' should be created (used to manage the run).  If `NULL` the directory of
#' the first path in `model_paths` will be used.  Either way the registry 
#' directory will be a concatenation of `"batch_flux"`, the date, and `"_mf"`.
#' @param memory GB of memory allocated for each flux calculation. 
#' @param walltime Ellapsed time in minutes allocated to the job.
#' @return Nothing is returned.
batch_flux <- function(model_paths, flux_paths = NULL, base_path = NULL,
                       memory = 12, walltime = 180 ) {
  
  start <- Sys.time()
  
  m <- paste0("Starting batch_flux() run on ", length(model_paths), 
              " models.  ", start)
  message(m)
  
  
  # Set cluster resources needed by each task
  modelfit_resources <- list(walltime = walltime,
                             ncpus = 1,
                             memory = memory)
  
  if(is.null(base_path)){
    base_path <- dirname(model_paths[1])
  }
  
  
  
  reg_dir <- file.path(base_path, 
                       paste0("batch_flux_", make_timestamp(), '_mf'))
  
  
  batchtools::batchMap(
    fun = process_flux,
    model_path = model_paths, # will be vectorized over
    flux_path = flux_paths, # will be vectorized over
    reg = batchtools::makeRegistry(
      file.dir = reg_dir,
      conf.file = system.file('batchtools.conf.R',
                              package = 'BirdFlowPipeline')))
  
  max_retries <- 0  # set to 2 when everything is working, 0 for debugging
  
  # Ramp up memory and time with each retry
#  memory <- c(12, 18, 24, rep(24, 4))
#  walltime <- c(180, 240, 480, rep(600, 4) )
  
  retries <- 0
  success <- FALSE
  while(!success && retries <= max_retries){
    if(retries > 0 )
      message("Requeuing jobs that expired or had an error, attempt ", 
              retries, " of ",  max_retries)
 #   modelfit_resources$memory <- memory[retries + 1]
    
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), 
                                         chunk = 1L),
                           resources = modelfit_resources)
    success <- batchtools::waitForJobs()
    retries <- retries + 1
  }
  
  if (!isTRUE(success)) {
    first_error <- batchtools::findErrors()$job.id[1] 
    first_err_msg <- batchtools::getErrorMessages(first_error)$message
    log <- batchtools::getLog(first_error)
    cat("Log of first error - batchtools::getLog(", first_error, "):\n")
    print(log)
    cat("\n\nRegistry directory:", reg_dir, "\n")
    stop("Not all runs competed.  First error message:\n", first_err_msg)
  }
  
  end <- Sys.time()
  diff <- end - start
  message("batch_flux() completed successfully after ", 
          format(diff, digits = 4), ".")
}
