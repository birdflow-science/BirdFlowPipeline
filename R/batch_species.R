
 
#' Fit many species with identical parameters
#' 
#' Batch fit many species without to fixed loss function parameters without
#' tuning, evaluation, or reports.
#'
#' @param species A vector of species codes or common names to fit. Must work
#' with [ebirdst::get_species()]
#' @inheritParams set_pipeline_params
#' @param de_ratio,obs_prop,dist_pow,dist_weight,ent_weight loss function 
#' parameters see definition in `grid_search_list` below under `...` 
#' @inheritDotParams set_pipeline_params -species -grid_search_type 
#' @importMethodsFrom terra crs
#' @importMethodsFrom terra vect
#' @return
#' @export
batch_species <- function(
    species,
    grid_search_type = "old",
    de_ratio = NA,
    obs_prop = NA, 
    dist_pow = 4.167e-01,
    dist_weight = 8.177e-03,
    ent_weight = 1.924e-03,
    ...
 ){
  start <- Sys.time()
  m <- paste0("Starting batch_species() run on ", length(species), 
              " species.  ", start)
  message(m)
  
  params <- set_pipeline_params(...)
  
  # Debug
  #cat("Parameters resolved to:\n")
  #print(params)
 
  # Copy grid search arguments from function call into parameter list
  # replacing default vectors with single values
  gs_names <- names(params$grid_search_list)
  for(item in gs_names)
    params$grid_search_list[item] <- list(get(item)) # list() to preserve NULL
  params$grid_search_type <- grid_search_type

  
  # Set cluster resources needed by each task
  modelfit_resources <- list(walltime = 15,
                             ngpus = 1,
                             memory = params$gpu_ram + 1)
  
  # Drop species from params  (will be passed separately)
  params$species <- NA
  reg_dir <- file.path(params$base_output_path, 
                        paste0("batch_species_", make_timestamp(), '_mf'))
  
  batchtools::batchMap(
    fun = preprocess_and_fit,
    species = species, # will be vectorized over
    more.args = list(params = params), # passed as is to each call
    reg = batchtools::makeRegistry(file.dir = reg_dir,
      conf.file = system.file('batchtools.conf.R', 
                              package = 'BirdFlowPipeline')))
  
    max_retrys <- 2  # set to 2 when everything is working, 0 for debugging
    retrys <- 0
    success <- FALSE
    while(!success && retrys <= max_retrys){
      if(retrys > 0 )
        message("Requeuing jobs that expired or had an error, attempt ", 
                retrys, " of ",  max_retrys)
      batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), 
                                           chunk = 1L),
                             resources = modelfit_resources)
      success <- batchtools::waitForJobs()
      retrys <- retrys + 1
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
    message("batch_species() completed successfully after ", format(diff, digits = 4), ".")
}
