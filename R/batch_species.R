if(FALSE){
  # some arguments used in Dave's big run 
  
  # Dave's original list:
  og_species_list <- readRDS('migratory_species_list.rds')
  
  # Table of models that Dave produced (based on output files):
  species <- readRDS("/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/big_run/index.Rds")$species
  
  # These were species that presumable couldn't be fit due to metdata issues 
  # or zero abundance in some timesteps.  I'm leaving them off for now.
  dropped_species <- setdiff(og_species_list, species)

}
  
 
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
#'
#' @return
#' @export
#'
#' @examples
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
  
  params <- set_pipeline_params(...)
  
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
  
  # Drop species from params 
  params$species <- NULL
  
  batchtools::batchMap(
    fun = preprocess_and_fit,
    species = species, # will be vectorized over
    more.args = list(params = params), # passed as is to each call
    reg = batchtools::makeRegistry(
      file.path(params$base_output_path, paste0("batch_species_",  make_timestamp(), '_mf')),
      conf.file = system.file('batchtools.conf.R', 
                              package = 'BirdFlowPipeline')))
  
    max_retrys <- 1
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
      stopifnot(isTRUE(success))
      invisible(success)
    } else {
      stop("failed")
    }
}
