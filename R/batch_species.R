if(FALSE){
  # some arguments used in Dave's big run 
  
  # Dave's original list
  og_species_list <- readRDS('~/dave_work_in_progress/big_run/migratory_species_list.rds')
  
  # Table of models that Dave produced (based on output files):
  species <- readRDS("/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/big_run/index.Rds")$species
  
  # These were species that presumable couldn't be fit due to metdata issues 
  # or zero abundance in some timesteps.  I'm leaving them off for now.
  dropped_species <- setdiff(og_species_list, species)
  
  sp <- species[1]
  
  # Setup params object for direct call to preprocess_and_fit()
  # in local session - this requires a GPU and that .Renviron is configured
  # as per 
  # https://github.com/birdflow-science/BirdFlowPipeline?tab=readme-ov-file#7-optional-tips-for-developing-for-pythongpu-in-rstudio-using-the-container
  
  params <- set_pipeline_params()
  params$grid_search_type <- "old"
  gl <- params$grid_search_list
  gl$dist_pow = 4.167e-01
  gl$dist_weight = 8.177e-03
  gl$ent_weight = 1.924e-03
  gl$de_ratio <- NA
  gl$obs_prop <- NA
  params$grid_search_list <- gl
  params$suffix <- "bigruntest"
  params$skip_quality_checks <- TRUE
  
  
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL=0)
  # local test
  preprocess_and_fit(species[8], params)
  
  # Test with a few species:
  batch_species(species = species[10:11], skip_quality_checks = TRUE, res = 150, gpu_ram = 10, suffix = "bigrun2")
  
  end <- Sys.time()
  diff <- end - start
  message("batch_species() completed successfully after ", format(diff, digits = 4), ".")
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
  start <- Sys.time()
  m <- paste0("Starting batch_species() run on ", length(species), 
              " species.  ", start)
  message(m)
  
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
  
  # Drop species from params  (will be passed separately)
  params$species <- NA
  
  batchtools::batchMap(
    fun = preprocess_and_fit,
    species = species, # will be vectorized over
    more.args = list(params = params), # passed as is to each call
    reg = batchtools::makeRegistry(
      file.path(params$base_output_path, paste0("batch_species_",  
                                                make_timestamp(), '_mf')),
      conf.file = system.file('batchtools.conf.R', 
                              package = 'BirdFlowPipeline')))
  
    max_retrys <- 0  # set to 2 when everything is working, 0 for debugging
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
    }
}
