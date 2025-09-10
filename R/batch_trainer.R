#' @export
batch_trainer <- function(species, ...){
  if(!length(species) == 1){
    stop("batch_flow() only works with one species. ", 
         "Use multiple_species_batch()")
  }
  
  trainer <- new_batch_trainer(species, ...)
  validate_batch_trainer(trainer)
  
  return(trainer)
}

new_batch_trainer <- function(species, ...){

  params <- set_pipeline_params(species = species, ...)
  
  # preprocess species and set up directories
  params <- preprocess_species_wrapper(params)
  
  # Make the object
  obj <- list(
    params = params
  )
  
  class(obj) <- c('BatchBirdFlowTrainer', class(obj))
  return(obj)
}

#' @importFrom generics fit
#' @method fit BatchBirdFlowTrainer
#' @export
fit.BatchBirdFlowTrainer <- function(trainer, ...) {
  batch_modelfit_wrapper(trainer$params)
  print(paste0('Finished batch training for species: ', trainer$params$species))
  invisible(trainer)
}

#' @method save_metadata BatchBirdFlowTrainer
#' @export
save_metadata.BatchBirdFlowTrainer <- function(trainer, ...) {
  # Save finalized parameters
  saveRDS(trainer$params, file.path(trainer$params$output_path, 'params.rds'))
  invisible(NULL)
}

#' @param object The object to save meta data
#' @export
save_metadata <- function(object, ...) {
  UseMethod("save_metadata")
}






