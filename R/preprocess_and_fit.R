
#' Process and fit a single model with fixed parameters
#' 
#' This is primarily intended for use by [batch_species()] but could
#' also be called directly to fit a single species. 
#' 
#' @param species A species code appropriate for [ebirdst::get_species()].
#' @param params A parameter list similar to that returned by
#' [set_pipeline_params()] but for this function each of the
#' `grid_search` parameter list items should be a single value.
#'
#' @return This function is called for the side affect of creating a fitted
#' hdf5 model file.
#' @export
preprocess_and_fit <- function(species, params){
  
  # Set species in params
  params$species <- species
  
  # Preprocess a single species
  params <- preprocess_species_wrapper(params)
  
  saveRDS(params, file.path(params$output_path, 'params.rds'))
  
  # Call single model
  args <- params[c("hdf_dir", "species", "res", "ebirdst_year")]
  names(args)[names(args) =="hdf_dir"] <- "dir"
  args <- c(args, params$grid_search_list)
  if(params$grid_search_type == "new"){
    a <- refactor_hyperparams(args$de_ratio, args$obs_prop)
    args$dist_weight <- a$dist_weight
    args$ent_weight <- a$ent_weight
  }
  args <- args[!names(args) %in% c("de_ratio", "obs_prop")]
  
  if(!all(sapply(args, length) %in% c(0, 1)))
    stop("Unexpected vector arguments. ", 
         "All arguments should have a length of one.")
  
  stopifnot(all(
    c("dir", "species", "res", "dist_pow", "dist_weight", "ent_weight", 
      "ebirdst_year") %in% names(args)))
  
  do.call(birdflow_modelfit, args = args)
  
}