
#' Function to set BirdFlowPipeline parameters
#' 
#' This returns a parameter list as used by [batch_flow()] and related 
#' functions.  If no arguments are set then the default values are returned.  
#' It is primarily for internal use from other functions.
#' 
#' It's possible to set `res` to `NULL` in which case the realized resolution
#' won't be known until after [preprocess_species_wrapper()] is called. 
#' Therefore in a standard run (regardless of whether `res` is set), parameters
#' that depend on `res` are set by [preprocess_species_wrapper()]. These are
#' `hdf_dir`, `output_path`, and `output_fullname`. 
#' 
#' @param species The species to fit, it should be set by the user or the
#' calling function.
#' @param gpu_ram Set the GPU RAM allocation in GB when fitting 
#'  the models.  It is also passed to [BirdFlowR::preprocess_species()] and 
#'  if `res` is set to `NULL` that function will set the `res` to the smallest
#'  round number that can be fit with the allocated ram.
#' @param res The resolution of model. Specifically the height and width of 
#'  the model cells in km.  Set to `NULL` to fit the finest resolution possible
#'  with the allocated `gpu_ram`.
#' @param suffix Characters to append to the output directory and model file 
#' names.
#' @param grid_search_type Either `"old"` to specify all the Python 
#' hyperparameters (`dist_pow`, `dist_weight`, and 
#'  `ent_weight`) directly; or`"new"` to uses the alternative paramerization of
#'   `dist_pow`, `obs_prop`, and `de_ratio` which are passed to the
#'    `refactor_hyperparameters` function to generate the Python parameters.
#'     In either case there is a hidden python parameter `obs_weight` which is
#'    always set to 1 when called from **BirdFlowPipeline**.
#'    `"new"` is the current default behavior because the ratio between distance 
#'    weight and entropy weight heavily determines the behavior of the model. 
#'    These hyperparameters are highly sensitive and refactoring them in this 
#'    way makes it easier to intuit about and appears to create better
#'    coverage and performance of the "grid" search.
#' @param grid_search_list These parameters are  used 
#'   in  the loss function by [BirdFlowPy](https://github.com/birdflow-science/BirdFlowPy).
#'   They are also used in the grid search when running [batch_flow()]. 
#'   All six possible parameters are in the list even though two of them won't
#'    be used  - which two depends on  `grid_search_type`. They are
#'    the set of values that will be combined factorially in the grid 
#'    search. The default values represent our currently prefered approach.  
#'    
#'    *New* and *Old* below indicate which setting of `grid_search_type` they
#'    are used with.
#'   \describe{
#'     \item{`de_ratio`}{ *New* The ratio of the distance weight to entropy weight
#'      in the loss function that is optimized while fitting the models}
#'     \item{`obs_prop`}{ *New* The proportion of the total weight (among 
#'     observation, distance, and entropy weights), that is assigned to the 
#'     observation weight in the loss function.}
#'    \item{`dist_pow`}{ *New and Old* The power used to transform movement 
#'    distances prior to weighting them in the loss function}
#'    \item{`dist_weight`}{*Old* The weight assigned to distances in the loss
#'    function.}
#'    \item{`ent_weight`}{*Old* The weight assigned to entropy in the loss
#'    function.}
#'  }
#' @param hdf_path The path to the base directory in which hdf5 preprocessed and
#' fitted models are stored.
#' @param hdf_dir The path to the directory in which this run's hdf5 
#' preprocessed and fit models are stored. Set by 
#' [preprocess_species_wrapper()] to `<hdf_path>/<species>_<res>km/`.
#' @param base_output_path  The base path for fitted models, model reports,
#'  and other output.
#' @param output_fullname Initially `NULL` this is set by 
#' [preprocess_species_wrapper] to `"<species>_<res>km<suffix>"`. 
#' @param output_path Initially `NULL` this is set by
#' [preprocess_species_wrapper] to `"<base_output_dir>_<output_fullname>"`.
#' @param season This is used in two ways:  (1) to filter movement data
#'  (I think just tracks? - ebp) before evaluating the model and (2) 
#'  if `truncate_season` is `TRUE` it is passed to  
#'  [BirdFlowR::preprocess_species()], producing a model that is truncated to 
#'  just that season.
#' @param truncate_season If `TRUE` the model will be truncated to `season` and
#' marginals for transitions outside of the season won't be fit or included.
#' @param model_selection Set how model selection is performed within 
#' [rank_models()] as called from [batch_flow()].  It should be one of: 
#' \describe{
#'   \item{`"str_etc"`}{Straightness and traverse correlation only.}
#'   \item{`"pit_etc"`}{PIT metrics and traverse correlation only.}
#'   \item{`"real_tracking"`}{Tracking-focused model selection, this
#'    includes traverse correlation; PIT scores; and straightness and 
#'    n_stopovers targeted to observed values from real 
#'    tracking data.}
#'  \item{`"real_tracking_no_cal"`}{Tracking-focused model selection,
#'  this includes traverse correlation; and straightness and n_stopovers 
#'  targeted to observed values from real tracking, but no PIT scores.)}
#'  \item{`"averaged_parameters"`}{This is a place holder for when the loss 
#'  function parameters are fixed - typically to average values - in which
#'  case no model selection is performed.}
#' }
#' @param clip This is passed to [BirdFlowR::preprocess_species()] to define a
#' clipping polygon to use while preprocessing - only areas within the polygon
#' are included in the model.
#' @param crs Passed to [BirdFlowR::preprocess_species()] to define the coordinate 
#' reference system for the model. With the default of `NULL` the CRS
#' is set to the CRS assigned to the species by eBird status and trends.
#' @param skip_quality_checks Passed to [preprocess_species()] if `TRUE` an
#' @param skip_quality_checks Passed to [preprocess_species()] if `TRUE` an
#' error will be thrown if season quality values in [ebirdst::ebirdst_runs] are
#' below three.  If `TRUE` then attempt to fit the model regardless of quality.
#' @param fit_only Set to `TRUE` to fit a model without a grid search,
#' model evaluation, model ranking, or model reports. 
#' @param ebirdst_year The version year of the ebirdst package. This shouldn't 
#' be set by users.
#' @param trim_quantile Passed to [preprocess_species()].
#' @return A parameter list to be used for `batch_flow()` and related functions
#' @export
#' @examples
#' set_pipeline_params()  # default values (new grid search)
#'
#' # Old grid search
#' set_pipeline_params(grid_search_list = list(
#'   de_ratio = NA,
#'   obs_prop = NA,
#'   dist_weight = seq(from = 0.0008, to = 0.0018, length.out = 5),
#'   ent_weight = seq(from = 0.00015, to = 0.0004, length.out = 5),
#'   dist_pow = seq(from = 0.1, to = .9, length.out = 5)))
set_pipeline_params <- function(
    species = character(0),
    gpu_ram = 10,
    res = 150,
    suffix = as.character(Sys.Date()),
    grid_search_type = 'new',
    grid_search_list = list(
      de_ratio = c(2, 4, 6, 8, 10),  
      obs_prop = c(0.95, 0.975, 0.99, 0.999, 0.9999),  
      dist_pow = seq(from = 0.1, to = 0.9, by = 0.1),  
      dist_weight = NA_real_,  
      ent_weight = NA_real_),  
    hdf_path = the$hdf_path,
    hdf_dir = NULL,
    base_output_path = the$output_path,
    output_fullname = NULL,
    output_path = NULL,
    season = 'prebreeding',
    truncate_season = FALSE,
    model_selection = 'distance_metric',
    clip = NULL,
    crs = NULL,
    skip_quality_checks = FALSE,
    fit_only = FALSE, 
    ebirdst_year = ebirdst::ebirdst_version()$version_year,
    trim_quantile = NULL
){
  params <- as.list(environment())
  
  ## Check parameters
  
  # Make sure output_fullname and output_directory are NULL.
  # Previously these weren't exposed to the user. I like having all the params
  # in the list from the start so have added them to this function. 
  # Code in preprocess_species_wrapper() could be updated to support setting
  # these to other values, but for now I'm just enforcing NULL.
  if(!is.null(params$output_fullname))
    stop("output_fullname should be NULL. Support for other values may be added later.")
  if(!is.null(params$output_path))
    stop("output_path should be NULL. Support for other values may be added later.")
  
  # Check crs and clip
  
  # preprocess_species does this too. Checking here throws immediate error
  if(!is.null(params$crs)){
    params$crs <- terra::crs(params$crs)
  }

  return(params)
}