#' Construct a batch BirdFlow trainer
#'
#' Creates and validates a [BatchBirdFlowTrainer()] object for a single species,
#' bundling preprocessed eBird Status & Trends inputs and all parameters required
#' for downstream grid search and model fitting.
#'
#' @param species Character scalar. eBird species code or name resolvable by
#'   [ebirdst::get_species()].
#' @param ... Additional arguments forwarded to [new_BatchBirdFlowTrainer()] (and
#'   ultimately to [set_pipeline_params()] and preprocessing).
#'
#' @return A [BatchBirdFlowTrainer()] object (invisible).
#' @seealso [new_BatchBirdFlowTrainer()], [fit.BatchBirdFlowTrainer()],
#'   [preprocess_species_wrapper()]
#' @export
#' @examples
#' \dontrun{
#' trainer <- BatchBirdFlowTrainer("amewoo", res = 150, truncate_season = TRUE)
#' }
BatchBirdFlowTrainer <- function(species, ...){
  if(!length(species) == 1){
    stop("batch_flow() only works with one species. ", 
         "Use multiple_species_batch()")
  }
  
  trainer <- new_BatchBirdFlowTrainer(species, ...)
  validate_BatchBirdFlowTrainer(trainer)
  
  return(trainer)
}

#' Initialize a batch trainer (low-level constructor)
#'
#' Builds a [BatchBirdFlowTrainer()] by (1) collecting pipeline parameters,
#' (2) preprocessing species inputs via [BirdFlowR::preprocess_species()],
#' and (3) assembling the trainer object.
#'
#' @param species Character scalar, species code or name.
#' @param ... Passed to [set_pipeline_params()] (e.g., `res`, `season`,
#'   `clip`, `crs`, `skip_quality_checks`, etc.).
#'
#' @details Preprocessing is performed in a session temp directory and the
#'   resulting HDF5 is moved into the final `hdf_dir` computed from params.
#'
#' @return A [BatchBirdFlowTrainer()] object with elements:
#'   \itemize{
#'     \item `bf` – result of [BirdFlowR::preprocess_species()]
#'     \item `params` – enriched parameter list produced by
#'       [preprocess_species_wrapper()]
#'   }
#' @seealso [BatchBirdFlowTrainer()], [preprocess_species_wrapper()],
#'   [BirdFlowR::preprocess_species()]
#' @export
#' @examples
#' \dontrun{
#' new_BatchBirdFlowTrainer("amewoo", res = 100)
#' }
new_BatchBirdFlowTrainer <- function(species, ...){
  
  params <- set_pipeline_params(species = species, ...)
  
  # preprocess species and set up directories
  params <- preprocess_species_wrapper(params)
  
  pp_dir <- tempdir()
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
  bf$metadata <- params$metadata
  
  # Make the object
  obj <- list(
    bf = bf,
    params = params
  )
  
  class(obj) <- c('BatchBirdFlowTrainer', class(obj))
  return(obj)
}

#' Fit a BatchBirdFlowTrainer
#'
#' @description
#' Fits one or more BirdFlow models determined by the trainer's parameter grid,
#' submitting jobs via **batchtools** and retrying failed or expired jobs.
#'
#' @param trainer A [BatchBirdFlowTrainer()].
#' @param auto_calculate_gpu_ram Logical; if `TRUE` (default), sets
#'   `gpu_ram := max(gpu_ram(trainer$bf), 8)` before fitting.
#' @param force_refit Logical; if `TRUE`, refit models even if identical HDF5s
#'   already exist.
#' @param ... Additional arguments forwarded to the model fitting pipeline.
#'
#' @return The input `trainer` (invisible). Side effects: submits cluster jobs,
#'   writes HDF5 model files, and prunes extra preexisting fits when appropriate.
#' @seealso [birdflow_modelfit_args_df()], [batch_modelfit_wrapper()],
#'   [birdflow_modelfit()]
#' @importFrom generics fit
#' @method fit BatchBirdFlowTrainer
#' @export
#' @examples
#' \dontrun{
#' trainer <- BatchBirdFlowTrainer("amewoo", res = 150)
#' fit(trainer, force_refit = FALSE)
#' }
fit.BatchBirdFlowTrainer <- function(trainer, auto_calculate_gpu_ram = TRUE, force_refit=FALSE, ...) {
  
  fitting_params <- c(trainer$params, list(force_refit=force_refit), list(...))
  
  if (auto_calculate_gpu_ram) {
    fitting_params$gpu_ram <- max(gpu_ram(trainer$bf), 8)
    print(paste0('Auto-calculating gpu_ram based on no. parameters: ', fitting_params$gpu_ram, ' GB'))
  } else {
    print(paste0('Not auto-calculating gpu_ram, falling back to default setting: ', fitting_params$gpu_ram, ' GB'))
  }
  
  batch_modelfit_wrapper(fitting_params)
  print(paste0('Finished batch training for species: ', trainer$params$species))
  invisible(trainer)
}


####### Detailed functions

#' Make a timestamp string
#'
#' Create a filesystem-friendly timestamp using the package/session timezone.
#'
#' @param tz Timezone string. Defaults to `the$tz`.
#'
#' @return Character timestamp in the form `YYYY-mm-dd_HH-MM-SS`.
#' @keywords internal
#' @examples
#' make_timestamp("UTC")
make_timestamp <- function(tz = the$tz){
  datetime <- Sys.time()
  datetime <- `attr<-`(datetime, "tzone", tz)
  format(datetime, "%Y-%m-%d_%H-%M-%S")
}


#' Preprocess species and enrich batch parameters
#'
#' Wrapper around [BirdFlowR::preprocess_species()] that also derives output
#' paths, writes/moves the preprocessed HDF5, and returns an augmented `params`
#' list.
#'
#' @param params A parameter list, typically from [set_pipeline_params()].
#'
#' @details
#' Side effects:
#' \itemize{
#'   \item Resolves `params$species` with [ebirdst::get_species()].
#'   \item Creates `hdf_dir` and `output_path` if missing.
#'   \item Copies (or renames) the preprocessed HDF5 from `tempdir()` into `hdf_dir`.
#' }
#'
#' @return The modified `params` containing, among others:
#'   `res`, `ebirdst_year`, `output_fullname`, `hdf_dir`, `output_path`,
#'   `geom`, and `metadata`.
#' @seealso [BirdFlowR::preprocess_species()], [BatchBirdFlowTrainer()]
#' @export
#' @examples
#' \dontrun{
#' p <- set_pipeline_params(species = "amewoo", res = 150)
#' p2 <- preprocess_species_wrapper(p)
#' }
preprocess_species_wrapper <- function(params) {
  
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
          trim_quantile = params$trim_quantile,
          min_season_quality = params$min_season_quality
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
  params$geom <- bf$geom
  params$metadata <- bf$metadata
  
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

#' Find a dist_weight and ent_weight consistent with a given obs_prop and de_ratio
#'
#' @param de_ratio numeric Desired ratio, dist_weight/ent_weight
#' @param obs_prop numeric Desired observation_proportion when obs_weight = 1,
#'   e.g. obs_prop = 1 / (1 + dist_weight + ent_weight)
#' @param digits integer Desired number of significant digits to retain in result
#'
#' @returns A list:
#'  * `dist_weight` dist_weight parameter that satisfies desired de_ratio and obs_prop
#'  * `ent_weight` ent_weight parameter that satisfies desired de_ratio and obs_prop
#' @export
refactor_hyperparams <- function(de_ratio, obs_prop, digits = 5){
  x = (de_ratio - de_ratio * obs_prop) / (de_ratio * obs_prop + obs_prop)
  y = (1 - obs_prop) / (de_ratio * obs_prop + obs_prop)
  list(dist_weight = signif(x, digits),
       ent_weight = signif(y, digits)
  )
}

#' Function to fit BirdFlow model by executing `update_hdf.py` in Python. 
#'   Arguments typically passed from [batch_modelfit_wrapper()] via 
#'   [birdflow_modelfit_args_df()]
#' @param py_script Path of Python script to fit a model (`update_hdf.py`)
#' @param dir Argument for `update_hdf.py` - the directory for output
#' @param species Argument for `update_hdf.py` - the species code to fit
#' @param res Argument for `update_hdf.py` - the model resolution
#' @param dist_weight Argument for `update_hdf.py` - the distance weight
#' @param ent_weight Argument for `update_hdf.py` - the entropy weight 
#' @param dist_pow Argument for `update_hdf.py` - the distance power 
#' @param obs_weight Argument for `update_hdf.py` - the observation weight
#' @param learning_rate Argument for `update_hdf.py`
#' @param training_steps Argument for `update_hdf.py`.
#' @param rng_seed Argument for `update_hdf.py`.
#' @param ebirdst_year Argument for `update_hdf.py`.
#' @seealso [birdflow_modelfit_args_df()], [batch_modelfit_wrapper()]
#' @export
birdflow_modelfit <- function(
    py_script = file.path(the$python_repo_path, 'update_hdf.py'),
    dir,
    species,
    res,
    dist_weight,
    ent_weight,
    dist_pow,
    obs_weight = 1,
    learning_rate = 0.1,
    training_steps = 600,
    rng_seed = 17,
    ebirdst_year
){
  
  python_exit_code <- system2('python',
                              args = c(
                                py_script,
                                dir,  # root
                                species, # species
                                res,  # resolution
                                paste0('--dist_weight=', dist_weight),
                                paste0('--ent_weight=', ent_weight),
                                paste0('--dist_pow=', dist_pow),
                                paste0('--obs_weight=', obs_weight),
                                paste0('--learning_rate=', learning_rate),
                                paste0('--training_steps=', training_steps),
                                paste0('--rng_seed=', rng_seed),
                                paste0('--ebirdst_year=', ebirdst_year)
                              ))
  stopifnot(python_exit_code == 0)
}

#' Read key parameters from hdf5 modelfit
#'
#' @param hdf5_path Path of hdf5 model to check
#'
#' @returns a one-row data.frame of hdf5 info
#' @export
identify_hdf5_model <- function(hdf5_path){
  hypers <- rhdf5::h5read(hdf5_path, '/metadata/hyperparameters')[c('dist_pow', 'dist_weight', 'ent_weight')]
  data.frame(dist_pow = hypers$dist_pow,
             dist_weight = hypers$dist_weight,
             ent_weight = hypers$ent_weight,
             myres = rhdf5::h5read(hdf5_path, '/geom/res')[1]/1000,
             mysp = rhdf5::h5read(hdf5_path, '/species')$species_code
  )
}

#' Create a grid-expanded data.frame of model fit arguments. Output designed to be passed to [batch_modelfit_wrapper()]
#'
#' @param params Parameters object, typically created during preprocessing
#'
#' @seealso [birdflow_modelfit()], [batch_modelfit_wrapper()]
#'
#' @export
birdflow_modelfit_args_df <- function(params){
  grid_search_type <- params$grid_search_type
  grid_search_list <- params$grid_search_list
  hdf_dir <- params$hdf_dir
  species <- params$species
  res <- params$res
  ebirdst_year <- params$ebirdst_year
  stopifnot(!is.null(grid_search_type) && grid_search_type %in% c('old', 'new'))
  # base df without grid search parameters
  orig <- data.frame(
    dir = hdf_dir,
    species = species,
    res = res,
    ebirdst_year = ebirdst_year
  )
  orig$id <- seq_len(nrow(orig))
  grid_search_list$id <- orig$id
  df <- expand.grid(grid_search_list)
  
  # if the grid search type is new, calculate dist_weight and ent_weight
  if (grid_search_type == "new"){
    xy <- refactor_hyperparams(df$de_ratio, df$obs_prop)
    df$dist_weight <- xy$dist_weight
    df$ent_weight <- xy$ent_weight
    
    df$de_ratio <- NULL
    df$obs_prop <- NULL
  }
  
  args <- dplyr::left_join(orig, df, by = "id")
  args$id <- NULL
  
  # Check if force refit
  if (params$force_refit == TRUE) {
    return(args)
  }
  
  # Check for existing identical fitted model hdf5s using metadata, and if so, remove from list to fit
  hdf_path_vec <- list.files(params$hdf_dir, pattern = 'km_', full.names = TRUE)
  if (length(hdf_path_vec) > 0){
    message(
      paste('Found', length(hdf_path_vec), 'previously fitted models')
    )
    hdf_df <- sapply(hdf_path_vec, identify_hdf5_model, USE.NAMES = TRUE, simplify = FALSE) %>%
      (data.table::rbindlist)(idcol = 'hdf5_path') %>% dplyr::as_tibble()
    # Deal with floating point issues before anti_join, which only uses `==`
    args <- args %>% dplyr::mutate_if(is.numeric, ~ signif(., 10))
    hdf_df <- hdf_df %>% dplyr::mutate_if(is.numeric, ~ signif(., 10))
    # Delete unneeded hdf5s
    extra_hdf5s <- dplyr::anti_join(hdf_df, args, by = dplyr::join_by('mysp' == 'species', 'myres' == 'res', 'dist_pow' == 'dist_pow', 'dist_weight' == 'dist_weight', 'ent_weight' == 'ent_weight')) %>%
      dplyr::pull(.data$hdf5_path)
    if (length(extra_hdf5s) > 0){
      invisible(file.remove(extra_hdf5s))
      message(
        paste('Deleted', length(extra_hdf5s), 'previously fitted models not needed for this grid search')
      )
    }
    dplyr::anti_join(args, hdf_df, by = dplyr::join_by('species' == 'mysp', 'res' == 'myres', 'dist_pow' == 'dist_pow', 'dist_weight' == 'dist_weight', 'ent_weight' == 'ent_weight'))
  } else {
    args
  }
}


#' Function to batch fit models on the cluster, including resubmitting any failed jobs
#' @param params A list of parameters, as generated by [preprocess_species_wrapper()]
#' @returns This function is run for its side-effects, namely calling [birdflow_modelfit_args_df()] and [birdflow_modelfit()].
#' @seealso [birdflow_modelfit_args_df()] and [birdflow_modelfit()]
#' @export
batch_modelfit_wrapper <- function(params){
  modelfit_resources <- list(walltime = 60,
                             ngpus = 1,
                             memory = params$gpu_ram + 1,
                             measure.memory = TRUE)
  modelfit_args_df <- birdflow_modelfit_args_df(params)
  print(paste0('Using py script: ', the$python_repo_path))
  
  if (nrow(modelfit_args_df) > 0){
    success <- FALSE
    batchtools::batchMap(
      fun = birdflow_modelfit,
      args = modelfit_args_df,
      reg = batchtools::makeRegistry(
        file.path(params$output_path, paste0(make_timestamp(), '_mf')),
        conf.file = system.file('batchtools.conf.R', 
                                package = 'BirdFlowPipeline')
      )
    )
    
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                           resources = modelfit_resources)
    success <- batchtools::waitForJobs()
    if (! isTRUE(success)) {
      message('Requeuing jobs that expired or had an error, attempt 1 of 2')
      batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                             resources = modelfit_resources)
      success <- batchtools::waitForJobs()
    }
    if (! isTRUE(success)) {
      message('Requeuing jobs that expired or had an error, attempt 2 of 2')
      batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                             resources = modelfit_resources)
      success <- batchtools::waitForJobs()
    }
    stopifnot(isTRUE(success))
    invisible(success)
  } else {
    message('Using previously fitted models')
    invisible(TRUE)
  }
}




