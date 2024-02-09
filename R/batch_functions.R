# make timestamp
make_timestamp <- function(tz = the$tz){
  datetime <- Sys.time()
  datetime <- `attr<-`(datetime, "tzone", tz)
  format(datetime, "%Y-%m-%d_%H-%M-%S")
}

#' Wrapper for [BirdFlowR::preprocess_species()] that also prepares batch parameters
#' @param params A list of starting parameters, currently set up in [batch_flow()]
#' @returns The starting params list, modified to include additional information calculated during preprocessing. Also has side effects:
#'  * create directories as needed
#'  * write preprocessed hdf5 file
#' @seealso [batch_flow()]
#'
#' @export
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
          skip_quality_checks = params$skip_quality_checks)
      )
    )
  )
  # return res
  params$res <- BirdFlowR::res(bf)[1] / 1000
  params$ebirdst_year <- bf$metadata$ebird_version_year
  # set up directories
  params$output_fullname <- paste0(params$species, '_', params$res, 'km', '_', params$suffix)
  params$hdf_dir <- file.path(params$hdf_path, paste0(params$species, '_', params$res, 'km'))
  dir.create(params$hdf_dir, showWarnings = FALSE)
  dir.create(params$output_path, showWarnings = FALSE)
  params$output_path <- file.path(params$output_path, params$output_fullname)
  dir.create(params$output_path, showWarnings = FALSE)
  # move preprocessed file to modelfit directory
  preprocessed_file <- list.files(path = pp_dir,
                                  pattern = paste0('^', params$species, '.*', params$res, 'km.*\\.hdf5$'),
                                  full.names = TRUE)
  invisible(file.copy(preprocessed_file, params$hdf_dir, overwrite = TRUE))
  if (file.exists(preprocessed_file)) invisible(file.remove(preprocessed_file))
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
  c(dist_weight = signif(x, digits),
    ent_weight = signif(y, digits)
  )
}

#' Function to fit BirdFlow model by executing `update_hdf.py` in Python. 
#'   Arguments typically passed from [batch_modelfit_wrapper()] via [birdflow_modelfit_args_df()]
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
    species,
    dir,
    py_script = file.path(the$python_repo_path, 'update_hdf.py'),
    res,
    dist_weight,
    ent_weight,
    dist_pow,
    obs_weight = 1,
    learning_rate = 0.1,
    training_steps = 600,
    rng_seed = 17,
    ebirdst_year,
    mypy = lifecycle::deprecated(),
    mydir = lifecycle::deprecated(),
    mysp  = lifecycle::deprecated(),
    myres = lifecycle::deprecated()
    
){
  
  
  # Handle deprecated arguments
  if(lifecycle::is_present("mypy")){
    lifecycle::deprecate_warn("0.0.0.9002", 
                              what = "birdflow_model_fit(mypy)", 
                              with = "birdflow_model_fit(py_script)")
    py_pcript <- mypy
  }
  if(lifecycle::is_present("mydir")){
    lifecycle::deprecate_warn("0.0.0.9002", 
                            what = "birdflow_model_fit(mydir)", 
                            with = "birdflow_model_fit(preprocess_dir)")
    preprocess_dir <- mydir
  }
  
  if(lifecycle::is_present("mysp")){
    lifecycle::deprecate_warn("0.0.0.9002", 
                              what = "birdflow_model_fit(mysp)", 
                              with = "birdflow_model_fit(species)")
    species <- mysp
  }
  
  
  if(lifecycle::is_present("myres")){
    lifecycle::deprecate_warn("0.0.0.9002", 
                              what = "birdflow_model_fit(myres)", 
                              with = "birdflow_model_fit(res)")
    res <- myres
  }
  
  
  python_exit_code <- system2('python',
          args = c(
            py_script,
            dir,
            species,
            res,
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
    mydir = hdf_dir,
    species = species,
    res = res,
    ebirdst_year = ebirdst_year
  )
  orig$id <- seq_len(nrow(orig))
  grid_search_list$id <- orig$id
  df <- expand.grid(grid_search_list)

  # if the grid search type is new, calculate dist_weight and ent_weight
  if (grid_search_type == "new"){
    for (i in seq_len(nrow(df))){
      xy <- refactor_hyperparams(df$de_ratio[i], df$obs_prop[i])
      df$dist_weight[i] <- xy["dist_weight"]
      df$ent_weight[i] <- xy["ent_weight"]
    }
    df$de_ratio <- NULL
    df$obs_prop <- NULL
  }

  args <- dplyr::left_join(orig, df, by = "id")
  args$id <- NULL
  
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
    extra_hdf5s <- dplyr::anti_join(hdf_df, args, by = dplyr::join_by('mysp' == 'mysp', 'myres' == 'myres', 'dist_pow' == 'dist_pow', 'dist_weight' == 'dist_weight', 'ent_weight' == 'ent_weight')) %>%
      dplyr::pull(.data$hdf5_path)
    if (length(extra_hdf5s) > 0){
      invisible(file.remove(extra_hdf5s))
      message(
        paste('Deleted', length(extra_hdf5s), 'previously fitted models not needed for this grid search')
      )
    }
    dplyr::anti_join(args, hdf_df, by = dplyr::join_by('mysp' == 'mysp', 'myres' == 'myres', 'dist_pow' == 'dist_pow', 'dist_weight' == 'dist_weight', 'ent_weight' == 'ent_weight'))
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
  modelfit_resources <- list(walltime = 15,
                             ngpus = 1,
                             memory = params$gpu_ram + 1)
  modelfit_args_df <- birdflow_modelfit_args_df(params)
  if (nrow(modelfit_args_df) > 0){
    success <- FALSE
    batchtools::batchMap(fun = birdflow_modelfit,
                         args = modelfit_args_df,
                         reg = batchtools::makeRegistry(file.path(params$output_path, paste0(make_timestamp(), '_mf')), conf.file = system.file('batchtools.conf.R', package = 'BirdFlowPipeline')))
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

#' Batch evaluate models on the cluster
#' @param params A list of parameters, as returned by [preprocess_species_wrapper()].
#' @param track_info A list of track info, as returned by [make_tracks()], and passed internally to [BirdFlowR::interval_log_likelihood()].
#' @returns A data.frame with a row for each model evaluated
#' @seealso [evaluate_model()], [preprocess_species_wrapper()], [make_tracks()], [BirdFlowR::interval_log_likelihood()]
#' @export
batch_evaluate_models <- function(params, track_info){
  files <- list.files(path = params$hdf_dir,
                      pattern = paste0('^', params$species, '.*', params$res, 'km_.*\\.hdf5$'),
                      full.names = TRUE)
  evaluation_resources <- list(walltime = 25, memory = 8)
  success <- FALSE
  batchtools::batchMap(import_birdflow_and_evaluate,
                       files,
                       more.args = list(track_info = track_info, params = params),
                       reg = batchtools::makeRegistry(file.path(params$output_path, paste0(make_timestamp(), '_ll')),
                                                      conf.file = system.file('batchtools.conf.R', package = 'BirdFlowPipeline')))
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = evaluation_resources)
  success <- batchtools::waitForJobs()
  if (! isTRUE(success)) {
    message('Requeuing jobs that expired or had an error, attempt 1 of 2')
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = evaluation_resources)
    success <- batchtools::waitForJobs()
  }
  if (! isTRUE(success)) {
    message('Requeuing jobs that expired or had an error, attempt 2 of 2')
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = evaluation_resources)
    success <- batchtools::waitForJobs()
  }
  stopifnot(isTRUE(success))
  eval_metrics <- batchtools::reduceResultsList() %>%
    lapply(function(i){i$df}) %>%
    (data.table::rbindlist) %>%
    (dplyr::as_tibble) %>%
    (dplyr::arrange)(-.data$ll)
  # replace ll and nll with 0 if all NAs
  if (all(is.na(eval_metrics$ll))) {eval_metrics$ll <- 0}
  if (all(is.na(eval_metrics$nll))) {eval_metrics$nll <- 0}
  eval_metrics
}
