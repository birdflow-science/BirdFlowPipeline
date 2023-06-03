# make timestamp
make_timestamp <- function(tz = the$tz){
  datetime <- Sys.time()
  datetime <- `attr<-`(datetime, "tzone", tz)
  format(datetime, "%Y-%m-%d_%H-%M-%S")
}

# preprocess species wrapper
#' @export
preprocess_species_wrapper <- function(params) {
  params$my_species <- ebirdst::get_species(params$my_species)
  pp_dir <- tempdir()
  suppressMessages(
    invisible(
      capture.output(
        bf <- BirdFlowR::preprocess_species(
          species = params$my_species,
          out_dir = pp_dir,
          gpu_ram = params$gpu_ram,
          res = params$my_res)
      )
    )
  )
  # return res
  params$my_res <- BirdFlowR::res(bf)[1] / 1000
  # set up directories
  params$output_fullname <- paste0(params$my_species, '_', params$my_res, 'km', '_', params$output_nickname)
  params$hdf_dir <- file.path(the$batch_hdf_path, params$output_fullname)
  dir.create(params$hdf_dir, showWarnings = FALSE)
  dir.create(the$banding_output_path, showWarnings = FALSE)
  params$output_path <- file.path(the$banding_output_path, params$output_fullname)
  dir.create(params$output_path, showWarnings = FALSE)
  # move preprocessed file to modelfit directory
  preprocessed_file <- list.files(path = pp_dir,
                                  pattern = paste0('^', params$my_species, '.*', params$my_res, 'km.*\\.hdf5$'),
                                  full.names = TRUE)
  invisible(file.copy(preprocessed_file, params$hdf_dir))
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

# fit model container function
# grid search parameters from paper not included as default arguments here:
#  dist_weight = 0.005 # a
#  ent_weight = seq(from = 0, to = 0.006, by = 0.001) # B
#  dist_pow <- seq(from = 0.1, to = 1.0, by = 0.1) # E
#' @export
birdflow_modelfit <- function(
    mypy = file.path(the$python_repo_path, 'update_hdf.py'),
    mydir,
    mysp,
    myres,
    dist_weight,
    ent_weight,
    dist_pow,
    obs_weight = 1,
    learning_rate = 0.1,
    training_steps = 600,
    rng_seed = 17
){
  python_exit_code <- system2('python',
          args = c(
            mypy,
            mydir,
            mysp,
            myres,
            paste0('--dist_weight=', dist_weight),
            paste0('--ent_weight=', ent_weight),
            paste0('--dist_pow=', dist_pow),
            paste0('--obs_weight=', obs_weight),
            paste0('--learning_rate=', learning_rate),
            paste0('--training_steps=', training_steps),
            paste0('--rng_seed=', rng_seed)
          ))
  stopifnot(python_exit_code == 0)
}

# function to make modelfit arguments df from old and new style grid_search_list
# OLD EXAMPLE:
# grid_search_list <- list(
#   dist_weight = seq(from = 0.0008, to = 0.0018, length.out = 5),
#   ent_weight = seq(from = 0.00015, to = 0.0004, length.out = 5),
#   dist_pow = seq(from = 0.1, to = .9, length.out = 5)
# )
# NEW EXAMPLE:
# grid_search_list <- list(
#   de_ratio = c(2, 4, 8, 16),
#   obs_prop = c(0.95, 0.975, 0.99, 0.999, 0.9999),
#   dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
#   dist_weight = NA_real_,
#   ent_weight = NA_real_
# )
# create grid-expanded df for old and new grid search types
#' @export
birdflow_modelfit_args_df <- function(
    grid_search_type = NULL,
    grid_search_list = NULL,
    hdf_dir = NULL,
    my_species = NULL,
    my_res = NULL){
  stopifnot(!is.null(grid_search_type) && grid_search_type %in% c('old', 'new'))
  # base df without grid search parameters
  orig <- data.frame(
    mydir = hdf_dir,
    mysp = my_species,
    myres = my_res
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
  args
}

#' @export
load_batch_params <- function(output_path = NULL){
  params <<- readRDS(file.path(output_path, 'params.rds'))
  ll_df <<- readRDS(file.path(output_path, 'll_df.rds'))
}

# Function to batch fit models from params, including multiple cluster attempts
#' @export
batch_modelfit_wrapper <- function(params){
  modelfit_resources <- list(walltime = 15,
                             ngpus = 1,
                             memory = params$gpu_ram + 1)
  success <- FALSE
  batchtools::batchMap(fun = birdflow_modelfit,
                       args = birdflow_modelfit_args_df(
                         grid_search_type = params$grid_search_type,
                         grid_search_list = params$grid_search_list,
                         hdf_dir = params$hdf_dir,
                         my_species = params$my_species,
                         my_res = params$my_res),
                       reg = batchtools::makeRegistry(file.path(params$output_path, paste0(make_timestamp(), '_mf')), conf.file = system.file('batchtools.conf.R', package = 'banding')))
  batchtools::submitJobs(mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = modelfit_resources)
  success <- batchtools::waitForJobs()
  if (! isTRUE(success)) {
    message('Requeuing jobs that expired or had an error, attempt 1 of 2')
    batchtools::submitJobs(mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = modelfit_resources)
    success <- batchtools::waitForJobs()
  }
  if (! isTRUE(success)) {
    message('Requeuing jobs that expired or had an error, attempt 2 of 2')
    batchtools::submitJobs(mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = modelfit_resources)
    success <- batchtools::waitForJobs()
  }
  stopifnot(isTRUE(success))
  return(success)
}

# Wrapper to batch evaluate models from params and track_info
#' @export
batch_evaluate_models <- function(params, track_info){
  files <- list.files(path = params$hdf_dir,
                      pattern = paste0('^', params$my_species, '.*', params$my_res, 'km_.*\\.hdf5$'),
                      full.names = TRUE)
  evaluation_resources <- list(walltime = 15, memory = 8)
  success <- FALSE
  batchtools::batchMap(evaluate_model,
                       files,
                       more.args = list(track_info = track_info),
                       reg = batchtools::makeRegistry(file.path(params$output_path, paste0(make_timestamp(), '_ll')),
                                                      conf.file = system.file('batchtools.conf.R', package = 'banding')))
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
  ll_df <- batchtools::reduceResultsList() %>%
    lapply(function(i){i$df}) %>%
    (data.table::rbindlist) %>%
    (dplyr::as_tibble) %>%
    (dplyr::arrange)(-.data$ll)
  # replace ll and nll with 0 if all NAs
  if (all(is.na(ll_df$ll))) {ll_df$ll <- 0}
  if (all(is.na(ll_df$nll))) {ll_df$nll <- 0}
  ll_df
}
