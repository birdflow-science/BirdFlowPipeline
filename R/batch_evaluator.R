#' Construct a batch evaluator
#'
#' Validates a [BatchBirdFlowTrainer()] and returns a corresponding
#' [BatchBirdFlowEvaluator()] ready to evaluate all fitted HDF5 models for that species.
#'
#' @param trainer A [BatchBirdFlowTrainer()].
#'
#' @return A [BatchBirdFlowEvaluator()] object (invisible).
#' @seealso [new_BatchBirdFlowEvaluator()], [evaluate.BatchBirdFlowEvaluator()],
#'   [BatchBirdFlowTrainer()], [fit.BatchBirdFlowTrainer()]
#' @export
#' @examples
#' \dontrun{
#' trainer <- BatchBirdFlowTrainer("amewoo", res = 150)
#' trainer <- fit(trainer)
#' ev <- BatchBirdFlowEvaluator(trainer)
#' }
BatchBirdFlowEvaluator <- function(trainer){
  validate_BatchBirdFlowTrainer(trainer)
  evaluator <- new_BatchBirdFlowEvaluator(trainer)
  
  validate_BatchBirdFlowEvaluator(evaluator)
  return(evaluator)
}


#' Initialize a batch evaluator (low-level constructor)
#'
#' Creates a [BatchBirdFlowEvaluator()] that carries a reference to the
#' underlying [BatchBirdFlowTrainer()].
#'
#' @param trainer A [BatchBirdFlowTrainer()].
#'
#' @return A [BatchBirdFlowEvaluator()] with field `batch_trainer`.
#' @seealso [BatchBirdFlowEvaluator()], [evaluate.BatchBirdFlowEvaluator()]
#' @export
#' @examples
#' \dontrun{
#' ev <- new_BatchBirdFlowEvaluator(BatchBirdFlowTrainer("amewoo"))
#' }
new_BatchBirdFlowEvaluator <- function(trainer) {
  obj <- list(
    batch_trainer = trainer
  )
  class(obj) <- c('BatchBirdFlowEvaluator', class(obj))
  return(obj)
}



#' Evaluate a set of BirdFlow HDF5 models (S3 method)
#'
#' Submits evaluation jobs (one per HDF5) via **batchtools** using the provided
#' `evaluation_function` (defaults to [evaluate_model()]). Retries failed/expired
#' jobs up to two times. Returns a list of results gathered with
#' [batchtools::reduceResultsList()].
#'
#' @param object A [BatchBirdFlowEvaluator()].
#' @param data A list or object containing evaluation data. Typically
#'   `split_data$training_data` or `split_data$test_data`, where index `[[1]]`
#'   is interval data and `[[2]]` is 1-week data used for PIT calibration.
#' @param evaluation_function Function of signature
#'   `function(bf_path, data, params)` returning a result per model (defaults to
#'   [evaluate_model()]).
#' @param test_one_evaluate Logical; if `TRUE`, just do some test on this local 
#' session instead of submitting it to slurm
#' @details
#' HDF5 models are discovered in `evaluator$batch_trainer$params$hdf_dir`
#' matching the current species and resolution. A **batchtools** registry is
#' created under `output_path` with a timestamped suffix.
#'
#' @return A list as returned by [batchtools::reduceResultsList()] (often a list
#'   of per-model lists/data frames produced by `evaluation_function`).
#'   Side effects: writes PIT objects to disk (if done by `evaluation_function`),
#'   prints job status.
#'
#' @seealso [evaluate_model()], [BatchBirdFlowEvaluator()], [batchtools::reduceResultsList()]
#' @method evaluate BatchBirdFlowEvaluator
#' @export
#' @examples
#' \dontrun{
#' ev <- BatchBirdFlowEvaluator(trainer)
#' res <- evaluate(ev, split_data$test_data)
#' }
evaluate.BatchBirdFlowEvaluator <- function(object, 
                                            data, 
                                            evaluation_function=evaluate_model, 
                                            test_one_evaluate=FALSE,
                                            ...) {
  evaluator <- object
  validate_BatchBirdFlowEvaluator(evaluator)
  
  files <- list.files(path = evaluator$batch_trainer$params$hdf_dir,
                      pattern = paste0('^', evaluator$batch_trainer$params$species, '.*', evaluator$batch_trainer$params$res, 'km_.*\\.hdf5$'),
                      full.names = TRUE)
  
  if (test_one_evaluate) {
    the_file <- sample(files, 1)
    evaluation_res <- evaluation_function(the_file, data=data, params = evaluator$batch_trainer$params)
    return(evaluation_res)
  }
  
  evaluation_resources <- list(walltime = 1000, memory = 10, measure.memory = TRUE)
  success <- FALSE
  batchtools::batchMap(evaluation_function,
                       files,
                       more.args = list(data=data, params = evaluator$batch_trainer$params),
                       reg = batchtools::makeRegistry(file.path(evaluator$batch_trainer$params$output_path, paste0(make_timestamp(), '_ll')),
                                                      conf.file = system.file('batchtools.conf.R', package = 'BirdFlowPipeline')))
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = evaluation_resources)
  success <- batchtools::waitForJobs()
  print(batchtools::getStatus())
  
  
  if (! isTRUE(success)) {
    
    job_status_df <- batchtools::getJobStatus()
    print('Tasks with issues: ')
    print(job_status_df[!is.na(job_status_df$error), , drop = FALSE])
    
    message('Requeuing jobs that expired or had an error, attempt 1 of 2')
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = evaluation_resources)
    success <- batchtools::waitForJobs()
  }
  if (! isTRUE(success)) {
    
    job_status_df <- batchtools::getJobStatus()
    print('Tasks with issues: ')
    print(job_status_df[!is.na(job_status_df$error), , drop = FALSE])
    
    message('Requeuing jobs that expired or had an error, attempt 2 of 2')
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = evaluation_resources)
    success <- batchtools::waitForJobs()
  }
  
  # stopifnot(isTRUE(success))
  eval_metrics <- batchtools::reduceResultsList()
  
  
  # %>%
  #   lapply(function(i){i$df}) %>%
  #   (data.table::rbindlist) %>%
  #   (dplyr::as_tibble) %>%
  #   (dplyr::arrange)(-.data$mean_ll)
  # # replace ll and nll with 0 if all NAs
  # if (all(is.na(eval_metrics$mean_ll))) {eval_metrics$mean_ll <- 0}
  # if (all(is.na(eval_metrics$mean_null_ll))) {eval_metrics$mean_null_ll <- 0}
  # 
  
  return(eval_metrics)
}



#' Evaluate a single BirdFlow model (default evaluation function)
#'
#' Loads a fitted BirdFlow HDF5, computes interval-based metrics, optional
#' route statistics (both conditional on observed tracks and unconditional
#' synthetic routes by season), and PIT calibration diagnostics. Also extracts
#' key hyperparameters and distribution-level correlations.
#'
#' @param bf_path Path to a fitted BirdFlow HDF5 file.
#' @param data A list where `[[1]]` holds interval data and `[[2]]` holds
#'   one-week interval data used for PIT calibration.
#' @param params Parameter list (usually `evaluator$batch_trainer$params`)
#'   providing `output_path`, `season`, and other configuration used by helpers.
#'
#' @details
#' Side effects:
#' \itemize{
#'   \item Saves a PIT calibration RDS to `<output_path>/pit_data/`.
#'   \item Prints a compact tibble of derived metrics.
#' }
#'
#' @return A list with elements:
#' \describe{
#'   \item{df}{A one-row tibble of summary metrics and hyperparameters.}
#'   \item{obs}{The interval data object passed in `data[[1]]`.}
#'   \item{metric_for_each_transition}{Per-transition metrics table.}
#' }
#'
#' @seealso [evaluate.BatchBirdFlowEvaluator()], [BirdFlowR::calc_interval_metrics()],
#'   [BirdFlowR::distribution_performance()], [BirdFlowR::route()]
#' @export
#' @examples
#' \dontrun{
#' one <- split_data$training_data
#' two <- split_data$training_data_one_week
#' out <- evaluate_model(".../amewoo_150km_xyz.hdf5", data = list(one, two), params = trainer$params)
#' out$df
#' }
evaluate_model <- function(bf_path, data, params){
  
  # Load bf
  bf <- BirdFlowR::import_birdflow(bf_path)
  modelname <- basename(bf_path)
  
  # Dispatch the needed data from `data`
  birdflow_intervals <- data[[1]]  #$train_data
  birdflow_intervals_one_week <- data[[2]]  #$train_data_one_week
  
  # Start evaluation
  result <- BirdFlowR::calc_interval_metrics(birdflow_intervals, bf = bf)
  interval_based_metrics <- result[[1]]
  metric_for_each_transition <- result[[2]]
  metric_for_each_transition$route_type <- birdflow_intervals$data$route_type
  motus_fraction <- mean(c(metric_for_each_transition$route_type)=='motus')
  banding_fraction <- mean(c(metric_for_each_transition$route_type)=='banding')
  tracking_fraction <- mean(c(metric_for_each_transition$route_type)=='tracking')

  ###### UNDER DEVELOPMENT
  params$season <- 'prebreeding'
  real_track <- get_real_track(bf, params, filter=TRUE)
  params$season <- 'postbreeding'
  real_track2 <- get_real_track(bf, params, filter=TRUE)
  real_track$data <- rbind(real_track$data, real_track2$data)
  params$season <- 'prebreeding'
  
  if(is.null(real_track)){
    route_stats <- list()
    route_stats$straightness <- NULL
    route_stats$displacement <- NULL
    route_stats$n_stopovers <- NULL
    route_stats$n_stopovers_daves <- NULL
    route_stats$speed <- NULL
  } else {
    splitted_track <- split(real_track$data, real_track$data$route_id)
    all_route_stats <- list()
    for (track in splitted_track){
      the_synth_track <- BirdFlowR::route(bf = bf, n = 10, 
                                          start = track$timestep[1], end=track$timestep[nrow(track)], 
                                          x_coord = track$x[1], y_coord = track$y[1], 
                                          from_marginals = TRUE)
      route_stats <- rts_stats(the_synth_track)
      all_route_stats[[length(all_route_stats) + 1]] <- as.data.frame(route_stats)
    }
    
    conditional_rts_stats_res <- as.data.frame(do.call(rbind, all_route_stats))
    conditional_rts_stats_res <- conditional_rts_stats_res[!is.na(conditional_rts_stats_res$straightness),]
    route_stats <- colMeans(conditional_rts_stats_res)
    route_stats_list <- list()
    for (name in names(route_stats)){
      route_stats_list[[name]] <- route_stats[[name]]
    }
    route_stats <- route_stats_list
  }
  
  print('Doing synth routes stats...')
  synth_routes_prebreeding_migration_route_stats <- rts_stats(BirdFlowR::route(bf = bf, n = 100, season='prebreeding_migration',
                                                                               from_marginals = TRUE), remove_head_and_tail_stationaries = TRUE)
  synth_routes_breeding_route_stats <- rts_stats(BirdFlowR::route(bf = bf, n = 100, season='breeding',
                                                                  from_marginals = TRUE))
  synth_routes_postbreeding_migration_route_stats <- rts_stats(BirdFlowR::route(bf = bf, n = 100, season='postbreeding_migration',
                                                                                from_marginals = TRUE), remove_head_and_tail_stationaries = TRUE)
  synth_routes_nonbreeding_route_stats <- rts_stats(BirdFlowR::route(bf = bf, n = 100, season='nonbreeding',
                                                                     from_marginals = TRUE))
  
  ######
  # Do PIT calculations
  pit_calibration_obj <- pit_calibration(bf, birdflow_intervals_one_week$data, params)
  
  dir.create(file.path(params$output_path, 'pit_data'), showWarnings = FALSE)
  pit_data_filename <- paste0(sub('\\.hdf5$', "", modelname), '_pit.rds')
  outfile <- file.path(file.path(params$output_path, 'pit_data'), pit_data_filename)
  saveRDS(pit_calibration_obj, outfile)
  
  out_df <- dplyr::tibble(
    model = modelname,
    obs_weight = safe_numeric(bf$metadata$hyperparameters$obs_weight),
    ent_weight = safe_numeric(bf$metadata$hyperparameters$ent_weight),
    dist_weight = safe_numeric(bf$metadata$hyperparameters$dist_weight),
    dist_pow = safe_numeric(bf$metadata$hyperparameters$dist_pow),
    de_ratio = safe_numeric(signif(bf$metadata$hyperparameters$dist_weight / bf$metadata$hyperparameters$ent_weight, 3)),
    obs_prop = safe_numeric(signif(1 / (1 + bf$metadata$hyperparameters$dist_weight + bf$metadata$hyperparameters$ent_weight), 4)),
    
    traverse_cor_prebreeding = BirdFlowR::distribution_performance(bf, metrics = 'md_traverse_cor', season = 'prebreeding')$md_traverse_cor,
    traverse_cor_st_prebreeding = BirdFlowR::distribution_performance(bf, metrics = 'st_traverse_cor', season = 'prebreeding')$st_traverse_cor,
    mean_dist_cor_prebreeding = BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor', season = 'prebreeding')$mean_distr_cor,
    min_dist_cor_prebreeding = BirdFlowR::distribution_performance(bf, metrics = 'min_distr_cor', season = 'prebreeding')$min_distr_cor,
    
    traverse_cor_postbreeding = BirdFlowR::distribution_performance(bf, metrics = 'md_traverse_cor', season = 'postbreeding')$md_traverse_cor,
    traverse_cor_st_postbreeding = BirdFlowR::distribution_performance(bf, metrics = 'st_traverse_cor', season = 'postbreeding')$st_traverse_cor,
    mean_dist_cor_postbreeding = BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor', season = 'postbreeding')$mean_distr_cor,
    min_dist_cor_postbreeding = BirdFlowR::distribution_performance(bf, metrics = 'min_distr_cor', season = 'postbreeding')$min_distr_cor,
    
    traverse_cor_whole_year = BirdFlowR::distribution_performance(bf, metrics = 'md_traverse_cor')$md_traverse_cor,
    traverse_cor_st_whole_year = BirdFlowR::distribution_performance(bf, metrics = 'st_traverse_cor')$st_traverse_cor,
    mean_dist_cor_whole_year = BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor')$mean_distr_cor,
    min_dist_cor_whole_year = BirdFlowR::distribution_performance(bf, metrics = 'min_distr_cor')$min_distr_cor,
    
    mean_win_prob = interval_based_metrics[['mean_win_prob']],
    mean_win_distance = interval_based_metrics[['mean_win_distance']],
    mean_win_distance_fraction = interval_based_metrics[['mean_win_distance_fraction']],
    mean_null_ll = interval_based_metrics[['mean_null_ll']],
    mean_ll = interval_based_metrics[['mean_ll']],
    
    mean_energy_improvement = interval_based_metrics[['mean_energy_improvement']],
    
    weighted_mean_win_prob=interval_based_metrics[['weighted_mean_win_prob']],
    weighted_mean_win_distance=interval_based_metrics[['weighted_mean_win_distance']],
    weighted_mean_win_distance_fraction=interval_based_metrics[['weighted_mean_win_distance_fraction']],
    weighted_mean_null_ll=interval_based_metrics[['weighted_mean_null_ll']],
    weighted_mean_ll=interval_based_metrics[['weighted_mean_ll']],
    
    weighted_energy_improvement=interval_based_metrics[['weighted_energy_improvement']],
    
    mean_global_prob_of_the_banding_starting = interval_based_metrics[['mean_global_prob_of_the_starting']],
    mean_elapsed_days = interval_based_metrics[['mean_elapsed_days']],
    mean_elapsed_km = interval_based_metrics[['mean_elapsed_km']],
    
    n_intervals=interval_based_metrics[['n_intervals']],
    
    straightness = route_stats$straightness, # of synthetic routes, but conditional on the tracking data starts and ends
    length = route_stats$length,
    displacement = route_stats$displacement,
    n_stopovers = route_stats$n_stopovers,
    n_stopovers_daves = route_stats$n_stopovers_daves,
    speed = route_stats$speed,
    
    # of synthetic routes, and not conditional on the tracking data at all
    synth_routes_prebreeding_migration_straightness = synth_routes_prebreeding_migration_route_stats$straightness,
    synth_routes_prebreeding_migration_n_stopovers = synth_routes_prebreeding_migration_route_stats$n_stopovers,
    synth_routes_prebreeding_migration_speed = synth_routes_prebreeding_migration_route_stats$speed,
    synth_routes_breeding_straightness = synth_routes_breeding_route_stats$straightness,
    synth_routes_breeding_n_stopovers = synth_routes_breeding_route_stats$n_stopovers,
    synth_routes_breeding_speed = synth_routes_breeding_route_stats$speed,
    synth_routes_postbreeding_migration_straightness = synth_routes_postbreeding_migration_route_stats$straightness,
    synth_routes_postbreeding_migration_n_stopovers = synth_routes_postbreeding_migration_route_stats$n_stopovers,
    synth_routes_postbreeding_migration_speed = synth_routes_postbreeding_migration_route_stats$speed,
    synth_routes_nonbreeding_straightness = synth_routes_nonbreeding_route_stats$straightness,
    synth_routes_nonbreeding_n_stopovers = synth_routes_nonbreeding_route_stats$n_stopovers,
    synth_routes_nonbreeding_speed = synth_routes_nonbreeding_route_stats$speed,
    
    pit_row = pit_calibration_obj[['D_row']], # The lower (close to 0) the better
    pit_row_p = pit_calibration_obj[['PIT_row_p']],
    pit_col = pit_calibration_obj[['D_col']], # The lower (close to 0) the better
    pit_col_p = pit_calibration_obj[['PIT_col_p']],
    pit_in_95 = ifelse(is.null(pit_calibration_obj$res), NA, sum(pit_calibration_obj$res$in_95_set,na.rm=T)/length(pit_calibration_obj$res$in_95_set)),
    
    training_tracking_fraction = tracking_fraction,
    training_banding_fraction = banding_fraction,
    training_motus_fraction = motus_fraction
  ) |>
    dplyr::mutate(
      traverse_cor = (traverse_cor_prebreeding + traverse_cor_postbreeding) / 2,
      traverse_cor_st = (traverse_cor_st_prebreeding + traverse_cor_st_postbreeding) / 2,
      mean_dist_cor = (mean_dist_cor_prebreeding + mean_dist_cor_postbreeding) / 2,
      min_dist_cor = (min_dist_cor_prebreeding + min_dist_cor_postbreeding) / 2
    )
  #my_ll
  print(out_df)
  list(df = out_df, obs = birdflow_intervals, metric_for_each_transition=metric_for_each_transition)
}



#' Generic evaluation S3
#'
#' Dispatches to class-specific `evaluate()` methods (e.g.,
#' [evaluate.BatchBirdFlowEvaluator()]).
#'
#' @param object The object to evaluate.
#' @param ... Passed to methods.
#'
#' @return Method-dependent; see specific methods.
#' @export
#' @examples
#' \dontrun{
#' res <- evaluate(BatchBirdFlowEvaluator(trainer), split_data$test_data)
#' }
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}




