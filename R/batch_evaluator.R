#' evaluator
#' @export
batch_evaluator <- function(trainer){
  validate_batch_trainer(trainer)
  evaluator <- new_batch_evaluator(trainer)
  
  validate_batch_evaluator(evaluator)
  return(evaluator)
}

new_batch_evaluator <- function(trainer) {
  obj <- list(
    batch_trainer = trainer
  )
  class(obj) <- c('BatchBirdFlowEvaluator', class(obj))
  return(obj)
}


#' @method evaluate BatchBirdFlowEvaluator
#' @param data Data can be split_data$training_data or split_data$test_data
#' @export
evaluate.BatchBirdFlowEvaluator <- function(evaluator, data, evaluation_function=evaluate_model) {
  validate_batch_evaluator(evaluator)
  
  files <- list.files(path = evaluator$batch_trainer$params$hdf_dir,
                      pattern = paste0('^', evaluator$batch_trainer$params$species, '.*', evaluator$batch_trainer$params$res, 'km_.*\\.hdf5$'),
                      full.names = TRUE)
  evaluation_resources <- list(walltime = 1000, memory = 10)
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
    message('Requeuing jobs that expired or had an error, attempt 1 of 2')
    batchtools::submitJobs(dplyr::mutate(batchtools::findNotDone(), chunk = 1L),
                           resources = evaluation_resources)
    success <- batchtools::waitForJobs()
  }
  if (! isTRUE(success)) {
    print(batchtools::getJobTable())
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



#' Evaluate a BirdFlow model
#' This function could be user-customized
#' @export
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


#' @param object The object to evaluate
#' @export
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}




