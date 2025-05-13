#' Grid search, model selection, and model evaluation for one species
#'
#' @param species A single eBird 6-letter code or common name describing 
#' a species recognized by [ebirdst::get_species()].
#' @inheritDotParams set_pipeline_params -species
#' @returns function is used for its many side effects, according to 
#' configuration settings in `as.list(BirdFlowPipeline:::the)`
#'  * create destination directories as needed
#'  * write preprocessed hdf5 file
#'  * write modelfit hdf5 files
#'  * write reports, plots, and maps for model evaluation and visualization
#' @seealso [multiple_species_batch()]
#' @export
batch_flow <- function(species, ...){
  
  if(!length(species) == 1){
    stop("batch_flow() only works with one species. ", 
         "Use multiple_species_batch()")
  }
  
  # species = sp = 'norpin'
  # sp_output_path <- paste0('/work/pi_drsheldon_umass_edu/birdflow_modeling/yangkang/model_output_hyperparams_distance_metric','/',sp)
  # params <- set_pipeline_params(species = species, gpu_ram=10, hdf_path = sp_output_path, base_output_path = sp_output_path, skip_quality_checks=TRUE,
  #                               min_season_quality = 1, model_selection = 'distance_metric',
  #                               suffix='interval_based_eval_using_migration_transitions')
  params <- set_pipeline_params(species = species, ...)
  
  # preprocess species and set up directories
  params <- preprocess_species_wrapper(params)
  
  # Save finalized parameters
  saveRDS(params, file.path(params$output_path, 'params.rds'))
  
  # Batch fit models
  batch_modelfit_wrapper(params)
  
  # Exit if fitting only
  if (isTRUE(params$fit_only)){
    return(NULL)
  }
  
  # Get bf object (for converting to BirdFlowIntervals)
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
  
  # Routes to BirdFlowRoutes to BirdFlowIntervals
  # Load and save track info
  # Here should combine banding and motus data and convert to BirdFlowIntervals class
  banding_df <- load_banding_transitions_df(file.path(BirdFlowPipeline:::the$banding_rds_path, paste0(params$species, '.rds')))
  motus_df <- load_motus_transitions_df(file.path(BirdFlowPipeline:::the$motus_rds_path, paste0(params$species, '.rds')))
  track_birdflowroutes_obj <- get_real_track(bf, params, filter=FALSE) # Real track. Not filtered by season. All year round.
  combined_data <- rbind(banding_df, motus_df, track_birdflowroutes_obj$data[,c('route_id','date','lon','lat','route_type')])
  combined_data <- na.omit(combined_data)
  saveRDS(combined_data, file.path(params$output_path, 'all_ground_truth_transitions_df.rds'))

  # Dataframe to Routes
  source <- ''
  if (!is.null(banding_df)){
    if (source==''){
      source <- 'Banding'
    } else {
      source <- paste0(source, ' & ', 'Banding')
    }
  } else if (!is.null(motus_df)){
    if (source==''){
      source <- 'MOTUS'
    } else {
      source <- paste0(source, ' & ', 'MOTUS')
    }
  } else if (!is.null(track_birdflowroutes_obj)){
    if (source==''){
      source <- 'Tracking'
    } else {
      source <- paste0(source, ' & ', 'Tracking')
    }
  }
  
  if (source==''){
    source <- 'No Data'
  }
  
  ## All interval samples
  routes_obj <- BirdFlowR::Routes(combined_data, species=bf$species, source=source)
  if (nrow(routes_obj$data)==0){
    stop("No Transition data available")
  }
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf=bf)
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=180,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  # Filter intervals to ask at least one leg in the migration season
  target_timesteps <- c(BirdFlowR::lookup_season_timesteps(bf, season='prebreeding'), 
    BirdFlowR::lookup_season_timesteps(bf, season='postbreeding'))
  interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
  
  if (is.null(interval_obj)){
    stop("No intervals available")
  }
  
  # One week samples
  routes_one_week_obj <- BirdFlowR::Routes(combined_data, species=bf$species, source=source)
  interval_one_week_obj <- routes_one_week_obj |> BirdFlowR::as_BirdFlowRoutes(bf=bf) |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=13,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  interval_one_week_obj$data <-interval_one_week_obj$data[interval_one_week_obj$data$timestep2 - interval_one_week_obj$data$timestep1 == 1,]
  # Filter intervals to ask at least one leg in the migration season
  interval_one_week_obj$data <- interval_one_week_obj$data[(interval_one_week_obj$data$timestep1 %in% target_timesteps) | (interval_one_week_obj$data$timestep2 %in% target_timesteps),]
  params$transition_type <- 'all_combined'
  
  # Train-test split
  set.seed(42)
  train_data <- interval_obj$data |> dplyr::sample_frac(0.7)
  test_data <- dplyr::setdiff(interval_obj$data, train_data)
  train_data <- BirdFlowR::BirdFlowIntervals(data=train_data,
                                             species=interval_obj$species,
                                             metadata=interval_obj$metadata,
                                             geom=interval_obj$geom,
                                             dates=interval_obj$dates,
                                             source=interval_obj$source)
  test_data <- BirdFlowR::BirdFlowIntervals(data=test_data,
                                            species=interval_obj$species,
                                            metadata=interval_obj$metadata,
                                            geom=interval_obj$geom,
                                            dates=interval_obj$dates,
                                            source=interval_obj$source)
  
  set.seed(42)
  train_data_one_week <- interval_one_week_obj$data |> dplyr::sample_frac(0.7)
  test_data_one_week <- dplyr::setdiff(interval_one_week_obj$data, train_data_one_week)
  train_data_one_week <- BirdFlowR::BirdFlowIntervals(data=train_data_one_week,
                                                      species=interval_one_week_obj$species,
                                                      metadata=interval_one_week_obj$metadata,
                                                      geom=interval_one_week_obj$geom,
                                                      dates=interval_one_week_obj$dates,
                                                      source=interval_one_week_obj$source)
  test_data_one_week <- BirdFlowR::BirdFlowIntervals(data=test_data_one_week,
                                                     species=interval_one_week_obj$species,
                                                     metadata=interval_one_week_obj$metadata,
                                                     geom=interval_one_week_obj$geom,
                                                     dates=interval_one_week_obj$dates,
                                                     source=interval_one_week_obj$source)
  
  saveRDS(train_data, file.path(params$output_path, glue::glue('train_data_all_combined.rds')))
  saveRDS(test_data, file.path(params$output_path, glue::glue('test_data_all_combined.rds')))
  saveRDS(train_data_one_week, file.path(params$output_path, glue::glue('train_data_one_week_all_combined.rds')))
  saveRDS(test_data_one_week, file.path(params$output_path, glue::glue('test_data_one_week_all_combined.rds')))
  
  # Batch model evaluation
  if (!dir.exists(file.path(params$output_path, 'each_transition_evaluation'))){
    dir.create(file.path(params$output_path, 'each_transition_evaluation'))
  }
  
  params$mode <- 'train'
  eval_metrics_train <- batch_evaluate_models(params, train_data, train_data_one_week)
  params$mode <- 'test'
  eval_metrics_test <- batch_evaluate_models(params, test_data, test_data_one_week)
  
  # Model selection and ranking with desirability
  params$mode <- 'train'
  params$model_selection <- 'distance_metric'
  eval_metrics_train1 <- rank_models(eval_metrics_train, params)
  saveRDS(eval_metrics_train1, file.path(params$output_path, glue::glue('eval_metrics_train_distance_metric_all_combined.rds')))
  
  tryCatch({
    params$model_selection <- 'real_tracking'
    eval_metrics_train2 <- rank_models(eval_metrics_train, params)
    saveRDS(eval_metrics_train2, file.path(params$output_path, glue::glue('eval_metrics_train_multi_objective_all_combined.rds')))
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    cat("Tracking data might not be available for this species")
  })
  
  params$mode <- 'test'
  params$model_selection <- 'distance_metric'
  eval_metrics_test <- rank_models(eval_metrics_test, params)
  saveRDS(eval_metrics_test, file.path(params$output_path, glue::glue('eval_metrics_test_distance_metric_all_combined.rds')))
  
  eval_metrics <- eval_metrics_test


  ## Plotting
  # make PCA evaluation plot
  
  # 
  # if (nrow(eval_metrics) > 1){
  #   model_evaluation_biplot(eval_metrics, params)
  #   
  #   # Plot likelihood results cube
  #   make_3d_plot('color_ll', 'll', eval_metrics, params)
  #   # Plot null likelihood cube
  #   make_3d_plot('color_nll', 'nll', eval_metrics, params)
  #   # Plot correlation cube
  #   make_3d_plot('color_cor', 'cor', eval_metrics, params)
  #   
  #   # create model reports for top 5 models
  #   
  #   for (i in 1:5){
  #     bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, eval_metrics$model[i]))
  #     rmarkdown::render(system.file("rmd", "model_report.Rmd", package = "BirdFlowPipeline"), 
  #                       output_file = paste0("model_report", i, ".html"), output_dir = params$output_path)
  #   }
  #   
  #   # plot most desirable models
  #   
  #   for (i in 1:5){
  #     pdf(file.path(params$output_path, paste0('desirability', i, '.pdf')))
  #     quick_visualize_routes(i, df = eval_metrics, dir = params$hdf_dir, season = params$season)
  #     dev.off()
  #   }
  #   
  #   # graph tradeoff
  #   
  #   pdf(file.path(params$output_path, 'straightness_vs_end_traverse_cor.pdf'))
  #   plot(eval_metrics$end_traverse_cor, eval_metrics$straightness, xlab = 'end traverse correlation', ylab = 'route straightness', main = params$species)
  #   dev.off()
  #   
  #   # Visualize model with best LL
  #   # 
  #   # quick_visualize_routes(1)
  #   
  #   ## 3d plot of straightness, end_traverse_cor, and ll
  #   
  #   rgl::plot3d(
  #     x = eval_metrics$ll, y = eval_metrics$straightness, z = eval_metrics$end_traverse_cor,
  #     col = 'gray',
  #     type = 's',
  #     xlab="ll", ylab="straightness", zlab="end_traverse_cor")
  #   # To display in an R Markdown document:
  #   # rglwidget()
  #   #
  #   # # To save to a file:
  #   htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
  #                           file = file.path(params$output_path, "_3d_ll_straightness_traverse_cor.html"),
  #                           libdir = "libs",
  #                           selfcontained = TRUE
  #   )
  #   
  #   ## 3d plot of stopover, straightness, and end_traverse_cor
  #   
  #   rgl::plot3d(
  #     x = eval_metrics$n_stopovers, y = eval_metrics$straightness, z = eval_metrics$end_traverse_cor,
  #     col = 'gray',
  #     type = 's',
  #     xlab="n_stopovers", ylab="straightness", zlab="end_traverse_cor")
  #   # To display in an R Markdown document:
  #   # rglwidget()
  #   #
  #   # # To save to a file:
  #   htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
  #                           file = file.path(params$output_path, "_3d_n_stopovers_straightness_traverse_cor.html"),
  #                           libdir = "libs",
  #                           selfcontained = TRUE
  #   )
  # } # end if eval_metrics rows > 1

} # big function end

#' Grid search, model selection, and model evaluation for multiple species on 
#' the cluster, with error handling
#' @param multispecies_vector a character vector that contains a valid inputs 
#' to [ebirdst::get_species()], e.g., eBird 6-letter codes or valid common 
#' names.
#' @inheritDotParams set_pipeline_params -species
#' 
#' @returns function is used for its many side effects, according to 
#' configuration settings in `as.list(BirdFlowPipeline:::the)`
#'  * create destination directories as needed
#'  * write preprocessed hdf5 files
#'  * write modelfit hdf5 files
#'  * write output files, plots, and maps for model evaluation and visualization
#' @seealso [batch_flow()] for doing this for one species
#' @export
multiple_species_batch <- function(multispecies_vector, ...) {
  for (species_i in multispecies_vector) {
    tryCatch({
      batch_flow(species_i, ...)
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
    })
  }
}
