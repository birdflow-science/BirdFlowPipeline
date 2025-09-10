#' Grid search, model selection, and model evaluation for one species
#'
#' @param species A single eBird 6-letter code or common name describing 
#' a species recognized by [ebirdst::get_species()].
#' @param training_n_transitions The number of training transitions to use. For learning curve analysis.
#' @param training_CV The number of CV to use during model fitting.
#' @param use_cached_data Whether to use the cached training and validation data if there are any
#' @param cached_path If use caching, which path to export to.
#' @inheritDotParams set_pipeline_params -species
#' @returns function is used for its many side effects, according to 
#' configuration settings in `as.list(BirdFlowPipeline:::the)`
#'  * create destination directories as needed
#'  * write preprocessed hdf5 file
#'  * write modelfit hdf5 files
#'  * write reports, plots, and maps for model evaluation and visualization
#' @seealso [multiple_species_batch()]
#' @import glue
#' @export
batch_flow <- function(species, training_n_transitions=NULL, training_CV=1, use_cached_data=FALSE, cached_path=NULL, ...){

  if(!length(species) == 1){
    stop("batch_flow() only works with one species. ", 
         "Use multiple_species_batch()")
  }
  
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
  all_ground_truth_transitions_df_path1 <- file.path(params$output_path, 'all_ground_truth_transitions_df.rds')
  all_ground_truth_transitions_df_path2 <- file.path(cached_path, 'all_ground_truth_transitions_df.rds')
  interval_obj_path <- file.path(cached_path, 'interval_obj.rds')
  interval_one_week_obj_path <- file.path(cached_path, 'interval_one_week_obj.rds')
  
  if (use_cached_data &&
      file.exists(all_ground_truth_transitions_df_path2) &&
      file.exists(interval_obj_path) &&
      file.exists(interval_one_week_obj_path)) {
    if (is.null(cached_path)) {
      stop('Have to provide cached_path if using cache!')
    }
    print('Loading cached training/validation data...')
    combined_routes_data <- readRDS(all_ground_truth_transitions_df_path2) # cached dir
    interval_obj <- readRDS(interval_obj_path) # cached dir
    interval_one_week_obj <- readRDS(interval_one_week_obj_path) # cached dir
    saveRDS(combined_routes_data, all_ground_truth_transitions_df_path1) # normal dir
    
  } else {
    if (use_cached_data) {
      print('Althought use_cached_data, no cached data available.')
    }
    res <- get_ground_truth_routes_intervals_and_one_week_intervals(params, bf)
    combined_routes_data <- res[['combined_routes_data']]
    interval_obj <- res[['interval_obj']]
    interval_one_week_obj <- res[['interval_one_week_obj']]
    
    saveRDS(combined_routes_data, all_ground_truth_transitions_df_path1) # normal dir
    if (!is.null(cached_path)) {
      saveRDS(combined_routes_data, all_ground_truth_transitions_df_path2) # cached dir
      saveRDS(interval_obj, interval_obj_path) # cached dir
      saveRDS(interval_one_week_obj, interval_one_week_obj_path) # cached dir
    }
  }
  
  params$transition_type <- 'all_combined'
  
  # Train-test split
  set.seed(42)
  train_data <- interval_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
  test_data <- dplyr::setdiff(interval_obj$data, train_data)
  set.seed(42)
  if (is.null(training_n_transitions)) {
    ## Nothing happens
  } else {
    if (nrow(train_data) < training_n_transitions) {
      stop(glue::glue('Cannot sample {training_n_transitions} transitions -- not enough data.'))
    } else {
      train_data <- train_data|> dplyr::sample_n(training_n_transitions, replace = FALSE) ## Subsample, for learning curve analysis
    }
  }

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
  train_data_one_week <- interval_one_week_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
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
  
  if (!is.numeric(training_CV)) {
    stop('training_CV should be numeric!')
  }
  
  idx <- sample.int(nrow(train_data$data))
  if (training_CV==1) {
    parts <- list(`1`=idx)
  } else {
    parts <- split(
      idx,
      cut(seq_along(idx), breaks = training_CV, labels = FALSE)
    )
  }
  
  ## Run CV
  for (cv_iter in names(parts)) {
    the_idx <- parts[[as.character(cv_iter)]]
    this_training_data <- train_data
    this_training_data$data <- this_training_data$data[the_idx, ]
    
    params$mode <- 'train'
    eval_metrics_train <- batch_evaluate_models(params, this_training_data, train_data_one_week)
    params$model_selection <- 'distance_metric'
    eval_metrics_train1 <- rank_models(eval_metrics_train, params)
    
    ## Decide appendix names
    if (length(names(parts)) > 1) {
      output_name_appendix <- glue::glue('_cv{cv_iter}')
    } else {
      output_name_appendix <- ''
    }
    
    saveRDS(eval_metrics_train1, file.path(params$output_path, glue::glue('eval_metrics_train_distance_metric_all_combined{output_name_appendix}.rds')))
    
    ## Model ranking using real tracking data
    params$model_selection <- 'real_tracking'
    eval_metrics_train2 <- rank_models(eval_metrics_train, params)
    if (is.null(eval_metrics_train2)) {
      print("Tracking data might not be available for this species")
    }
    saveRDS(eval_metrics_train2, file.path(params$output_path, glue::glue('eval_metrics_train_multi_objective_all_combined{output_name_appendix}.rds')))

  ## Evaluate on test set
  params$mode <- 'test'
  eval_metrics_test <- batch_evaluate_models(params, test_data, test_data_one_week)
  params$model_selection <- 'distance_metric'
  eval_metrics_test <- rank_models(eval_metrics_test, params)
  saveRDS(eval_metrics_test, file.path(params$output_path, glue::glue('eval_metrics_test_distance_metric_all_combined.rds')))
  
  eval_metrics <- eval_metrics_test
  }

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
