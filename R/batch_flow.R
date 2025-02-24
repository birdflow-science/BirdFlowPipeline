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
  
  # species = 'amewoo'
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
  
  # Load and save track info
  # Here should combine banding and motus data and convert to BirdFlowIntervals class
  banding_df <- load_banding_transitions_df(file.path(the$banding_rds_path, paste0(params$species, '.rds')))
  motus_df <- load_motus_transitions_df(file.path(the$motus_rds_path, paste0(params$species, '.rds')))
  combined_data <- rbind(banding_df, motus_df)
  combined_data <- na.omit(combined_data)
  saveRDS(combined_data, file.path(params$output_path, 'all_ground_truth_transitions_df.rds'))
  
  # Dataframe to Routes
  if (is.null(banding_df) & !is.null(motus_df)){
    source <- 'MOTUS'
  } else if (!is.null(banding_df) & is.null(motus_df)){
    source <- 'Banding'
  } else if (!is.null(banding_df) & !is.null(motus_df)){
    source <- 'Banding & MOTUS'
  }else {
    source <- 'No Data'
  }
  routes_obj <- BirdFlowR::Routes(combined_data, species=list(), metadata=params$metadata, source=source)
  
  # Routes to BirdFlowRoutes to BirdFlowIntervals
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
  
  # Get the intervals
  interval_obj <- routes_obj |> 
    BirdFlowR::as_BirdFlowRoutes(bf=bf,
                                 aggregate = "random") |> 
    BirdFlowR::as_BirdFlowIntervals(max_n=5000,
                                    min_day_interval=7,
                                    max_day_interval=180,
                                    min_km_interval=200,
                                    max_km_interval=8000)
  
  # Train-test split
  set.seed(42)
  train_data <- interval_obj$data |> dplyr::sample_frac(0.7)
  test_data <- dplyr::setdiff(interval_obj$data, train_data)
  train_data <- BirdFlowIntervals(data=train_data,
                                  species=interval_obj$species,
                                  metadata=interval_obj$metadata,
                                  geom=interval_obj$geom,
                                  dates=interval_obj$dates,
                                  source=interval_obj$source)
  test_data <- BirdFlowIntervals(data=test_data,
                                  species=interval_obj$species,
                                  metadata=interval_obj$metadata,
                                  geom=interval_obj$geom,
                                  dates=interval_obj$dates,
                                  source=interval_obj$source)
  
  
  # Batch model evaluation
  eval_metrics <- batch_evaluate_models(params, train_data)
  eval_metrics_test <- batch_evaluate_models(params, test_data)
  
  # Model selection and ranking with desirability
  eval_metrics <- rank_models(eval_metrics, params)
  
  # save model evaluation RDS
  saveRDS(eval_metrics, file.path(params$output_path, 'eval_metrics.rds'))
  saveRDS(eval_metrics_test, file.path(params$output_path, 'eval_metrics_test.rds'))
  
  ## Plotting
  
  # make PCA evaluation plot
  
  if (nrow(eval_metrics) > 1){
    model_evaluation_biplot(eval_metrics, params)
    
    # Plot likelihood results cube
    make_3d_plot('color_ll', 'll', eval_metrics, params)
    # Plot null likelihood cube
    make_3d_plot('color_nll', 'nll', eval_metrics, params)
    # Plot correlation cube
    make_3d_plot('color_cor', 'cor', eval_metrics, params)
    
    # create model reports for top 5 models
    
    for (i in 1:5){
      bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, eval_metrics$model[i]))
      rmarkdown::render(system.file("rmd", "model_report.Rmd", package = "BirdFlowPipeline"), 
                        output_file = paste0("model_report", i, ".html"), output_dir = params$output_path)
    }
    
    # plot most desirable models
    
    for (i in 1:5){
      pdf(file.path(params$output_path, paste0('desirability', i, '.pdf')))
      quick_visualize_routes(i, df = eval_metrics, dir = params$hdf_dir, season = params$season)
      dev.off()
    }
    
    # graph tradeoff
    
    pdf(file.path(params$output_path, 'straightness_vs_end_traverse_cor.pdf'))
    plot(eval_metrics$end_traverse_cor, eval_metrics$straightness, xlab = 'end traverse correlation', ylab = 'route straightness', main = params$species)
    dev.off()
    
    # Visualize model with best LL
    # 
    # quick_visualize_routes(1)
    
    ## 3d plot of straightness, end_traverse_cor, and ll
    
    rgl::plot3d(
      x = eval_metrics$ll, y = eval_metrics$straightness, z = eval_metrics$end_traverse_cor,
      col = 'gray',
      type = 's',
      xlab="ll", ylab="straightness", zlab="end_traverse_cor")
    # To display in an R Markdown document:
    # rglwidget()
    #
    # # To save to a file:
    htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                            file = file.path(params$output_path, "_3d_ll_straightness_traverse_cor.html"),
                            libdir = "libs",
                            selfcontained = TRUE
    )
    
    ## 3d plot of stopover, straightness, and end_traverse_cor
    
    rgl::plot3d(
      x = eval_metrics$n_stopovers, y = eval_metrics$straightness, z = eval_metrics$end_traverse_cor,
      col = 'gray',
      type = 's',
      xlab="n_stopovers", ylab="straightness", zlab="end_traverse_cor")
    # To display in an R Markdown document:
    # rglwidget()
    #
    # # To save to a file:
    htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                            file = file.path(params$output_path, "_3d_n_stopovers_straightness_traverse_cor.html"),
                            libdir = "libs",
                            selfcontained = TRUE
    )
  } # end if eval_metrics rows > 1
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
