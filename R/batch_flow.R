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
  
  track_info <- make_tracks(file.path(the$banding_rds_path, paste0(params$species, '.rds')))
  saveRDS(track_info, file.path(params$output_path, 'track_info.rds'))
  
  # Batch model evaluation
  
  eval_metrics <- batch_evaluate_models(params, track_info)
  print(colnames(eval_metrics))
  
  # Model selection and ranking with desirability
  
  eval_metrics <- rank_models(eval_metrics, params)
  
  # save model evaluation RDS
  saveRDS(eval_metrics, file.path(params$output_path, 'eval_metrics.rds'))
  
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
