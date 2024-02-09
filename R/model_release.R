#' Copy model file and html model report from pipeline output to release staging folder
#'
#' @param output_directory dir where pipeline outputs models and reports
#' @param desirability_rank desirability rank of model to release (1 = top model)
#' @param staging_directory where to put release staging models and Rds files
#'
#' @returns side effects, copies Rds and html files to staging folder
#' @export
stage_model_release <- function(
    output_directory,
    desirability_rank,
    staging_directory = the$model_release_staging_path
    ){
  ll_df <- readRDS(file.path(output_directory, 'll_df.rds'))
  params <-readRDS(file.path(output_directory, 'params.rds'))
  filename_base <- paste(params$species, params$season, sep = '_')
  rds_new_filename <- paste0(filename_base, '.Rds')
  html_new_filename <- paste0(filename_base, '.html')
  # Put Rds of chosen model in staging
  bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, ll_df$model[desirability_rank]))
  saveRDS(bf, file.path(staging_directory, rds_new_filename))
  # Put html model report for chosen model in staging
  invisible(
    file.copy(
      from = file.path(params$output_path, paste0('model_report', desirability_rank, '.html')),
      to = file.path(staging_directory, html_new_filename),
      overwrite = TRUE
    )
  )
}

#' Rebuild model report HTML files from existing output directory
#'
#' @param output_directory The output directory
#' @param model_numbers Number of top models to rebuild Html files for
#'
#' @returns Side effects, rebuilt Html files in output directory
#' @export
rebuild_model_reports <- function(output_directory, model_numbers = 1:5){
  ll_df <- readRDS(file.path(output_directory, 'll_df.rds'))
  params <-readRDS(file.path(output_directory, 'params.rds'))
  for (i in model_numbers){
    bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, ll_df$model[i]))
    rmarkdown::render(system.file("rmd", "model_report.Rmd", package = "BirdFlowPipeline"), 
                      output_file = paste0("model_report", i, ".html"), output_dir = params$output_path)
  }
}
