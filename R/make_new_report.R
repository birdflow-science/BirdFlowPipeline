#' Make species report
#'
#' `make_new_species_report()` uses template RMD files in the BirdFLowPipeline
#' package to make species reports which include figures and information on
#' model performance during the grid search and hyperparameter tuning.  This
#' is based on the 2026 tuning approach as described in
#' [Chen et al. 2026](www.biorxiv.org/content/10.1101/2025.09.30.679621v1.abstract)
#' and implemented in [BatchBirdFlowTrainer] and related functions.
#'
#' @param model path to HDF5 model file
#' @param bmtr path to BMTR RDS file containing output from [BirdFlowR::calc_bmtr()]
#' @param anim path to abundance and flux animation GIF
#' @param html path to output html file
#' @param routes path to routes plot
#' @param prevlink relative path to the previous species report or NULL if 
#' there is none.
#' @param nextlink relative path to the next species report or NULL if none.
#' @param test_metrics,train_metrics Optional data frame containing 
#' performance metrics from the grid search for the training and test 
#' datasets respectively. If NA than the tuning 
#' section isn't included in the report.
#'  @param title Title for report, default of `NA` results in  "<Common name> BMTR"
#'  @param cor_threshold,cor_metric Threshold and metric used to exclude
#'   models that are not sufficient close to the training S&T distributions
#' @param mean_rating Mean rating from 1 (worst) to 5 (best), 
#' assigned to the BirdFlow model
#'  by a minimum of two reviewers on the BirdFlow team.
#' @return Nothing is returned, a report is created at `html`
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' p <- list()
#' p$model <-  system.file("rmd/example_data/sstspa_model.hdf5", package = "BirdFlowPipeline") 
#' p$bmtr <-  system.file("rmd/example_data/sstspa_bmtr.Rds", package = "BirdFlowPipeline") 
#' p$anim <-  system.file("rmd/example_data/sstspa_anim.gif", package = "BirdFlowPipeline") 
#' p$html <-  tempfile(fileext = ".html", pattern = "report")
#' p$routes <-  system.file("rmd/example_data/sstspa_routes.hdf5", package = "BirdFlowPipeline") 
#' p$prevlink <-  NA
#' p$nextlink <-  NA
#' p$test_metrics <-  system.file("rmd/example_data/sstspa_test_metrics.rds", package = "BirdFlowPipeline") |> readRDS()                             
#' p$train_metrics <-  system.file("rmd/example_data/sstspa_train_metrics.rds", package = "BirdFlowPipeline")   |> readRDS()
#' p$title <- "Example Species Report"
#' p$cor_threshold <- 0.9
#' p$cor_metric <- "min_dist_cor"  
#' p$mean_rating <- 4
#' 
#' do.call(make_new_report, args = p)
#' 
#' cat("Report: ", p$html, "\n")
#' 
#'}
make_new_report <- function(model, 
                             bmtr, 
                             anim, 
                             html, 
                             routes, 
                             prevlink = NA, 
                             nextlink = NA,
                             test_metrics = NA, 
                             train_metrics = NA, 
                            selection_criteria = NA, 
                            title = NA, 
                            cor_threshold = NA,
                            cor_metric = NA,
                            mean_rating = NA) {
  
  
  rmd_file <- system.file("rmd/new_model_report.Rmd", package = "BirdFlowPipeline")
  
  suppressMessages({
    rmarkdown::render(
      input = rmd_file,
      output_file = html,
      params = list(model = model,
                    bmtr = bmtr, 
                    anim = anim, 
                    html = html,
                    routes = routes,
                    prevlink = prevlink, 
                    nextlink = nextlink, 
                    test_metrics = test_metrics,
                    train_metrics = train_metrics, 
                    selection_criteria = selection_criteria, 
                    title = title, 
                    cor_threshold = cor_threshold,
                    cor_metric = cor_metric,
                    mean_rating = mean_rating),
      quiet = !BirdFlowR::birdflow_options("verbose"))
  })
 
}