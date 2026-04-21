#' Plot grid search metrics
#'
#' Plot the selected model metrics relative to the 
#' distribution of across all models in the grid search
#' @param train_metrics training statistics data frame
#' @param test_metrics test statistics data frame
#' @param bf bird flow model
#' @param include Names of metrics to include in the plots should be columns
#'  in `train_metrics` or `test_metrics`. 
#' @param cor_threshold correlation threshold used when selecting models
#' @param cor_metric correlation metrics used when selecting models
#' @param use `"test"` or `"train"` to indicate what statistics to use in the plot
#' @returns A ggplot plot object
#' @export
#'
#' @examples
#' files <- c("sstspa_model.hdf5", "sstspa_test_metrics.rds", "sstspa_train_metrics.rds")
#' paths <- lapply(paste0("rmd/example_data/", files), function(x) system.file(x, package = "BirdFlowPipeline"))
#' names(paths) <- c("model", "test_metrics", "train_metrics")
#' bf <- import_birdflow(paths$model)
#' test_metrics <- readRDS(paths$test_metrics)
#' train_metrics <- readRDS(paths$train_metrics)
#' 
#' included_metrics <- c("mean_dist_cor", "mean_ll", "mean_energy_improvement", 
#'   "weighted_mean_win_prob", "weighted_mean_win_distance",
#'   "weighted_mean_win_distance_fraction", "weighted_mean_ll", 
#'   "weighted_energy_improvement", "pit_row", "pit_col")
#' 
#' plot_grid_search_metrics(train = train_metrics, test = test_metrics, bf, include = included_metrics,  cor_threshold = 0.9, cor_metric = "min_dist_cor")
#' 
plot_grid_search_metrics <- function(train_metrics, test_metrics, bf = NULL, include = NULL, cor_threshold, cor_metric, use = "test") {
 
  if(!is.null(bf)) {
    hp <- bf$metadata$hyperparameters
    sv <- rep(TRUE, nrow(train_metrics))
    for(par in c("dist_pow", "ent_weight", "dist_weight", "obs_weight" )) {
      sv <- sv & train_metrics[[par]] %in% hp[[par]]
    } 
    if(!sum(sv) == 1)
      stop("Failed to indentify this model in grid search training metrics")
    this_model <- train_metrics$model[sv]
  }
  
  
  if (!all(include %in% names(train_metrics))) {
    stop("Some metrics from include weren't in the training data: ",
         paste(setdiff(include, names(train_metrics)), collapse = ", "))
  }
  
  

  
  metrics <- format_metrics(train_metrics, test_metrics, cor_threshold = cor_threshold, cor_metric = cor_metric, this_model = this_model, use = use, include = include)
  
  lines <- metrics[!metrics$type %in% c("candidate", "low_cor"), , drop = FALSE] 
  
  # Put "this" first
  lines <- lines[order(lines$type != "this"), ]
  
  # Reformat symbology into named vectors for use with scale_<x>_manual()
  symbology <- tuning_symbology
  values <- list()  
  for(col in c("color", "linewidth", "shape", "size")){
    a <- symbology[[col]]
    names(a) <- symbology$label
    values[[col]] <- a
  }

  p <- metrics |> 
    dplyr::filter(type != "low_cor") |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_density() 
  

  
  p <- p +
    ggplot2::geom_vline(ggplot2::aes(xintercept = value, color = label, linewidth = label), data = lines) +
    ggplot2::facet_wrap(facets = ggplot2::vars(metric), scales = "free", ncol = 2) + 
    ggplot2::scale_color_manual(values = values$color) + 
    ggplot2::scale_linewidth_manual(values = values$linewidth) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank()) + 
    ggplot2::xlab(NULL) + 
    ggplot2::ylab(NULL)
  
  
  p
}