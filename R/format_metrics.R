#' Format metrics for plotting
#' 
#' This formats the metris 
#' 
#' @param train_metrics,test_metrics Similarly formatted evaluation metrics 
#' for all the models in the  grid search.  This is saved to an Rds during Yangkang's grid search
#' workflow and is calculated on either the training models or testing models.
#' @param cor_threshold A threshold in correlation between model distributions
#' and training (S&T) distributions below which models will be excluded 
#' for selection by some of the criteria.
#' @param cor_metric The particular correlation metric to which the 
#' threshold is applied -  a column in metrics.
#' @param include list of metric columns to include in the output. 
#' @param this_model The model id (in `metrics$model`) for the current model.
#' This is used when making a model report to indicate which model the report
#' is for,
#' @param use either `"test"` or `"train"` indicating which metrics should be
#' plotted.
#' @param long if `TRUE` use long format with metric and value column, 
#' otherwise use wider format with a column for each metric.
#' @returns A modified version of `metrics` with 
#' models labeled by type. Will include both the best and candidate models.
#' Columns:
#' \item{model}{the model file name - used as unique ID}
#' \item{type}{Classified model types. One of the types defined by [identify_best_models()] or one of
#' `this`, `candidate` or `low_cor`}
#' \item{metric}{one of the metric names - a column in the input}
#' \item{value}{the metric value.}
#' 
#' @export
format_metrics <- function(train_metrics, test_metrics, cor_threshold, cor_metric, this_model = NULL, include = NULL, use = "test", long = TRUE) {
  
  # Special models (best models and current model) are plotted differently  
  special <- identify_best_models(train_metrics, cor_threshold, cor_metric)
  if(!is.null(this_model)) {
    special <- rbind(special, data.frame(type = "this", model = this_model))
  }
  
  
  # Set type of standard models as either "candidate" or "low_cor"
  # Assess using training data regardless of whether we are plotting test
  # or training data
  train_metrics$type <- "candidate"
  train_metrics$type[train_metrics[[cor_metric]] < cor_threshold] <- "low_cor"
  stopifnot(setequal(train_metrics$model,test_metrics$model))
  test_metrics <- dplyr::left_join(test_metrics, train_metrics[ , c("model", "type")], 
                                   dplyr::join_by("model"))
  
  stopifnot(use %in% c("train", "test"))
  if(use == "train"){
    metrics <- train_metrics
  } else { 
    metrics <- test_metrics
  }

  if (!cor_metric %in% names(metrics)) 
    stop("cor_metric: ", cor_metric, " is not in the metrics table")
  
  # Add copies of the special models to metrics and then delete the version
  # with the standard type.
  # Note, this deliberately allows duplication of special models.
  # e.g. almost always this_model will be one of the best models but it's 
  # also possible that the same model is the best model for multiple criteria
  # in both of these cases we want to still plot all the best models (overlapping)
  special_metrics <- metrics[match(special$model, metrics$model), , drop = FALSE]
  special_metrics$type <- special$type
  metrics <- metrics[!metrics$model %in% special$model, ]
  metrics <- rbind(metrics, special_metrics)
  

  stopifnot(all(include %in% names(metrics)))  
  
  
  metrics <- dplyr::left_join(metrics, 
                              tuning_symbology |> dplyr::select(type, label), 
                              dplyr::join_by(type)
  )
  
  if(!long)
    return(metrics)
  
  metrics <-  metrics |> 
    dplyr::select(dplyr::all_of(c("model", "type", "label",  include))) |>
    tidyr::pivot_longer(cols = all_of(include), names_to = "metric")
  
   return(metrics) 
  
}