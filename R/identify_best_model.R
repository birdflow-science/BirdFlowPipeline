# Used by big_collection workflow

#' Identify the best models in grid search
#' 
#' Identify the best models from training or test metrics calculated for all 
#' the models in a grid search.
#' `identify_best_models()` duplicates Yangkang's model selection code 
#' and so needs to be updated as he changes
#' the selection criteria.  
#' 
#' Note the appropriate value for `cor_threshold` depends on model resolution. 
#' To get an equivalent amount of filtering a lower threshold is needed
#' when the cell size is smaller.
#' 
#' Last update 2025-12-12 
#' 
#' @param metrics A table of evaluation metrics for all the models in the 
#' grid search.  This is saved to an Rds during Yangkang's grid search
#' workflow and can be calculated on either the training models 
#' (often `train`) or the reserved testing models (`test`).
#' @param cor_threshold A threshold in correlation between model distributions
#' and training (S&T) distributions below which models will be excluded 
#' for selection by some of the criteria.
#' @param cor_metric The particular correlation metric to which the 
#' threshold is applied -  a column in metrics.
#'
#' @returns A data.frame with:
#' \item{type}{the selection criteria used to select the model in its short form}
#' \item{model}{the model id as it apears in `metrics$model`}
#' @export
identify_best_models <- function(metrics, cor_threshold = 0.9, cor_metric = "min_dist_cor") {
  
  # Identify best models   
    
  filtered <- metrics[ metrics[[cor_metric]] > cor_threshold, , drop = FALSE] 
  
  # Log Likelihood  
  best_ll_model <- filtered |> 
    dplyr::arrange(-weighted_mean_ll) |> 
    dplyr::slice_head(n=1) |>
    dplyr::pull(model)
  
  # Distance Gained
  best_dg_model <- filtered |> 
    dplyr::arrange(-weighted_mean_win_distance_fraction) |> 
    dplyr::slice_head(n=1) |>
    dplyr::pull(model)
  
  # Multi-objective
  # calculate multi-objective desirability
  metrics <- metrics |>
    dplyr::mutate(
      min_dist_cor_d = desirability2::d_max(min_dist_cor, use_data = T),
      weighted_mean_win_distance_fraction_d = 
        desirability2::d_max(weighted_mean_win_distance_fraction, use_data = T),
      weighted_mean_ll_d = desirability2::d_max(weighted_mean_ll, use_data = T),
      desirability = desirability2::d_overall(dplyr::across(ends_with("_d")))
    ) 
  best_mo_model <- metrics |> 
    dplyr::arrange(-desirability) |> 
    head(1) |> 
    dplyr::pull(model)
  
  
  best <- data.frame(type = c("best_dg", "best_ll", "best_mo"),
                     model = c(best_dg_model, best_ll_model, best_mo_model))

  return(best)
}

