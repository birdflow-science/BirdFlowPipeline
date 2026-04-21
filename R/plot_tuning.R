#' Plot gridsearch performance metrics
#'
#' @param train training statistics data frame
#' @param test test statistics data frame
#' @param bf bird flow model
#' @param cor_threshold correlation threshold used when selecting models
#' @param cor_metric correlation metrics used when selecting models
#' @param use `"test"` or `"train"` to indicate what statistics to use in the plot
#'
#' @returns A ggplot2 plot object
#' @export
#'
#' @examples
#' 
#' files <- c("sstspa_model.hdf5", "sstspa_test_metrics.rds", "sstspa_train_metrics.rds")
#' paths <- lapply(paste0("rmd/example_data/", files), function(x) system.file(x, package = "BirdFlowPipeline"))
#' names(paths) <- c("model", "test_metrics", "train_metrics")
#' bf <- import_birdflow(paths$model)
#' test_metrics <- readRDS(paths$test_metrics)
#' train_metrics <- readRDS(paths$train_metrics)
#' 
#' plot_tuning(train = train_metrics, test = test_metrics, bf, 0.9 )
#' 
plot_tuning <- function(train, test, bf, cor_threshold, cor_metric = "min_dist_cor", use = "test") {
  
  # Identify the current model name as it appear in the grid search
  # training data 
  hp <- bf$metadata$hyperparameters
  sv <- rep(TRUE, nrow(train))
  for(par in c("dist_pow", "ent_weight", "dist_weight", "obs_weight" )) {
    sv <- sv & train[[par]] %in% hp[[par]]
  } 
  if(!sum(sv) == 1)
    stop("Failed to indentify this model in grid search training metrics")
  this_model <- train$model[sv]
  
  # Convert to long format and add labels for current model and best models
  # Note: update `identify_best_models()` to change how we identify the best
  # models included in the plot
  metrics <- format_metrics(train,
                            test, 
                            cor_threshold = cor_threshold, 
                            cor_metric = cor_metric,
                            this_model = this_model, 
                            use = use, 
                            long = FALSE, 
                            include = c("weighted_mean_ll",
                                        "weighted_mean_win_distance_fraction"))
  
  
  
  # Reformat symbology into named vectors for use with scale_<x>_manual()
  symbology <- tuning_symbology 
  values <- list()  
  for(col in c("color", "linewidth", "shape", "size", "fill")){
    a <- symbology[[col]]
    names(a) <- symbology$label
    values[[col]] <- a
  }
  
  
  p <- metrics |> 
    ggplot2::ggplot(mapping = ggplot2::aes(x = weighted_mean_ll, 
                         y = weighted_mean_win_distance_fraction, 
                         color = label, 
                         size = label, 
                         shape = label, 
                         fill = label)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = values$color, name = NULL) +
    ggplot2::scale_size_manual(values = values$size, name = NULL) + 
    ggplot2::scale_shape_manual(values = values$shape, name = NULL) +
    ggplot2::scale_fill_manual(values = values$fill, name = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line.x = ggplot2::element_line(linewidth = 0.3),
          axis.line.y = ggplot2::element_line(linewidth = 0.3)) +
    ggplot2::xlab("Weighted mean log likelihood") +
    ggplot2::ylab("Weighted mean relative distance gain")+
    ggplot2::ggtitle(species(bf))
  
  p
  
}



# Symbology to use when plotting models
tuning_n_best <- 3
tuning_best_colors <- scales::hue_pal()(tuning_n_best) |> adjustcolor(alpha = 0.7)

tuning_symbology <- data.frame( type = c("best_dg", 
                                      "best_ll", 
                                      "best_mo", 
                                      "candidate", 
                                      "low_cor", 
                                      "this"),
                             label = c("Best distance gained",
                                       "Best log likelihood", 
                                       "Best multi objective", 
                                       "Candidate models", 
                                       "Low distr. correlation", 
                                       "This model"), 
                             color = c(tuning_best_colors, # , #1927DDFF, #0D0006FF  # fishualize::Acanthurus_sohal
                                       
                                       # Dark three color         "#1b9e77", "#d95f02","#7570b3", 
                                       "#000000CC",
                                       "#00000022",
                                       "#000000CC"
                             ), 
                             fill = c(tuning_best_colors, rep("#000000CC", 3)),  # 
                             linewidth = c(rep(1, tuning_n_best), 1, 1, 2), 
                             shape = c(rep(19, tuning_n_best), 19, 19, 1), 
                             size = c(rep(1.8, tuning_n_best), 1, 1, 3.2)
)