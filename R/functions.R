# pre-processing functions
preprocess_data_types <- function(df){
  # coerce ambiguous dates to NAs
  # convert codes for 1st, 2nd, and last 10 days of month to their midpoint
  # note these are not present in the database!
  # df$EVENT_DATE <- sub('[/]41[/]', '/05/', df$EVENT_DATE)
  # df$EVENT_DATE <- sub('[/]42[/]', '/15/', df$EVENT_DATE)
  # df$EVENT_DATE <- sub('[/]43[/]', '/25/', df$EVENT_DATE)
  # convert all other imprecise date codes to NA
  df$EVENT_DATE <- as.Date(df$EVENT_DATE, format = '%m/%d/%Y')
  # fields contain both NAs and empty strings, so make sure to do the below
  #df$BIRD_INFO <- suppressWarnings(as.integer(df$BIRD_INFO))
  df$EXTRA_INFO <- suppressWarnings(as.integer(df$EXTRA_INFO))
  df$BIRD_STATUS <- suppressWarnings(as.integer(df$BIRD_STATUS))
  df
}

preprocess_exclusions <- function(df){
  # exclude invalid dates
  df <- dplyr::filter(df, !is.na(.data$EVENT_DATE))
  # exclude hand-reared, experimental, transported, rehabbed, held, sick, dead
  df <- dplyr::filter(df, ! .data$BIRD_STATUS %in% c(2, 4, 5, 6, 7, 8, 9))
  # exclude state or country level locations
  df <- dplyr::filter(df, ! .data$CP %in% c(12, 72))
  # exclude records with longitude of 0 (might be real records with 0 latitude)
  df <- dplyr::filter(df, .data$LON_DD != 0)
  # exclude records with missing latitude or longitude
  df <- dplyr::filter(df, !is.na(.data$LON_DD) & !is.na(.data$LAT_DD))
  df
}

preprocess_with_recovery <- function(df){
  # delete exluded records
  df <- df %>% dplyr::group_by(.data$SPECIES_ID, .data$BAND) %>% (dplyr::filter)(dplyr::n() >= 2) %>% (dplyr::ungroup)
  df
}

preprocess_sort_band_date <- function(df){
  df %>% dplyr::arrange(.data$BAND, .data$EVENT_DATE)
}

preprocess_calc_distance_days <- function(df){
  df %>%
    (dplyr::group_by)(.data$BAND) %>%
    (dplyr::mutate)(distance = geodist::geodist(data.frame(lon = .data$LON_DD, lat = .data$LAT_DD),
                                       sequential = TRUE, measure = 'geodesic', pad = TRUE) / 1000) %>%
    (dplyr::mutate)(days = as.integer(.data$EVENT_DATE - dplyr::lag(.data$EVENT_DATE))) %>%
    (dplyr::ungroup)
}



#' Safely return a real value
#'
#' @param x Input
#'
#' @return x if x is not null, else NA_real_
safe_numeric <- function(x){
  ifelse(!is.null(x), x, NA_real_)
}



#' 3d plot function
#' @param color_column column to use for the colors
#' @param suffix suffix to use for output filenames
#' @param eval_metrics data.frame produced from rbindlist-ing output from [evaluate.BatchBirdFlowEvaluator()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [evaluate.BatchBirdFlowEvaluator()], [evaluate_model()], [preprocess_species_wrapper()]
#' @returns side-effect is a plot file created
#' @export
make_3d_plot <- function(color_column, suffix, eval_metrics, params){
  ## color_column can be color_ll, color_nll, or color_cor

  # Set the plot colors
  eval_metrics$color_ll <- grDevices::hcl.colors(15, rev = TRUE)[cut(eval_metrics$ll, 15)]
  eval_metrics <- eval_metrics %>% dplyr::mutate(color_nll = dplyr::if_else(.data$ll < .data$nll, '#ffffff', .data$color_ll))
  cor_breaks <- c(-Inf, 0.9, 0.95, 0.975, Inf)
  cor_labels <- c("< 0.9", "0.9 to <0.95", "0.95 to <0.975", ">= 0.975")
  cor_colors <- c('#FFFFFF', grDevices::hcl.colors(3, rev = TRUE))
  eval_metrics$color_cor <- cor_colors[cut(eval_metrics$traverse_cor, breaks = cor_breaks)]

  rgl::plot3d(
    x = eval_metrics$ent_weight, y = eval_metrics$dist_weight, z = eval_metrics$dist_pow, 
    col = eval_metrics[[color_column]], 
    type = 's', 
    radius = .02,
    xlab="ent_weight", ylab="dist_weight", zlab="dist_pow")
  # To display in an R Markdown document:
  # rglwidget()
  # 
  # # To save to a file:
  htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                          file = file.path(params$output_path, paste0(params$species, "_", params$res, "km_3dscatter_", suffix, ".html")),
                          libdir = "libs",
                          selfcontained = TRUE
  )
}

#' Calculate route summary statistics
#' @param rts BirdFlowRoutes object output from `BirdFlowR::route()` or
#'   a data frame with the same data. 
#' @param remove_head_and_tail_stationaries Whether to remove the head and tail
#' Stationary period. Useful when the calculaton is across multiple seasons (
#' e.g., From nonbreeding to breeding)
#' @returns a list of mean summary statistics
#' @seealso [BirdFlowR::route()], [BirdFlowR::BirdFlowRoutes()]
#' @export
#' 
rts_stats <- function(rts, remove_head_and_tail_stationaries = FALSE){

  if(inherits(rts, "data.frame")){
    # Continue to support older route format
    data <- rts
  } else {  
    # BirdFlowRoutes object
    data <- rts$data
  }
  
  ## Deal with only one track scenarios
  if (length(unique(data$route_id)) == 1){
    rts_lst <- list(data)
  } else {
    rts_lst <- split(data, data$route_id)
  }

  out <- lapply(rts_lst, function(rts){
    if (remove_head_and_tail_stationaries){

      min_rts_stay_id <- min(rts$stay_id)
      max_rts_stay_id <- max(rts$stay_id)
      if (rts[rts$stay_id==min_rts_stay_id,]$stay_len[1]>0){
        ## The first location is a stationary, remove the redundant timesteps
        the_stationaries <- rts[(rts$stay_id==min_rts_stay_id),]
        rts <- rbind(the_stationaries[nrow(the_stationaries),], rts[!(rts$stay_id==min_rts_stay_id),])
      }
      if (rts[rts$stay_id==max_rts_stay_id,]$stay_len[1]>0){
        ## The last location is a stationary, remove the redundant timesteps
        the_stationaries <- rts[(rts$stay_id==max_rts_stay_id),]
        rts <- rbind(rts[!(rts$stay_id==max_rts_stay_id),], the_stationaries[nrow(the_stationaries),])
      }
      if (nrow(rts)==0){
        return(NULL)
      }
    }
    
    rts$ts_sequential <- seq_len(nrow(rts))
    traj <- trajr::TrajFromCoords(rts, xCol = 'x', yCol = 'y', timeCol = 'ts_sequential', timeUnits = 'ts')
    
    rts <- rts[order(rts$date), ]
    dx <- diff(rts$x)
    dy <- diff(rts$y)
    distances <- sqrt(dx^2 + dy^2)
    total_distance <- sum(distances)
    total_days <- as.numeric(rts$date[nrow(rts)] - rts$date[1], unite='days')
    speed <- total_distance / total_days
    
    my_intermediate_stays <- rts[
      (!(rts$stay_id %in% c(min(rts$stay_id), max(rts$stay_id)))) & (rts$stay_len>=7)
      ,]
    my_n_stopovers <- my_intermediate_stays$stay_id |> unique() |> length()
    
    list(
      straightness = trajr::TrajStraightness(traj),
      length = trajr::TrajLength(traj)/1000,
      displacement = trajr::TrajDistance(traj)/1000,
      n_stopovers_daves = max(sum(trajr::TrajStepLengths(traj) > 0) - 1, 0),
      speed = speed,
      n_stopovers = my_n_stopovers
    )
  })
  
  out <- out[!sapply(out, is.null)]
  out <- out |> data.table::rbindlist() |> colMeans(na.rm = TRUE) |> as.list()
  out
}



#' PCA biplot hyperparameters evaluation
#' @param eval_metrics data.frame produced from rbindlist-ing output from [evaluate.BatchBirdFlowEvaluator()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [evaluate.BatchBirdFlowEvaluator()], [preprocess_species_wrapper()]
#' @returns side effect is a plot written to file
#' @export
model_evaluation_biplot <- function(eval_metrics, params){
  # workaround for unexported method ggfortify:::autoplot.princomp()
  requireNamespace('ggfortify', quietly = TRUE)
  outfile <- file.path(params$output_path, 'pca_evaluation.pdf')
  if (length(unique(eval_metrics$ll)) == 1) {eval_metrics$ll <- NULL}
  pca_columns <- c(
    'ent_weight',
    'dist_weight',
    'dist_pow',
    'traverse_cor',
    'll',
    'straightness',
    'length',
    'displacement'
  )
  pca_columns <- pca_columns[pca_columns %in% names(eval_metrics)]
  fit <- stats::princomp(eval_metrics[,pca_columns], cor = TRUE)
  pdf(outfile, 13, 5.5)
  plot1 <- ggplot2::autoplot(fit, color = 'straightness',
                    loadings = TRUE,
                    loadings.label = TRUE,
                    loadings.colour = 'gray',
                    loadings.label.colour = 'black',
                    loadings.label.size = 2.5,
                    label = TRUE,
                    label.colour = 'black',
                    label.repel = TRUE,
                    label.size = 2.5) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_c(limits = c(0, 1))
  
  plot2 <- ggplot2::autoplot(fit, color = 'traverse_cor',
                    loadings = TRUE,
                    loadings.label = TRUE,
                    loadings.colour = 'gray',
                    loadings.label.colour = 'black',
                    loadings.label.size = 2.5,
                    label = TRUE,
                    label.colour = 'black',
                    label.repel = TRUE,
                    label.size = 2.5) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_viridis_c()
  
  suppressWarnings({
    gridExtra::grid.arrange(plot1, plot2, top = substr(eval_metrics$model[1], 1, 6), ncol=2)
    })
  dev.off()
}

#' Quick visualize a spring migration routes simulation from eval_metrics by row number
#' @param i row number in `df` from which to extract model
#' @param n number of routes to simulate for visualization
#' @param season season for which to simulate route, parseable by [BirdFlowR::lookup_season_timesteps()]
#' @param df a `eval_metrics` produced by [evaluate.BatchBirdFlowEvaluator()]
#' @param dir hdf5 directory from `params` list.  Need to refactor some of these arguments
#' @seealso \code{BirdFlowR::lookup_season_timesteps()}, [evaluate.BatchBirdFlowEvaluator()]
#' @returns side effect is an interactive plot
#' @export
quick_visualize_routes <- function(i, n = 10, season = 'prebreeding', df = eval_metrics, dir = hdf_dir){
  eval_metrics <- df
  hdf_dir <- dir
  bf <- BirdFlowR::import_birdflow(file.path(hdf_dir, eval_metrics$model[i]))
  # 
  # ## Plot map route_migration spring msap
  # 
  rts <- BirdFlowR::route(bf = bf, n = n, season = season, from_marginals = TRUE)
  print(
    {
      BirdFlowR::plot_routes(rts, bf, use_seasonal_colors = FALSE) +
        ggplot2::labs(title = eval_metrics$model[i])
    }
  )
}

#' Rank the models in an eval_metrics data frame
#'
#' @param eval_metrics The eval_metrics data frame often produced by 
#' recombining results from [evaluate.BatchBirdFlowEvaluator()]
#' @param params The standard params list, which includes `model_selection` method
#' @returns Updated eval_metrics including desirability columns
#' @export
rank_models <- function(eval_metrics, params){
  # remove any existing desirability columns (for interactive scripting)
  eval_metrics <- eval_metrics %>% (dplyr::select)(-dplyr::ends_with("_d"))
  eval_metrics$overall_des <- NULL
  # Calculate desirability columns depending on chosen criteria set
  if (params$model_selection == 'str_etc'){
    # straightness and end traverse correlation only
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        str_d = desirability2::d_target(.data$straightness, low = 0.5, target = 0.85, high = 1, use_data = TRUE),
        etc_d = desirability2::d_max(.data$traverse_cor, use_data = TRUE)
      )
  } else if (params$model_selection == 'pit_etc'){
    # PIT metrics and end traverse correlation only
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
        d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
        d_pit_in_95 = desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE),
        pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit"))),
        etc_d = desirability2::d_max(.data$traverse_cor, use_data = TRUE)
      )
  } else if (params$model_selection == 'real_tracking'){
    # Tracking-focused model selection
    # Includes traverse correlation, PIT scores, and
    # straightness + n_stopovers targetted to observed values from real tracking
    bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, eval_metrics$model[1])) # Load a random model
    real_track <- get_real_track(bf, params, filter=TRUE)
    if (is.null(real_track)) {
      return(NULL)
    } 
    
    real_track_stats_res <- rts_stats(real_track)
    # save real_track_stats RDS
    saveRDS(real_track_stats_res, file.path(params$output_path, 'real_track_stats.rds'))
    stopifnot(
      !is.na(real_track_stats_res$straightness),
      !is.na(real_track_stats_res$n_stopovers)
    )
    
    if (sum(is.na(eval_metrics$pit_row))==0){
      # If the pit are evaluated with enough data
      eval_metrics <- eval_metrics |> 
        dplyr::mutate(
          d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
          d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
          d_pit_in_95 = desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE),
          pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit")))
        )
    }
    
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        etc_d = desirability2::d_max(.data$traverse_cor, use_data = TRUE),
        str_d = desirability2::d_min(abs(.data$straightness - real_track_stats_res$straightness), use_data = TRUE),
        nso_d = desirability2::d_min(abs(.data$n_stopovers - real_track_stats_res$n_stopovers), use_data = TRUE),
        straightness_real_track = real_track_stats_res$straightness,
        n_stopovers_real_track = real_track_stats_res$n_stopovers,
        n_stopovers_daves_real_track = real_track_stats_res$n_stopovers_daves,
        speed_real_track = real_track_stats_res$speed,
        length_real_track = real_track_stats_res$length,
        straightness_diff = abs(.data$straightness - real_track_stats_res$straightness),
        n_stopovers_diff = abs(.data$n_stopovers - real_track_stats_res$n_stopovers),
        speed_diff = abs(.data$speed - real_track_stats_res$speed),
      )
    
  } else if (params$model_selection == 'real_tracking_no_cal'){
    # Tracking-focused model selection
    # Includes traverse correlation, and
    # straightness + n_stopovers targetted to observed values from real tracking
    # But no PIT scores!
    bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, eval_metrics$model[1])) # Load a random model
    real_track <- get_real_track(bf, params, filter=TRUE)
    real_track_stats_res <- rts_stats(real_track)
    
    # save rts_stats RDS
    saveRDS(real_track_stats_res, file.path(params$output_path, 'real_track_stats.rds'))
    stopifnot(
      !is.na(real_track_stats_res$straightness),
      !is.na(real_track_stats_res$n_stopovers)
    )
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        etc_d = desirability2::d_max(.data$traverse_cor, use_data = TRUE),
        str_d = desirability2::d_min(abs(.data$straightness - real_track_stats_res$straightness), use_data = TRUE),
        nso_d = desirability2::d_min(abs(.data$n_stopovers - real_track_stats_res$n_stopovers), use_data = TRUE)
      )
  } else if (params$model_selection == 'averaged_parameters') {
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        dummy_d = rep(1, nrow(eval_metrics)),
      )
  } else if (params$model_selection == 'distance_metric') {
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        dummy_d = rep(1, nrow(eval_metrics)),
      )
  } else {
    stop('invalid model_selection in params')
  }

  # Calculate overall desirability using desirability columns ending with '_d'
  eval_metrics <- eval_metrics %>% dplyr::mutate(
    overall_des = desirability2::d_overall(dplyr::across(dplyr::ends_with("_d")))
    )
  # Do desirability rankings (and return the new eval_metrics)
  eval_metrics %>% (dplyr::arrange)(-.data$overall_des)
}

