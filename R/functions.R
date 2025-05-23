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

#' Prepare banding data for `BirdFlowR::interval_log_likelihood()`
#' 
#' @param banding_rds_path file path to `.rds` file containing raw banding data
#' @param max_band_tracks maximum number of band_tracks to output, downsample if necessary
#' @param max_preprocess  maximum number of bands to preprocess, downsample if necessary
#' @param min_dist_m minimum distance in meters between origin and destination to retain a band_track
#' @param max_days maximum number of days between origin and destination to retain a band_track
#' @returns a list:
#'  * `obs_df` data.frame of observations for `BirdFlowR::interval_log_likelihood()`
#'  * `int_df` data.frame of intervals for `BirdFlowR::interval_log_likelihood()`
#' @seealso [BirdFlowR::interval_log_likelihood()]
#' @export
make_tracks <- function(
    banding_rds_path,
    max_band_tracks = 10000,
    max_preprocess = 500000,
    min_dist_m = 15000,
    max_days = 180){
  file_exists <- dplyr::if_else(file.exists(banding_rds_path), TRUE, FALSE)
  if (file_exists){
    df <- readRDS(banding_rds_path)
  }
  if (!file_exists || nrow(df) == 0) {
    # return zero-row track_info
    return(list(
      obs_df = structure(
        list(
          BAND = character(0),
          EVENT_TYPE = character(0),
          date = structure(numeric(0), class = "Date"),
          lat = numeric(0),
          lon = numeric(0),
          EBIRDST_CODE = character(0),
          BAND_TRACK = character(0),
          distance = numeric(0),
          days = integer(0),
          when = character(0),
          id = integer(0)
        ),
        row.names = integer(0),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      int_df = structure(
        list(
          BAND_TRACK = character(0),
          from = integer(0),
          to = integer(0)
        ),
        row.names = integer(0),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    ))
  }
  # Downsample to max for preprocessing
  sampled_bands <- sample(unique(df$BAND), dplyr::if_else(max_preprocess > dplyr::n_distinct(df$BAND), dplyr::n_distinct(df$BAND), max_preprocess))
  df <- df %>% dplyr::filter(.data$BAND %in% sampled_bands)
  # Function to convert banding df to an sf object of linestrings of origin-destination tracks
  # expand to two steps
  df <- df %>% dplyr::group_by(.data$BAND) %>%
    dplyr::mutate(count = c(1, rep(2, dplyr::n() - 2), 1)) %>%
    tidyr::uncount(.data$count) %>%
    dplyr::mutate(BAND_TRACK = paste(.data$BAND, rep(1:(dplyr::n()/2), each = 2), sep = '_')) %>%
    (dplyr::ungroup)
  df <- preprocess_calc_distance_days(df)
  df <- df %>% dplyr::select(c('BAND', 'EVENT_TYPE', 'EVENT_DATE', 'LAT_DD', 'LON_DD', 'EBIRDST_CODE', 'BAND_TRACK', 'distance', 'days'))
  df <- df %>% dplyr::group_by(.data$BAND_TRACK) %>%
    dplyr::filter(.data$distance[2] > min_dist_m / 1000) %>%
    dplyr::filter(.data$days[2] <= max_days) %>%
    dplyr::mutate(
      when = rep(
        c('from', 'to'), times = dplyr::n() / 2
      )
    ) %>%
    (dplyr::ungroup)
  # Downsample to max # band_tracks
  sampled_band_tracks <- sample(unique(df$BAND_TRACK), dplyr::if_else(max_band_tracks > dplyr::n_distinct(df$BAND_TRACK), dplyr::n_distinct(df$BAND_TRACK), max_band_tracks))
  df <- df %>% dplyr::filter(.data$BAND_TRACK %in% sampled_band_tracks)
  # Make IDs
  df$id <- seq_len(nrow(df))
  obs_df <- df %>% dplyr::rename(date = 'EVENT_DATE', lat = 'LAT_DD', lon = 'LON_DD')
  int_df <- df %>% dplyr::select(c('BAND_TRACK', 'when', 'id'))
  if (nrow(int_df) > 0){
    # typical behavior if data is present
    int_df <- tidyr::pivot_wider(int_df, id_cols = 'BAND_TRACK', names_from = 'when', values_from = 'id')
  } else {
    # return zero-row data frame with same columns as if there was data
    int_df <-
      structure(
        list(
          BAND_TRACK = character(0),
          from = integer(0),
          to = integer(0)
        ),
        row.names = integer(0),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
  }
  return(list(obs_df = obs_df, int_df = int_df))
}

#' Safely return a real value
#'
#' @param x Input
#'
#' @return x if x is not null, else NA_real_
safe_numeric <- function(x){
  ifelse(!is.null(x), x, NA_real_)
}

#' Import BirdFlow model, and apply further arguments to [evaluate_model()]
#'
#' @param path Path of hdf5 to pass to [BirdFlowR::import_birdflow()].
#' @param ... Further arguments passed to [evaluate_model()]
#' @export
import_birdflow_and_evaluate <- function(path, ...){
  bf <- BirdFlowR::import_birdflow(path)
  evaluate_model(bf, modelname = basename(path), ...)
}

#' Evaluate a BirdFlow model
#' 
#' `evaluate_model` calculates
#'  * Log likelihood of banding data with 
#'   [BirdFlowR::interval_log_likelihood()]
#'  * Probability integral transform (PIT) calibration of tracking data with `pit_calibration()`
#'  * Synthetic route statistics with [BirdFlowR::route()] and [rts_stats()]
#'
#' @param bf A birdflow model object, often passed from [import_birdflow_and_evaluate()]
#' @param modelname A name used to label the model in result tables.
#' Often the basename of the model path.
#' @param track_info Object produced from [make_tracks()]
#' @param params Standard params object created by [set_pipeline_params]
#'  The `species`, `season`, and `output_path` elements will be used. `params` 
#'  will also be passed on to `pit_calibration()` and `pit_plots()`.
#' @seealso [make_tracks()], [batch_evaluate_models()], [import_birdflow_and_evaluate()]
#' @returns A list:
#'  * `df` 1-row data.frame of model descriptors and metrics 
#'  * `obs` obs_df portion from original `track_info`
#'  * `int` = int_df portion from original `track_info`
#'
#' @export
evaluate_model <- function(bf, modelname, track_info, params){
  
  # Process banding data to make log likelihood 
  # Note "track_info" is banding data
  my_ll <- BirdFlowR::interval_log_likelihood(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf)
  
  # Generate synthetic routes and route stats (straightness etc)
  rts <- BirdFlowR::route(bf = bf, n = 100, season = params$season, from_marginals = TRUE)
  route_stats <- rts_stats(rts)

  # Perform PIT evaluation of tracking data if there is any
  # Note: transitions_files hold the tracking data
  transitions_files <- list.files(the$tracking_data_path,
                               pattern = paste0("^", params$species, ".*transitions.*\\.csv"),
                               full.names = TRUE)
  if (length(transitions_files) > 0){
    transitions_df_list <- lapply(transitions_files, utils::read.csv)
    indiv_id_list <- lapply(transitions_df_list, function(i){unique(i$indiv)})
    # throw error if all the indiv ids are not unique across the different data.frames
    stopifnot(
      length(unique(Reduce(c, indiv_id_list))) == sum(sapply(indiv_id_list, function(i){length(unique(i))}))
    )
    # Combine potentially multiple tracking data.frames
    transitions <- as.data.frame(data.table::rbindlist(transitions_df_list, fill = TRUE))
    # Do PIT calculations
    pit_calibration_obj <- pit_calibration(bf, transitions, params)
    pit_plots(pit_calibration_obj, params, modelname)
  } else {
    # NA's for pit calibration if no tracking data 
    pit_calibration_obj <- list(D_row = NA_real_, D_col = NA_real_, res = data.frame(in_95_set = NA_real_))
  }
  # Write PIT calibration (on tracking data)
  dir.create(file.path(params$output_path, 'pit_data'), showWarnings = FALSE)
  pit_data_filename <- paste0(sub('\\.hdf5$', "", modelname), '_pit.rds')
  outfile <- file.path(file.path(params$output_path, 'pit_data'), pit_data_filename)
  saveRDS(pit_calibration_obj, outfile)
  
  # Combine all the metrics
  out_df <- dplyr::tibble(
    model = modelname,
    obs_weight = safe_numeric(bf$metadata$hyperparameters$obs_weight),
    ent_weight = safe_numeric(bf$metadata$hyperparameters$ent_weight),
    dist_weight = safe_numeric(bf$metadata$hyperparameters$dist_weight),
    dist_pow = safe_numeric(bf$metadata$hyperparameters$dist_pow),
    de_ratio = safe_numeric(signif(bf$metadata$hyperparameters$dist_weight / bf$metadata$hyperparameters$ent_weight, 3)),
    obs_prop = safe_numeric(signif(1 / (1 + bf$metadata$hyperparameters$dist_weight + bf$metadata$hyperparameters$ent_weight), 4)),
    end_traverse_cor = BirdFlowR::distribution_performance(bf, metrics = 'md_traverse_cor', season = params$season)$md_traverse_cor,
    ll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$log_likelihood, na.rm = TRUE), NA_real_),
    nll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$null_ll, na.rm = TRUE), NA_real_),
    ll_raw_n = nrow(my_ll),
    ll_n = length(stats::na.omit(my_ll$log_likelihood)),
    straightness = route_stats$straightness,
    length = route_stats$length,
    displacement = route_stats$displacement,
    n_stopovers = route_stats$n_stopovers,
    pit_row = pit_calibration_obj$D_row,
    pit_col = pit_calibration_obj$D_col,
    pit_in_95 = sum(pit_calibration_obj$res$in_95_set,na.rm=T)/length(pit_calibration_obj$res$in_95_set)
  )
  
  # Return list this is usually assigned to "my_ll"
  # components represnt, metrics, banding observations, and banding intervals
  list(df = out_df, obs = track_info$obs_df, int = track_info$int_df)
}

#' 3d plot function
#' @param color_column column to use for the colors
#' @param suffix suffix to use for output filenames
#' @param eval_metrics data.frame produced from rbindlist-ing output from [batch_evaluate_models()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [batch_evaluate_models()], [evaluate_model()], [preprocess_species_wrapper()]
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
  eval_metrics$color_cor <- cor_colors[cut(eval_metrics$end_traverse_cor, breaks = cor_breaks)]

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
#' @returns a list of mean summary statistics
#' @seealso [BirdFlowR::route()]
#' @export
#' 
rts_stats <- function(rts){
  
  if(inherits(rts, "data.frame")){
    # Continue to support older route format
    data <- rts
  } else {  
    # BirdFlowRoutes object
    data <- rts$data
  }
  
  rts_lst <- split(data, data$route_id)
  out <- lapply(rts_lst, function(rts){
    rts$ts_sequential <- seq_len(nrow(rts))
    traj <- trajr::TrajFromCoords(rts, xCol = 'x', yCol = 'y', timeCol = 'ts_sequential', timeUnits = 'ts')
    list(
      straightness = trajr::TrajStraightness(traj),
      length = trajr::TrajLength(traj)/1000,
      displacement = trajr::TrajDistance(traj)/1000,
      n_stopovers = max(sum(trajr::TrajStepLengths(traj) > 0) - 1, 0)
    )
  }) %>% (data.table::rbindlist) %>% colMeans(na.rm = TRUE) %>% as.list
  out
}

#' PCA biplot hyperparameters evaluation
#' @param eval_metrics data.frame produced from rbindlist-ing output from [batch_evaluate_models()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [batch_evaluate_models()], [preprocess_species_wrapper()]
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
    'end_traverse_cor',
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
  
  plot2 <- ggplot2::autoplot(fit, color = 'end_traverse_cor',
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
#' @param df a `eval_metrics` produced by [batch_evaluate_models()]
#' @param dir hdf5 directory from `params` list.  Need to refactor some of these arguments
#' @seealso [BirdFlowR::lookup_season_timesteps()], [batch_evaluate_models()]
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
#' recombining results from [batch_evaluate_models()]
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
        etc_d = desirability2::d_max(.data$end_traverse_cor, use_data = TRUE)
      )
  } else if (params$model_selection == 'pit_etc'){
    # PIT metrics and end traverse correlation only
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
        d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
        d_pit_in_95 = desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE),
        pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit"))),
        etc_d = desirability2::d_max(.data$end_traverse_cor, use_data = TRUE)
      )
  } else if (params$model_selection == 'real_tracking'){
    # Tracking-focused model selection
    # Includes traverse correlation, PIT scores, and
    # straightness + n_stopovers targetted to observed values from real tracking
    real_track_stats <- real_track_stats(eval_metrics, params)
    # save real_track_stats RDS
    saveRDS(real_track_stats, file.path(params$output_path, 'real_track_stats.rds'))
    stopifnot(
      !is.na(real_track_stats$straightness),
      !is.na(real_track_stats$n_stopovers)
    )
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
        d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
        d_pit_in_95 = desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE),
        pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit"))),
        etc_d = desirability2::d_max(.data$end_traverse_cor, use_data = TRUE),
        str_d = desirability2::d_min(abs(.data$straightness - real_track_stats$straightness), use_data = TRUE),
        nso_d = desirability2::d_min(abs(.data$n_stopovers - real_track_stats$n_stopovers), use_data = TRUE)
      )
  } else if (params$model_selection == 'real_tracking_no_cal'){
    # Tracking-focused model selection
    # Includes traverse correlation, and
    # straightness + n_stopovers targetted to observed values from real tracking
    # But no PIT scores!
    real_track_stats <- real_track_stats(eval_metrics, params)
    # save real_track_stats RDS
    saveRDS(real_track_stats, file.path(params$output_path, 'real_track_stats.rds'))
    stopifnot(
      !is.na(real_track_stats$straightness),
      !is.na(real_track_stats$n_stopovers)
    )
    eval_metrics <- eval_metrics %>%
      dplyr::mutate(
        etc_d = desirability2::d_max(.data$end_traverse_cor, use_data = TRUE),
        str_d = desirability2::d_min(abs(.data$straightness - real_track_stats$straightness), use_data = TRUE),
        nso_d = desirability2::d_min(abs(.data$n_stopovers - real_track_stats$n_stopovers), use_data = TRUE)
      )
  } else if (params$model_selection == 'averaged_parameters') {
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
  #
  # # old code
  # eval_metrics %>%
  #   # create new desirability columns
  #   ## Previous desirability:  Used just end traverse correlation and straigthness like this, but models were underdispersed
  #   # etc_d = desirability2::d_max(.data$end_traverse_cor, low = 0.9, use_data = TRUE)
  #   # str_d = desirability2::d_max(.data$straightness, low = 0.5, use_data = TRUE)
  #   dplyr::mutate(
  #     # etc_d = desirability2::d_max(.data$end_traverse_cor, low = 0.9, use_data = TRUE),
  #     str_d = desirability2::d_target(.data$straightness, low = 0.5, target = 0.85, high = 1, use_data = TRUE),
  #     #nso_d = desirability2::d_target(.data$n_stopovers, target = 3.54), ### CHECK ARGS ###
  #     #str_d = desirability2::d_target(straightness, low = 0.5, target = 0.85, high = 1),
  #     #ll_d  = desirability2::d_max(ll, use_data = TRUE),
  #     #str_d = desirability2::d_target(straightness, target = 0.85, low = 0.5, high = 1, scale_low = 1/2, scale_high = 1/2),
  #     #dsp_d = desirability2::d_max(displacement, low = 0.75 * max(displacement), high = max(displacement)),
  #     #str_d = desirability2::d_max(.data$straightness, low = 0.5, use_data = TRUE),
  #     #etc_d = desirability2::d_max(.data$end_traverse_cor, low = 0.9, use_data = TRUE),
  #     d_pit_row = desirability2::d_min(.data$pit_row, use_data = TRUE),
  #     d_pit_col = desirability2::d_min(.data$pit_col, use_data = TRUE),
  #     d_pit_in_95 = desirability2::d_min(abs(.data$pit_in_95 - 0.95), use_data = TRUE),
  #     pit_d = desirability2::d_overall(dplyr::across(dplyr::starts_with("d_pit"))),
  #     etc_d = desirability2::d_max(.data$end_traverse_cor, use_data = TRUE),
  #     overall_des = desirability2::d_overall(dplyr::across(dplyr::ends_with("_d")))
  #   )
}
