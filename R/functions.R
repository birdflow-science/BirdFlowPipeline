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
  df <- df %>% dplyr::select(.data$BAND, .data$EVENT_TYPE, .data$EVENT_DATE, .data$LAT_DD, .data$LON_DD, .data$EBIRDST_CODE, .data$BAND_TRACK, .data$distance, .data$days)
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
  obs_df <- df %>% dplyr::rename(date = .data$EVENT_DATE, lat = .data$LAT_DD, lon = .data$LON_DD)
  int_df <- df %>% dplyr::select(.data$BAND_TRACK, .data$when, .data$id)
  if (nrow(int_df) > 0){
    # typical behavior if data is present
    int_df <- tidyr::pivot_wider(int_df, id_cols = .data$BAND_TRACK, names_from = .data$when, values_from = .data$id)
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
#' @param bf A birdflow model object, often passed from [import_birdflow_and_evaluate()]
#' @param modelname A name for the model, often the basename of the model path
#' @param track_info Object produced from [make_tracks()]
#'
#' @seealso [make_tracks()], [batch_evaluate_models()], [import_birdflow_and_evaluate()]
#' @returns A list:
#'  * `df` 1-row data.frame of model descriptors and metrics 
#'  * `obs` obs_df portion from original `track_info`
#'  * `int` = int_df portion from original `track_info`
#'
#' @export
evaluate_model <- function(bf, modelname, track_info){
  if (nrow(track_info$int_df) == 0){
    # return zero-row likelihood data.frame
    # can remove this once interval_log_likelihood handles this internally
    my_ll <- structure(list(BAND_TRACK = character(0), from = integer(0), 
                            to = integer(0), log_likelihood = numeric(0), null_ll = numeric(0), 
                            lag = numeric(0), exclude = logical(0), not_active = logical(0), 
                            dynamic_mask = logical(0), sparse = logical(0), same_timestep = logical(0), 
                            bad_date = logical(0)), row.names = integer(0), class = "data.frame")
  } else {
    # proceed with likelihood calculation
    my_ll <- BirdFlowR::interval_log_likelihood(
      intervals = as.data.frame(track_info$int_df),
      observations = as.data.frame(track_info$obs_df),
      bf = bf)
  }
  rts <- BirdFlowR::route_migration(bf, 100, 'prebreeding')
  route_stats <- rts_stats(rts)
  out_df <- dplyr::tibble(
    model = modelname,
    obs_weight = safe_numeric(bf$metadata$hyperparameters$obs_weight),
    ent_weight = safe_numeric(bf$metadata$hyperparameters$ent_weight),
    dist_weight = safe_numeric(bf$metadata$hyperparameters$dist_weight),
    dist_pow = safe_numeric(bf$metadata$hyperparameters$dist_pow),
    de_ratio = safe_numeric(signif(bf$metadata$hyperparameters$dist_weight / bf$metadata$hyperparameters$ent_weight, 3)),
    obs_prop = safe_numeric(signif(1 / (1 + bf$metadata$hyperparameters$dist_weight + bf$metadata$hyperparameters$ent_weight), 4)),
    mean_distr_cor = BirdFlowR::distribution_performance(bf, metrics = 'mean_distr_cor')$mean_distr_cor,
    end_traverse_cor = BirdFlowR::distribution_performance(bf, metrics = NULL, season = 'prebreeding')$md_traverse_cor,
    ll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$log_likelihood, na.rm = TRUE), NA_real_),
    nll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$null_ll, na.rm = TRUE), NA_real_),
    ll_raw_n = nrow(my_ll),
    ll_n = length(stats::na.omit(my_ll$log_likelihood)),
    straightness = route_stats$straightness,
    sinuosity = route_stats$sinuosity,
    length = route_stats$length,
    displacement = route_stats$displacement
  )
  #my_ll
  list(df = out_df, obs = track_info$obs_df, int = track_info$int_df)
}

#' 3d plot function
#' @param color_column column to use for the colors
#' @param suffix suffix to use for output filenames
#' @param ll_df data.frame produced from rbindlist-ing output from [batch_evaluate_models()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [batch_evaluate_models()], [evaluate_model()], [preprocess_species_wrapper()]
#' @returns side-effect is a plot file created
#' @export
make_3d_plot <- function(color_column, suffix, ll_df, params){
  ## color_column can be color_ll, color_nll, or color_cor

  # Set the plot colors
  ll_df$color_ll <- grDevices::hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]
  ll_df <- ll_df %>% dplyr::mutate(color_nll = dplyr::if_else(.data$ll < .data$nll, '#ffffff', .data$color_ll))
  cor_breaks <- c(-Inf, 0.9, 0.95, 0.975, Inf)
  cor_labels <- c("< 0.9", "0.9 to <0.95", "0.95 to <0.975", ">= 0.975")
  cor_colors <- c('#FFFFFF', grDevices::hcl.colors(3, rev = TRUE))
  ll_df$color_cor <- cor_colors[cut(ll_df$mean_distr_cor, breaks = cor_breaks)]

  rgl::plot3d(
    x = ll_df$ent_weight, y = ll_df$dist_weight, z = ll_df$dist_pow, 
    col = ll_df[[color_column]], 
    type = 's', 
    radius = .02,
    xlab="ent_weight", ylab="dist_weight", zlab="dist_pow")
  # To display in an R Markdown document:
  # rglwidget()
  # 
  # # To save to a file:
  htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                          file = file.path(params$output_path, paste0(params$my_species, "_", params$my_res, "km_3dscatter_", suffix, ".html")),
                          libdir = "libs",
                          selfcontained = TRUE
  )
}

#' Calculate route summary statistics
#' @param rts data.frame object output from `BirdFlowR::route()` or `BirdFlowR::route_migration()`
#' @returns a list of mean summary statistics
#' @seealso [BirdFlowR::route()], [BirdFlowR::route_migration()]
#' @export
#' 
rts_stats <- function(rts){
  rts_lst <- split(rts$points, rts$points$route)
  out <- lapply(rts_lst, function(rts){
    rts$ts_sequential <- seq_len(nrow(rts))
    traj <- trajr::TrajFromCoords(rts, xCol = 'x', yCol = 'y', timeCol = 'ts_sequential', timeUnits = 'ts')
    list(
      straightness = trajr::TrajStraightness(traj),
      sinuosity = trajr::TrajSinuosity2(traj),
      length = trajr::TrajLength(traj)/1000,
      displacement = trajr::TrajDistance(traj)/1000
    )
  }) %>% (data.table::rbindlist) %>% colMeans(na.rm = TRUE) %>% as.list
  out
}

#' PCA biplot hyperparameters evaluation
#' @param ll_df data.frame produced from rbindlist-ing output from [batch_evaluate_models()]
#' @param params the standard params list object, see [preprocess_species_wrapper()]
#' @seealso [batch_evaluate_models()], [preprocess_species_wrapper()]
#' @returns side effect is a plot written to file
#' @export
model_evaluation_biplot <- function(ll_df, params){
  # workaround for unexported method ggfortify:::autoplot.princomp()
  requireNamespace('ggfortify', quietly = TRUE)
  outfile <- file.path(params$output_path, 'pca_evaluation.pdf')
  if (length(unique(ll_df$ll)) == 1) {ll_df$ll <- NULL}
  pca_columns <- c(
    'ent_weight',
    'dist_weight',
    'dist_pow',
    'mean_distr_cor',
    'll',
    'straightness',
    'sinuosity',
    'length',
    'displacement'
  )
  pca_columns <- pca_columns[pca_columns %in% names(ll_df)]
  fit <- stats::princomp(ll_df[,pca_columns], cor = TRUE)
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
  
  plot2 <- ggplot2::autoplot(fit, color = 'mean_distr_cor',
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
    gridExtra::grid.arrange(plot1, plot2, top = substr(ll_df$model[1], 1, 6), ncol=2)
    })
  dev.off()
}

#' Quick visualize a spring migration routes simulation from ll_df by row number
#' @param i row number in `df` from which to extract model
#' @param n number of routes to simulate for visualization
#' @param season season for which to simulate route, parseable by [BirdFlowR::lookup_season_timesteps()]
#' @param df a `ll_df` produced by [batch_evaluate_models()]
#' @param dir hdf5 directory from `params` list.  Need to refactor some of these arguments
#' @seealso [BirdFlowR::lookup_season_timesteps()], [batch_evaluate_models()]
#' @returns side effect is an interactive plot
#' @export
quick_visualize_routes <- function(i, n = 10, season = 'prebreeding', df = ll_df, dir = hdf_dir){
  ll_df <- df
  hdf_dir <- dir
  bf <- BirdFlowR::import_birdflow(file.path(hdf_dir, ll_df$model[i]))
  # 
  # ## Plot map route_migration spring msap
  # 
  rts <- BirdFlowR::route_migration(bf, n = n, migration = season)
  print(
    {
      BirdFlowR::plot_routes(rts, bf, use_seasonal_colors = FALSE) +
        ggplot2::labs(title = ll_df$model[i])
    }
  )
}
