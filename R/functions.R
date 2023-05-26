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
  df <- filter(df, !is.na(EVENT_DATE))
  # exclude hand-reared, experimental, transported, rehabbed, held, sick, dead
  df <- filter(df, ! BIRD_STATUS %in% c(2, 4, 5, 6, 7, 8, 9))
  # exclude state or country level locations
  df <- filter(df, ! CP %in% c(12, 72))
  # exclude records with longitude of 0 (might be real records with 0 latitude)
  df <- filter(df, LON_DD != 0)
  # exclude records with missing latitude or longitude
  df <- filter(df, !is.na(LON_DD) & !is.na(LAT_DD))
  df
}

preprocess_with_recovery <- function(df){
  # delete exluded records
  df <- df %>% group_by(SPECIES_ID, BAND) %>% filter(n() >= 2) %>% ungroup
  df
}

preprocess_sort_band_date <- function(df){
  df %>% arrange(BAND, EVENT_DATE)
}

preprocess_calc_distance_days <- function(df){
  df %>%
    group_by(BAND) %>%
    mutate(distance = geodist::geodist(data.frame(lon = LON_DD, lat = LAT_DD),
                                       sequential = TRUE, measure = 'geodesic', pad = TRUE) / 1000) %>%
    mutate(days = as.integer(EVENT_DATE - lag(EVENT_DATE))) %>%
    ungroup
}

# Version for preparing for log likelihood calcs #
#' @export
make_tracks <- function(
    banding_rds_path,
    max_band_tracks = 10000,
    max_preprocess = 500000,
    min_dist_m = 15000,
    max_days = 180){
  file_exists <- if_else(file.exists(banding_rds_path), TRUE, FALSE)
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
  sampled_bands <- sample(unique(df$BAND), if_else(max_preprocess > n_distinct(df$BAND), n_distinct(df$BAND), max_preprocess))
  df <- df %>% filter(BAND %in% sampled_bands)
  # Function to convert banding df to an sf object of linestrings of origin-destination tracks
  # expand to two steps
  df <- df %>% group_by(BAND) %>%
    mutate(count = c(1, rep(2, n() - 2), 1)) %>%
    tidyr::uncount(count) %>%
    mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
    ungroup
  df <- preprocess_calc_distance_days(df)
  df <- df %>% select(BAND, EVENT_TYPE, EVENT_DATE, LAT_DD, LON_DD, EBIRDST_CODE, BAND_TRACK, distance, days)
  df <- df %>% group_by(BAND_TRACK) %>%
    filter(distance[2] > min_dist_m / 1000) %>%
    filter(days[2] <= max_days) %>%
    mutate(
      when = rep(
        c('from', 'to'), times = n() / 2
      )
    ) %>%
    ungroup
  # Downsample to max # band_tracks
  sampled_band_tracks <- sample(unique(df$BAND_TRACK), if_else(max_band_tracks > n_distinct(df$BAND_TRACK), n_distinct(df$BAND_TRACK), max_band_tracks))
  df <- df %>% filter(BAND_TRACK %in% sampled_band_tracks)
  # Make IDs
  df$id <- seq_len(nrow(df))
  obs_df <- df %>% rename(date = EVENT_DATE, lat = LAT_DD, lon = LON_DD)
  int_df <- df %>% select(BAND_TRACK, when, id)
  if (nrow(int_df) > 0){
    # typical behavior if data is present
    int_df <- tidyr::pivot_wider(int_df, id_cols = BAND_TRACK, names_from = when, values_from = id)
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

inspect_flagged_tracks_sf <- function(track_info, my_ll, true_column){
  my_ll <- as_tibble(my_ll)
  df <- left_join(my_ll, track_info$obs_df, by = 'BAND_TRACK') %>%
    filter(.data[[true_column]])
  df <- df %>%
    group_by(BAND_TRACK) %>%
    summarise(start_date = date[1],
              stop_date = date[2],
              geom = sprintf("LINESTRING(%s %s, %s %s)",
                             lon[1], lat[1], lon[2], lat[2])
    ) %>% ungroup
  df <- st_as_sf(df, wkt = "geom", crs = 'wgs84')
  plot(get_coastline(df))
  plot(df %>% select(BAND_TRACK), add = TRUE)
}

# season is 'prebreeding', 'postbreeding', 'all'
# intervals outside that season will get an NA for log likelihood
do_ll <- function(path, season){
  bf <- import_birdflow(path)
  bf <- sparsify(bf, method = "state")
  species_code <- BirdFlowR::species_info(bf)$species_code
  banding_df <- readRDS(file.path('rds', paste0(species_code, '.rds')))
  # subsampling banding data down to 5000 if larger
  unique_bands <- unique(banding_df$BAND)
  banding_df <- dplyr::filter(banding_df, BAND %in% sample(unique_bands, min(5000, length(unique_bands))))
  track_info <- make_tracks(banding_df)
  my_ll <- BirdFlowR::interval_log_likelihood(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf,
    season = season)
  my_ll
  list(model = basename(path), obs = track_info$obs_df, int = track_info$int_df, ll = as_tibble(my_ll))
}

# evaluate model tracks
#' @export
evaluate_model <- function(path, track_info){
  bf <- BirdFlowR::import_birdflow(path)
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
    model = basename(path),
    obs_weight = bf$metadata$hyperparameters$obs_weight,
    ent_weight = bf$metadata$hyperparameters$ent_weight,
    dist_weight = bf$metadata$hyperparameters$dist_weight,
    dist_pow = bf$metadata$hyperparameters$dist_pow,
    de_ratio = signif(bf$metadata$hyperparameters$dist_weight / bf$metadata$hyperparameters$ent_weight, 3),
    obs_prop = signif(1 / (1 + bf$metadata$hyperparameters$dist_weight + bf$metadata$hyperparameters$ent_weight), 4),
    mean_distr_cor = BirdFlowR::evaluate_performance(bf)$mean_distr_cor,
    start_cor = evaluate_performance_route(bf, season = 'prebreeding')$start_cor,
    end_traverse_cor = evaluate_performance_route(bf, season = 'prebreeding')$end_traverse_cor,
    ll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$log_likelihood, na.rm = TRUE), NA_real_),
    nll = dplyr::if_else(nrow(my_ll) > 0, sum(my_ll$null_ll, na.rm = TRUE), NA_real_),
    ll_raw_n = nrow(my_ll),
    ll_n = length(na.omit(my_ll$log_likelihood)),
    straightness = route_stats$straightness,
    sinuosity = route_stats$sinuosity,
    length = route_stats$length,
    displacement = route_stats$displacement
  )
  #my_ll
  list(df = out_df, obs = track_info$obs_df, int = track_info$int_df)
}

# 3d plot function
#' @export
make_3d_plot <- function(color_column, suffix, ll_df, params){
  ## color_column can be color_ll, color_nll, or color_cor

  # Set the plot colors
  ll_df$color_ll <- hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]
  ll_df <- ll_df %>% mutate(color_nll = if_else(ll < nll, '#ffffff', color_ll))
  cor_breaks <- c(-Inf, 0.9, 0.95, 0.975, Inf)
  cor_labels <- c("< 0.9", "0.9 to <0.95", "0.95 to <0.975", ">= 0.975")
  cor_colors <- c('#FFFFFF', hcl.colors(3, rev = TRUE))
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

# make pdf of spring route_migration for a model hdf5 file

spring_migration_pdf <- function(filename, hdf_dir){
  bf <- import_birdflow(file.path(hdf_dir, filename))
  rts <- route_migration(bf, 10, 'prebreeding')
  pdf(file.path('output', 'maps', paste0(filename, '.pdf')))
  print({
    plot_routes(rts, bf, use_seasonal_colors = FALSE) +
      labs(title = filename)
  })
  dev.off()
}

# get route summary information (from the output of route_migration)
#' @export
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
  }) %>% rbindlist %>% colMeans(na.rm = TRUE) %>% as.list
  out$length <- sf::st_length(rts$lines$geometry) %>% as.numeric %>% `/`(1000) %>% mean(na.rm = TRUE)
  out
}

# PCA biplot hyperparameters evaluation
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
  fit <- princomp(ll_df[,pca_columns], cor = TRUE)
  #biplot(fit)
  pdf(outfile, 13, 5.5)
  #print(factoextra::fviz_pca_biplot(fit, title = paste0('PCA biplot: ', substr(ll_df$model[1], 1, 6))))
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

# Quick visualize by model number
# Plot map route_migration spring map
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

# Evaluate how well the STS marginal distribution correlates with ebirdST distribution at the season start point
# AND
# Evaluate how well the ebirdST end date distribution correlates with the forwarded-projected distribution when starting with the marginals
#bf <- import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf/amewoo_58km_NEW/amewoo_2021_58km_obs1.0_ent0.00478_dist0.0478_pow0.7.hdf5')
#' @export
evaluate_performance_route <- function (x, season = 'all') 
{
  if (!BirdFlowR::has_dynamic_mask(x))
    x <- BirdFlowR::add_dynamic_mask(x)
  season_timesteps <- BirdFlowR::lookup_season_timesteps(x, season)
  start <- season_timesteps[1]
  end <- tail(season_timesteps, 1)
  
  start_distr_ebirdst <- BirdFlowR::get_distr(x, start, from_marginals = FALSE)
  start_distr_marginals <- BirdFlowR::get_distr(x, start, from_marginals = TRUE)
  start_dm <- BirdFlowR::get_dynamic_mask(x, start)
  start_cor <- cor(start_distr_ebirdst[start_dm], start_distr_marginals[start_dm])
  end_distr_ebirdst <- BirdFlowR::get_distr(x, end, from_marginals = FALSE)
  projected <- predict(x, distr = start_distr_marginals, start = start,
                       end = end, direction = "forward")
  end_dm <- BirdFlowR::get_dynamic_mask(x, end)
  end_traverse_cor <- cor(end_distr_ebirdst[end_dm], projected[end_dm, ncol(projected)])
  # get_distr(bf, start, TRUE) |> rasterize_distr(bf) |> terra::plot()
  # get_distr(bf, start, FALSE) |> rasterize_distr(bf) |> terra::plot()
  # get_distr(bf, end, TRUE) |> rasterize_distr(bf) |> terra::plot()
  # get_distr(bf, end, FALSE) |> rasterize_distr(bf) |> terra::plot()
  list(start_cor = start_cor,
       end_traverse_cor = end_traverse_cor)
}
#evaluate_performance(bf)
#evaluate_performance_route(bf, 'prebreeding')


# function to get centroid displacement in km from start to end of a season
get_season_centroid_displacement <- function(bf, season = 'all'){
  my_ts <- lookup_season_timesteps(bf, season)
  my_start_step <- my_ts[1]
  my_end_step   <- tail(my_ts, 1)
  my_start_distr <- get_distr(bf, which = my_end_step,   from_marginals = FALSE)
  my_end_distr <- get_distr(bf, which = my_start_step, from_marginals = FALSE)
  df <- tibble(
    time = rep(
      c('start', 'end'),
      each = length(my_start_distr)
    ),
    i = rep(seq_along(my_start_distr), 2),
    value = c(
      my_start_distr,
      my_end_distr
    )
  )
  df <- df %>%
    mutate(x = i_to_x(i, bf),
           y = i_to_y(i, bf))
  centroids <- df %>%
    group_by(time) %>%
    summarise(x = mean(x * value),
              y = mean(y * value))
  centroids %>% select(-time) %>% stats::dist() %>% as.numeric
}
