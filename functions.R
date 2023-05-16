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

make_tracks <- function(
    df,
    remove_identical = TRUE,
    crs_in = 'wgs84',
    crs_out = birdflow_crs,
    min_dist_m = 15000){
  # Function to convert banding df to an sf object of linestrings of origin-destination tracks
  # expand to two steps
  df <- df %>% group_by(BAND) %>%
    mutate(count = c(1, rep(2, n() - 2), 1)) %>%
    uncount(count) %>%
    mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
    ungroup
  # get rid of same start and stop coordinates (multipoint filter also does it)
  if (remove_identical){
    df <- df %>%
      group_by(BAND_TRACK) %>%
      filter(n_distinct(LON_DD) > 1 | n_distinct(LAT_DD) > 1) %>%
      ungroup
  }
  # summarise
  df <- df %>%
    group_by(BAND_TRACK) %>%
    summarise(start_date = min(EVENT_DATE),
              stop_date = max(EVENT_DATE),
              geom = sprintf("LINESTRING(%s %s, %s %s)",
                             LON_DD[1], LAT_DD[1], LON_DD[2], LAT_DD[2])
    ) %>% ungroup
  df <- st_as_sf(df, wkt = "geom", crs = crs_in) %>% st_transform(crs_out)
  # add and filter distances
  df$distance <- as.numeric(st_length(df))
  df <- df %>% filter(distance >= min_dist_m)
  df
}

# Version for preparing for log likelihood calcs #
make_tracks2 <- function(
    df,
    max_band_tracks = 10000,
    max_preprocess = 500000,
    min_dist_m = 15000,
    max_days = 180){
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
    mutate(when = c('from', 'to')) %>%
    ungroup
  # Downsample to max # band_tracks
  sampled_band_tracks <- sample(unique(df$BAND_TRACK), if_else(max_band_tracks > n_distinct(df$BAND_TRACK), n_distinct(df$BAND_TRACK), max_band_tracks))
  df <- df %>% filter(BAND_TRACK %in% sampled_band_tracks)
  # Make IDs
  df$id <- seq_len(nrow(df))
  obs_df <- df %>% rename(date = EVENT_DATE, lat = LAT_DD, lon = LON_DD)
  int_df <- df %>% select(BAND_TRACK, when, id)
  int_df <- pivot_wider(int_df, id_cols = BAND_TRACK, names_from = when, values_from = id)
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
  track_info <- make_tracks2(banding_df)
  my_ll <- BirdFlowR::interval_log_likelihood(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf,
    season = season)
  my_ll
  list(model = basename(path), obs = track_info$obs_df, int = track_info$int_df, ll = as_tibble(my_ll))
}

# evaluate model tracks
evaluate_model <- function(path, track_info){
  bf <- import_birdflow(path)
  my_ll <- BirdFlowR::interval_log_likelihood(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf)
  rts <- route_migration(bf, 100, 'prebreeding')
  route_stats <- rts_stats(rts)
  out_df <- tibble(
    model = basename(path),
    obs_weight = bf$metadata$hyperparameters$obs_weight,
    ent_weight = bf$metadata$hyperparameters$ent_weight,
    dist_weight = bf$metadata$hyperparameters$dist_weight,
    dist_pow = bf$metadata$hyperparameters$dist_pow,
    mean_distr_cor = BirdFlowR:::evaluate_performance(bf)$mean_distr_cor,
    ll = sum(my_ll$log_likelihood, na.rm = TRUE),
    nll = sum(my_ll$null_ll, na.rm = TRUE),
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

make_3d_plot <- function(color_column, suffix){
  plot3d( 
    x = ll_df$ent_weight, y = ll_df$dist_weight, z = ll_df$dist_pow, 
    col = ll_df[[color_column]], 
    type = 's', 
    radius = .02,
    xlab="ent_weight", ylab="dist_weight", zlab="dist_pow")
  # To display in an R Markdown document:
  # rglwidget()
  # 
  # # To save to a file:
  htmlwidgets::saveWidget(rglwidget(width = 520, height = 520),
                          file = file.path('output', output_folder, paste0(my_species, "_", my_res, "km_3dscatter_", suffix, ".html")),
                          libdir = "libs",
                          selfcontained = TRUE
  )
}

# make pdf of spring route_migration for a model hdf5 file

spring_migration_pdf <- function(filename, my_dir){
  bf <- import_birdflow(file.path(my_dir, filename))
  rts <- route_migration(bf, 10, 'prebreeding')
  pdf(file.path('output', 'maps', paste0(filename, '.pdf')))
  plot(get_coastline(bf))
  plot(rts$lines, add = TRUE)
  title(main = filename)
  dev.off()
}

# get route summary information (from the output of route_migration)

rts_stats <- function(rts){
  rts_lst <- split(rts$points, rts$points$route)
  out <- lapply(rts_lst, function(rts){
    rts$ts_sequential <- seq_len(nrow(rts))
    traj <- TrajFromCoords(rts, xCol = 'x', yCol = 'y', timeCol = 'ts_sequential', timeUnits = 'ts')
    list(
      straightness = TrajStraightness(traj),
      sinuosity = TrajSinuosity2(traj),
      length = TrajLength(traj)/1000,
      displacement = TrajDistance(traj)/1000
    )
  }) %>% rbindlist %>% colMeans(na.rm = TRUE) %>% as.list
  out$length <- sf::st_length(rts$lines$geometry) %>% as.numeric %>% `/`(1000) %>% mean(na.rm = TRUE)
  out
}

# PCA biplot hyperparameters evaluation
model_evaluation_biplot <- function(ll_df, outfile){
  fit <- princomp(
    ll_df[,
          c(
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
    ],
    cor = TRUE
  )
  #biplot(fit)
  pdf(outfile, 9,9)
  print(factoextra::fviz_pca_biplot(fit, title = paste0('PCA biplot: ', substr(ll_df$model[1], 1, 6))))
  dev.off()
}

# Quick visualize by model number
# Plot map route_migration spring map
quick_visualize_routes <- function(i){
  bf <- import_birdflow(file.path(my_dir, ll_df$model[i]))
  # 
  # ## Plot map route_migration spring msap
  # 
  rts <- route_migration(bf, 10, 'prebreeding')
  plot(get_coastline(bf))
  plot(rts$lines, add = TRUE)
  title(main = ll_df$model[i])
  print(ll_df[i,])
}
