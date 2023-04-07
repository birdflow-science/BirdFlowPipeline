preprocess_calc_distance_days <- function(df){
  df %>%
    group_by(BAND) %>%
    mutate(distance = geodist::geodist(data.frame(lon = LON_DD, lat = LAT_DD),
                                       sequential = TRUE, measure = 'geodesic', pad = TRUE) / 1000) %>%
    mutate(days = as.integer(EVENT_DATE - lag(EVENT_DATE))) %>%
    ungroup
}

preprocess_filter_days <- function(df, ndays){
  # something seems wrong with this filtering still, currently not using
  browser()
  df <- df %>% filter((days < {{ndays}}) | (lead(days) < {{ndays}}))
  df <- df %>% filter((days > 0) | (lead(days) > 0 ))
  df
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
preprocess_calc_distance_days2 <- function(df){
  df %>%
    group_by(BAND) %>%
    mutate(distance = geodist::geodist(data.frame(lon = LON_DD, lat = LAT_DD),
                                       sequential = TRUE, measure = 'geodesic', pad = TRUE) / 1000) %>%
    mutate(days = as.integer(EVENT_DATE - lag(EVENT_DATE))) %>%
    ungroup
}

# Version for preparing for log likelihood calcs #
make_tracks2 <- function(
    df,
    min_dist_m = 15000,
    max_days = 180){
  # Function to convert banding df to an sf object of linestrings of origin-destination tracks
  # expand to two steps
  df <- df %>% group_by(BAND) %>%
    mutate(count = c(1, rep(2, n() - 2), 1)) %>%
    tidyr::uncount(count) %>%
    mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
    ungroup
  df <- preprocess_calc_distance_days2(df)
  df <- df %>% select(BAND, EVENT_TYPE, EVENT_DATE, LAT_DD, LON_DD, EBIRDST_CODE, BAND_TRACK, distance, days)
  df <- df %>% group_by(BAND_TRACK) %>%
    filter(distance[2] > min_dist_m / 1000) %>%
    filter(days[2] <= max_days) %>%
    mutate(when = c('from', 'to')) %>%
    ungroup
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
  banding_df <- dplyr::filter(banding_df, BAND %in% sample(unique_bands, max(5000, length(unique_bands))))
  track_info <- make_tracks2(banding_df)
  my_ll <- interval_log_likelihood2(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf,
    season = season)
  my_ll
  list(model = basename(path), obs = track_info$obs_df, int = track_info$int_df, ll = as_tibble(my_ll))
}

# get params from beginning of batch_flow.R first
batch_likelihood <- function(dir, regex, params = params, season){
  files <- list.files(path = dir, pattern = regex, full.names = TRUE)
  # See ?Registry for more info on configuration files, e.g., always loading
  # certain packages or starting in certain working directories
  reg <- makeRegistry(params$pp_reg,
                      packages = c('data.table', 'dplyr', 'tidyr', 'BirdFlowR'),
                      source = c('functions.R', '~/BirdFlowR/R/interval_log_likelihood.R'))
  # saveRegistry()
  # ?setDefaultRegistry
  # not needed because once we make registry, it stays for session as reg
  reg$cluster.functions <- makeClusterFunctionsSlurm(template = '~/batchtools_proj/sbatch_preprocess_species.tmpl',
                                                     array.jobs = params$array,
                                                     nodename = params$login)
  batchMap(fun = do_ll,
           path = files,
           season = season)
  rez <- list(walltime = params$wt_pp, ncpus = params$ncpu_pp, memory = params$mem_pp * 1000, partition = params$part_pp)
  submitJobs(resources = rez)
  waitForJobs()
}

get_season_timesteps <- function(bf, season){
  stopifnot(!is.null(season), is.character(season), length(season) == 1)
  stopifnot(season %in% c("all", "prebreeding", "postbreeding"))
  full_model_start_step <- 1
  full_model_stop_step <- n_transitions(bf)
  if (season %in% c("prebreeding", "postbreeding")) {
    start_step <- lookup_timestep(bf$species[[paste0(season, "_migration_start")]], bf)
    stop_step <- lookup_timestep(bf$species[[paste0(season, "_migration_end")]], bf)
  }
  else {
    start_step <- full_model_start_step
    stop_step <- full_model_stop_step
  }
  if (start_step > stop_step) {
    selected_timesteps <- c(start_step:full_model_stop_step, full_model_start_step:stop_step)
  }
  else {
    selected_timesteps <- start_step:stop_step
  }
  selected_timesteps
}
