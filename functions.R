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
    uncount(count) %>%
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