### AMEWOO Tracking ###

min_dist_m <- 0
max_days = 180

tracking_df <- fread('amwo/amewoo_USGS_Movebank_st2019_weekly_tracks-100.csv')
tracking_df <- tracking_df %>% select(indiv_only, timestamp, lon, lat) %>% arrange(indiv_only, timestamp)
tracking_df$timestamp <- as.Date(tracking_df$timestamp)
tracking_df <- tracking_df %>% rename(BAND = indiv_only,
                                      EVENT_DATE = timestamp,
                                      LON_DD = lon,
                                      LAT_DD = lat)
# picking up here inside the make_tracks function...
tracking_df <- tracking_df %>% group_by(BAND) %>%
  mutate(count = c(1, rep(2, n() - 2), 1)) %>%
  tidyr::uncount(count) %>%
  mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
  ungroup
df <- tracking_df
df <- preprocess_calc_distance_days(df)
df <- df %>% select(BAND, EVENT_DATE, LAT_DD, LON_DD, BAND_TRACK, distance, days)
df <- df %>% group_by(BAND_TRACK) %>%
  (dplyr::filter)(distance[2] > min_dist_m / 1000) %>%
  (dplyr::filter)(days[2] <= max_days) %>%
  mutate(when = c('from', 'to')) %>%
  ungroup
df$id <- seq_len(nrow(df))
obs_df <- df %>% rename(date = EVENT_DATE, lat = LAT_DD, lon = LON_DD)
int_df <- df %>% select(BAND_TRACK, when, id)
int_df <- pivot_wider(int_df, id_cols = BAND_TRACK, names_from = when, values_from = id)
track_info <- list(obs_df = obs_df, int_df = int_df)
