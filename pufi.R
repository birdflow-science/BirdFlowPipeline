library(BirdFlowR)
library(terra)
library(dplyr)
library(data.table)
library(tidyr)

dir  <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
file <- 'purfin_2021_89km_obs20.0_ent0.006_dist0.005_pow0.6.hdf5'

path <- file.path(dir, file)
bf <- import_birdflow(path)
bf <- sparsify(bf, method = "state")

plot(rast(bf, 1))

file <- 'rds/purfin.rds'
df <- readRDS(file)
species_code <- basename(file) %>% sub('\\.rds$', '', .)
tax_join <- fread(file.path('tax', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)
species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`

preprocess_calc_distance_days <- function(df){
  df %>%
    group_by(BAND) %>%
    mutate(distance = geodist::geodist(data.frame(lon = LON_DD, lat = LAT_DD),
                                       sequential = TRUE, measure = 'geodesic', pad = TRUE) / 1000) %>%
    mutate(days = as.integer(EVENT_DATE - lag(EVENT_DATE))) %>%
    ungroup
}

######
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
  df <- preprocess_calc_distance_days(df)
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

track_info <- make_tracks2(df)
my_ll <- BirdFlowR::interval_log_likelihood(
  intervals = as.data.frame(track_info$int_df),
  observations = as.data.frame(track_info$obs_df),
  bf = bf)
my_colsums <- colSums(my_ll[,c('exclude', 'not_active', 'dynamic_mask', 'sparse', 'same_timestep', 'bad_date')])
my_colsums
round(my_colsums / nrow(my_ll), digits = 2)
nrow(my_ll)

my_ll <- as_tibble(my_ll)
left_join(my_ll, track_info$obs_df, by = 'BAND_TRACK') %>%
  filter(dynamic_mask)
