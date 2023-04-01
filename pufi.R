library(BirdFlowR)
library(terra)

dir  <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
file <- 'purfin_2021_89km_obs20.0_ent0.006_dist0.005_pow0.6.hdf5'

path <- file.path(dir, file)
bf <- import_birdflow(path)
bf <- sparsify(bf, method = "state")

plot(rast(bf, 1))

file <- 'rds/purfin.rds'
df <- readRDS(file)
species_code <- basename(file) %>% sub('\\.rds$', '', .)
species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`
df <- make_tracks(df)
df <- df %>% mutate(days = as.integer(stop_date - start_date)) %>% filter(days < 180)
df %>% st_geometry

## Need function that's like make_tracks, but produces the observations and
## intervals dfs fo rthe likeliehood function
starts = st_line_sample(df, sample = 0) %>% st_cast("POINT") %>%
  st_transform('wgs84') %>% st_coordinates %>% as.data.frame %>%
  rename(start_lon = X, start_lat = Y)
ends = st_line_sample(df, sample = 1) %>% st_cast("POINT") %>%
  st_transform('wgs84') %>% st_coordinates %>% as.data.frame %>%
  rename(stop_lon = X, stop_lat = Y)

df <- cbind(df, starts)
df <- cbind(df, ends)
df <- st_drop_geometry(df)

# this is basically intervals table, now pivot it longer to get observations table.