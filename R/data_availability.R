# my_packages <- c('dplyr', 'data.table', 'geodist', 'tidyr', 'batchtools')
# for (i in my_packages){
#   library(i, character.only = TRUE)
# }
# source(file.path('R', 'batch_functions.R'))

# Batch data availability info function
banding_data_availability <- function(file){
  tax_join <- fread(file.path('tax', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)
  df <- readRDS(file)
  species_code <- basename(file) %>% sub('\\.rds$', '', .)
  species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`
  message(species_name)
  max_track_days_options <- c(Inf, 180)
  min_distance_km_options <- c(0, 0.001, 15)
  attrition_df <- data.frame(species_code = species_code,
                             species_name = species_name,
                             max_track_days= rep(max_track_days_options, each = 3),
                             min_distance_km = rep(min_distance_km_options, 2),
                             n_tracks = rep(NA_integer_, 6))
  df <- preprocess_calc_distance_days(df)
  df <- drop_na(df, distance, days)
  df <- dplyr::filter(df, days > 0)
  # stats for all durations
  for (da in max_track_days_options){
    for (di in min_distance_km_options){
      attrition_df[attrition_df$max_track_days == da & attrition_df$min_distance_km == di, 'n_tracks'] <- 
        dplyr::filter(df, days <= da & distance >= di) %>% nrow
    }
  }
  # utils::write.csv(attrition_df, file.path('output','attrition.csv'), row.names = FALSE)
  # if (n_day180 < 20){
  #   next
  # }
  # plot(df$days, df$distance, xlab = 'days elapsed', ylab = 'distance (km)',
  #      main = paste0(species_name, '\n', '(n = ', n_day180, ')'))
  attrition_df
}

# Batch data availability function

batch_data_availability <- function() {
rds_files <- list.files('rds', full.names = TRUE)
my_suffix <- 'da'
batchtools::batchMap(banding_data_availability,
         rds_files,
         reg = batchtools::makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                            conf.file = system.file('batchtools.conf.R', package = 'banding'),
                            packages = my_packages,
                            source = file.path('R', 'functions.R'),
         ))
batchtools::submitJobs(mutate(batchtools::findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 30,
                            memory = 4))
batchtools::waitForJobs()
results_df <- lapply(batchtools::findJobs()$job.id, batchtools::loadResult) %>% rbindlist
utils::write.csv(results_df, 'data_availability.csv', row.names = FALSE)

hindex <- function(x) {
  tx <- sort(x, decreasing = T)
  sum(tx >= seq_along(tx))
}

results_df %>%
  group_by(max_track_days, min_distance_km) %>%
  summarise(n_species_n_tracks_h_index = hindex(n_tracks), .groups = 'drop')
}
