#' Batch data availability info function
#'
#' @param file Path to RDS file to check data availability
#' @return data.frame of attrition info
#' @export
banding_data_availability <- function(file){
  tax_join <- data.table::fread(file.path('data-raw', 'bandng_taxonomy', 'eBird_Taxonomy_v2021.csv')) %>%
    dplyr::select(.data$SPECIES_CODE, .data$PRIMARY_COM_NAME) %>%
    dplyr::distinct()
  df <- readRDS(file)
  species_code <- sub('\\.rds$', '', basename(file))
  species_name <- dplyr::filter(tax_join, .data$SPECIES_CODE == species_code) %>% dplyr::pull(.data$PRIMARY_COM_NAME)
  message(species_name)
  max_track_days_options <- c(Inf, 180)
  min_distance_km_options <- c(0, 0.001, 15)
  attrition_df <- data.frame(species_code = species_code,
                             species_name = species_name,
                             max_track_days= rep(max_track_days_options, each = 3),
                             min_distance_km = rep(min_distance_km_options, 2),
                             n_tracks = rep(NA_integer_, 6))
  df <- preprocess_calc_distance_days(df)
  df <- tidyr::drop_na(df, .data$distance, .data$days)
  df <- dplyr::filter(df, .data$days > 0)
  # stats for all durations
  for (da in max_track_days_options){
    for (di in min_distance_km_options){
      attrition_df[attrition_df$max_track_days == da & attrition_df$min_distance_km == di, 'n_tracks'] <- 
        dplyr::filter(df, .data$days <= da & .data$distance >= di) %>% nrow
    }
  }
  attrition_df
}

#' Batch data availability function
#'
#' @return Data.frame of data availability
#' @export
batch_data_availability <- function() {
  rds_files <- list.files('rds', full.names = TRUE)
  my_suffix <- 'da'
  batchtools::batchMap(banding_data_availability,
                       rds_files,
                       reg = batchtools::makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                                                      conf.file = system.file('batchtools.conf.R', package = 'banding')
                       ))
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = list(walltime = 30,
                                          memory = 4))
  batchtools::waitForJobs()
  results_df <- lapply(batchtools::findJobs()$job.id, batchtools::loadResult) %>% data.table::rbindlist
  utils::write.csv(results_df, 'data_availability.csv', row.names = FALSE)
  
  hindex <- function(x) {
    tx <- sort(x, decreasing = T)
    sum(tx >= seq_along(tx))
  }
  
  results_df %>%
    dplyr::group_by(.data$max_track_days, .data$min_distance_km) %>%
    dplyr::summarise(n_species_n_tracks_h_index = hindex(.data$n_tracks), .groups = 'drop')
}
