
# Function to take a set of raw files and produce per-species RDS
process_file_set <- function(collapse_list_item){
  crosswalk <- data.table::fread('tax/taxonomy_crosswalk.csv')
  df <- lapply(collapse_list_item,
               data.table::fread,
               colClasses = c(BIRD_STATUS = 'character', EXTRA_INFO = 'character')
  ) %>% (data.table::rbindlist)
  df <- preprocess_data_types(df)
  df <- preprocess_exclusions(df)
  df <- preprocess_with_recovery(df)
  df <- preprocess_sort_band_date(df)
  df <- dplyr::left_join(df, dplyr::select(crosswalk, .data$BBL_SPECIES_ID, .data$EBIRDST_CODE), by = dplyr::join_by('SPECIES_ID' == 'BBL_SPECIES_ID'))
  for (species in unique(stats::na.omit(df$EBIRDST_CODE))){
    print(species)
    df %>% (dplyr::filter)(.data$EBIRDST_CODE == species) %>% saveRDS(file.path(the$banding_rds_path, paste0(species, '.rds')))
  }
}

batch_preprocess_raw_files_to_rds <- function(banding_data_dir){

# Some file sets need to be collapsed together for processing because same ebirdst taxa span multiple files

collapse_list <- lapply(banding::banding_csv_collapse_list, function(i) file.path(banding_data_dir, paste0('NABBP_2022_grp_', sprintf('%02d', i), '.csv')))

if (!dir.exists(file.path(the$banding_rds_path))) {dir.create(the$banding_rds_path)}

## Batch process raw files to RDS

my_suffix <- 'pf'
batchtools::batchMap(process_file_set,
         collapse_list,
         reg = batchtools::makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                            conf.file = system.file('batchtools.conf.R', package = 'banding')
         ))
batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 60,
                            memory = 8))
batchtools::waitForJobs()
}
