library(data.table)
library(dplyr)

# Some file sets need to be collapsed together for processing because same ebirdst taxa span multiple files
collapse_list <- list(1, c(2,3), 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, c(42, 43), 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, c(55, 56), 57)
collapse_list <- lapply(collapse_list, function(i) file.path('data', paste0('NABBP_2022_grp_', sprintf('%02d', i), '.csv')))
  
crosswalk <- fread('tax/taxonomy_crosswalk.csv')
if (!dir.exists('rds')) {dir.create('rds')}

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

for (i in seq_along(collapse_list)){
  df <- lapply(collapse_list[[i]],
               fread,
               colClasses = c(BIRD_STATUS = 'character', EXTRA_INFO = 'character')
        ) %>% rbindlist
  df <- preprocess_data_types(df)
  df <- preprocess_exclusions(df)
  df <- preprocess_with_recovery(df)
  df <- preprocess_sort_band_date(df)
  df <- left_join(df, select(crosswalk, BBL_SPECIES_ID, EBIRDST_CODE), by = join_by('SPECIES_ID' == 'BBL_SPECIES_ID'))
  for (species in unique(na.omit(df$EBIRDST_CODE))){
    print(species)
    df %>% filter(EBIRDST_CODE == species) %>% saveRDS(file.path('rds', paste0(species, '.rds')))
  }
}
