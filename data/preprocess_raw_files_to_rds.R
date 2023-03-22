library(data.table)
library(dplyr)

# Some file sets need to be collapsed together for processing because same ebirdst taxa span multiple files
collapse_list <- list(1, c(2,3), 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, c(42, 43), 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, c(55, 56), 57)
collapse_list <- lapply(collapse_list, function(i) file.path('data', paste0('NABBP_2022_grp_', sprintf('%02d', i), '.csv')))
  
crosswalk <- fread('tax/taxonomy_crosswalk.csv')
if (!dir.exists('rds')) {dir.create('rds')}

for (i in seq_along(collapse_list)){
  df <- lapply(collapse_list[[i]], fread) %>% rbindlist
  df <- left_join(df, select(crosswalk, BBL_SPECIES_ID, EBIRDST_CODE), by = join_by('SPECIES_ID' == 'BBL_SPECIES_ID'))
  df <- df %>% group_by(SPECIES_ID, BAND) %>% filter(n() >= 2) %>% ungroup
  for (species in unique(na.omit(df$EBIRDST_CODE))){
    print(species)
    df %>% filter(EBIRDST_CODE == species) %>% saveRDS(file.path('rds', paste0(species, '.rds')))
  }
}
