## Somewhat messy and highly interactive/iterative code to assist in creating
## the crosswalk file between BBL and eBird v.2021 taxonomies

bbl_species_file <- '~/banding_raw/NABBP_Lookups_2022/species.csv'
eb_taxonomy_file <- 'data-raw/banding_taxonomy/eBird_Taxonomy_v2021.csv'
overrides_file <- 'data-raw/banding_taxonomy/taxonomic_join_overrides.csv'

suppressPackageStartupMessages(
  {
    library(ebirdst)
    library(data.table)
    library(dplyr)
    library(tidyr)
  }
)

df <- fread(bbl_species_file) %>%
  select(SPECIES_ID, SPECIES_NAME, SCI_NAME)
names(df) <- paste0('BBL_', names(df))

tax <- fread(eb_taxonomy_file) %>%
  select(SPECIES_CODE, PRIMARY_COM_NAME, SCI_NAME, CATEGORY)
names(tax) <- paste0('EBIRD_', names(tax))

# Join by both common and scientific name
joined <- left_join(df,
  tax %>% select(EBIRD_SPECIES_CODE_BOTH = EBIRD_SPECIES_CODE,
                 EBIRD_PRIMARY_COM_NAME,
                 EBIRD_SCI_NAME),
  by = join_by(BBL_SPECIES_NAME == EBIRD_PRIMARY_COM_NAME,
               BBL_SCI_NAME == EBIRD_SCI_NAME)
  )

# Join by just scientific name
joined <- left_join(
  joined,
  tax %>% select(EBIRD_SPECIES_CODE_SCI = EBIRD_SPECIES_CODE,
                 EBIRD_SCI_NAME),
  by = join_by(BBL_SCI_NAME == EBIRD_SCI_NAME)
)

# Join by just common name
joined <- left_join(
  joined,
  tax %>% select(EBIRD_SPECIES_CODE_COM = EBIRD_SPECIES_CODE,
                 EBIRD_PRIMARY_COM_NAME),
  by = join_by(BBL_SPECIES_NAME == EBIRD_PRIMARY_COM_NAME)
)

## Check for conflicts (more than one identified taxon based on sci, common, both)

joined$EBIRD_SPECIES_CODE_CONFLICT <- as.logical(NA)
for (i in seq_len(nrow(joined))){
  vec <- c(joined$EBIRD_SPECIES_CODE_BOTH[i],
           joined$EBIRD_SPECIES_CODE_SCI[i],
           joined$EBIRD_SPECIES_CODE_COM[i])
  is_conflict <- if (all(is.na(vec))){
    FALSE
  } else if (length(unique(stats::na.omit(vec))) == 1){
    FALSE
  } else {
    TRUE
  }
  joined[i, 'EBIRD_SPECIES_CODE_CONFLICT'] <- is_conflict
}
joined %>% (dplyr::filter)(EBIRD_SPECIES_CODE_CONFLICT)

## Add column for eBird species code consensus
joined$EBIRD_SPECIES_CODE_CONSENSUS <- NA_character_
for (i in seq_len(nrow(joined))){
  if (joined$EBIRD_SPECIES_CODE_CONFLICT[i]){
    joined$EBIRD_SPECIES_CODE_CONSENSUS[i] <- NA_character_
    next
  } else {
    vec <- c(joined$EBIRD_SPECIES_CODE_BOTH[i],
             joined$EBIRD_SPECIES_CODE_SCI[i],
             joined$EBIRD_SPECIES_CODE_COM[i])
    if (all(is.na(vec))){
      joined$EBIRD_SPECIES_CODE_CONSENSUS[i] <- NA_character_
      next
    } else {
      joined$EBIRD_SPECIES_CODE_CONSENSUS[i] <- unique(stats::na.omit(vec))
    }
  }
}

# Append eBird common name for consensus
joined <- left_join(
  joined,
  tax %>% select(
    EBIRD_SPECIES_CODE_CONSENSUS = EBIRD_SPECIES_CODE,
    EBIRD_PRIMARY_COM_NAME_CONSENSUS = EBIRD_PRIMARY_COM_NAME,
    EBIRD_CATEGORY_CONSENSUS = EBIRD_CATEGORY)
  )

# are all ebirdst_runs based on species-level taxa? YES.
# nrow(ebirdst_runs)
# left_join(
#   ebirdst_runs %>% select(species_code),
#   tax %>% select(EBIRD_SPECIES_CODE, EBIRD_CATEGORY),
#   by = join_by('species_code' == EBIRD_SPECIES_CODE)
# ) %>% pull(EBIRD_CATEGORY) %>% table(useNA='always')

# joined %>%
#   select(BBL_SPECIES_NAME, EBIRD_SPECIES_CODE_CONSENSUS, EBIRD_PRIMARY_COM_NAME_CONSENSUS, EBIRD_CATEGORY_CONSENSUS) %>%
#   (dplyr::filter)(EBIRD_CATEGORY_CONSENSUS != 'species' | EBIRD_CATEGORY_CONSENSUS %in% NA_character_ | grepl('[;]', EBIRD_SPECIES_CODE_CONSENSUS)) %>%
#   (utils::write.csv)('working_overrides.csv', row.names = FALSE)

# sketchy taxa to probably not include in analysis
# CANG
# CLRA
# BHVI/SOVI
# Scrub-Jays (CASJ, WESJ, WOSJ)
# WCSE/CRSE

#odf <- fread('working_overrides.csv', na.strings = '') %>% select(EBIRD_SPECIES_CODE_OVERRIDE, BBL_SPECIES_NAME) %>% (stats::na.omit)
#odf <- odf %>% left_join(tax, by = join_by('EBIRD_SPECIES_CODE_OVERRIDE' == 'EBIRD_SPECIES_CODE'))
#odf <- odf %>% select(BBL_SPECIES_NAME, EBIRD_SPECIES_CODE_OVERRIDE, EBIRD_PRIMARY_COM_NAME, EBIRD_SCI_NAME, EBIRD_CATEGORY)
#utils::write.csv(odf, 'tax/taxonomic_join_overrides.csv', row.names = FALSE)

overrides <- fread(overrides_file)

joined <- left_join(joined,
          overrides %>% select(BBL_SPECIES_NAME, EBIRD_SPECIES_CODE_OVERRIDE),
          by = 'BBL_SPECIES_NAME'
)

joined <- joined %>% select(-EBIRD_PRIMARY_COM_NAME_CONSENSUS, -EBIRD_CATEGORY_CONSENSUS)

joined$EBIRD_SPECIES_CODE_FINAL <- NA_character_
for (i in seq_len(nrow(joined))){
  if (!is.na(joined$EBIRD_SPECIES_CODE_OVERRIDE[i])){
    joined$EBIRD_SPECIES_CODE_FINAL[i] <- joined$EBIRD_SPECIES_CODE_OVERRIDE[i]
  } else {
    joined$EBIRD_SPECIES_CODE_FINAL[i] <- joined$EBIRD_SPECIES_CODE_CONSENSUS[i]
  }
}
joined %>% select(BBL_SPECIES_NAME, starts_with('EBIRD_SPECIES_CODE')) %>%
  left_join(tax,
            by = join_by('EBIRD_SPECIES_CODE_FINAL' == 'EBIRD_SPECIES_CODE')
  ) %>% 
  mutate(ebirdst = ebirdst::get_species(
    if_else(
      is.na(EBIRD_SPECIES_CODE_FINAL),
      'NA',
      EBIRD_SPECIES_CODE_FINAL))) %>%
  (utils::write.csv)('join_check.csv', row.names = FALSE)

##
# do checks of the above CSV file, and make any necessary fixes to taxonomic_join_overrides.csv, then run to this point again, and repeat
##

file.remove('join_check.csv')

taxonomy_crosswalk <- joined %>% select(BBL_SPECIES_ID, BBL_SPECIES_NAME, BBL_SCI_NAME, EBIRD_SPECIES_CODE = EBIRD_SPECIES_CODE_FINAL) %>%
  left_join(tax, by = 'EBIRD_SPECIES_CODE') %>%
  mutate(EBIRDST_CODE = ebirdst::get_species(
    if_else(
      is.na(EBIRD_SPECIES_CODE),
      'NA',
      EBIRD_SPECIES_CODE)))

usethis::use_data(taxonomy_crosswalk)