library(data.table)
library(dplyr)
library(rvest)
library(rebird)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(remotes)
library(BirdFlowR)
library(tidyr)
library(ebirdst)
install_github('birdflow-science/BirdFlowModels')
install_github('birdflow-science/BirdFlowR', build_vignettes = TRUE, upgrade = 'never')
library(BirdFlowR)
library(BirdFlowModels)
library(geodist)

# path management ---------------------------------------------------------

if (!dir.exists('pdf')) {dir.create('pdf')}

# summarize taxa ----------------------------------------------------------

# files <- list.files('data', pattern = '\\.csv$', full.names = TRUE)
# taxa_list <- lapply(files, function(file){
#   fread(file) %>% filter(EVENT_TYPE == 'E') %>% pull(SPECIES_ID)
# })
# taxa_vec <- Reduce(c, taxa_list)
# taxa_df <- data.frame(SPECIES_ID = taxa_vec)
# species_csv <- fread('data/NABBP_Lookups_2022/species.csv')
# taxa_df <- left_join(taxa_df, species_csv[,c('SPECIES_ID', 'SPECIES_NAME', 'SCI_NAME')])
# taxa_df <- taxa_df %>% group_by(SPECIES_ID, SPECIES_NAME, SCI_NAME) %>% summarize(N = n()) %>% arrange(-N) 
# taxa_df %>% write.csv('taxa_summary.csv')
# eb_tax <- rebird::ebirdtaxonomy()
# eb_tax <- eb_tax %>% select(sciName, comName, speciesCode, category, taxonOrder, reportAs)
# left_join(taxa_df, eb_tax, by = c('SPECIES_NAME' = 'comName', 'SCI_NAME' = 'sciName')) %>% write.csv('join_tax.csv')

# test using rwbl data ----------------------------------------------------

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
  df <- df %>% group_by(BAND) %>% filter(n() >= 2) %>% ungroup
  df
}

preprocess_sort_band_date <- function(df){
  df %>% arrange(BAND, EVENT_DATE)
}

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

## NEW process

species_lookup <- fread('data/NABBP_Lookups_2022/species.csv')
# slashes cause problems later for file.path()
species_lookup$SPECIES_NAME <- gsub('[/]', '_', species_lookup$SPECIES_NAME)
summary_df <- data.frame(SPECIES_ID = integer(0),
                         species_name = character(0),
                         n_filtered_tracks = integer(0))
pdf(file = file.path('pdf', 'distance_days.pdf'), onefile = TRUE)
for (filename in filter(file_df, grepl('\\.csv$', name))$name){
  multispecies_df <- fread(file.path('data',filename),
                           colClasses = c(BIRD_STATUS = 'character',
                                          EXTRA_INFO = 'character'))
  species_tbl <- data.frame(SPECIES_ID = unique(multispecies_df$SPECIES_ID)) %>%
    left_join(species_lookup[,c('SPECIES_ID', 'SPECIES_NAME')], by = 'SPECIES_ID')
  for (species_tbl_row in seq_len(nrow(species_tbl))){
    df <- multispecies_df %>% filter(SPECIES_ID == species_tbl[species_tbl_row, 'SPECIES_ID'])
    species_name <- species_tbl[species_tbl_row, 'SPECIES_NAME']
    message(species_name)
    summary_df_row <- data.frame(SPECIES_ID = species_tbl[species_tbl_row, 'SPECIES_ID'],
                                 species_name = species_name,
                                 n_filtered_tracks = NA_integer_)
    print(nrow(df))
    df <- preprocess_data_types(df)
    df <- preprocess_exclusions(df)
    df <- preprocess_with_recovery(df)
    df <- preprocess_sort_band_date(df)
    df <- preprocess_calc_distance_days(df)
    #df <- preprocess_filter_days(df, 365/2)
    df <- drop_na(df, distance, days)
    df <- filter(df, days > 0 & days < 365/2)
    print(nrow(df))
    n_filtered_tracks <- nrow(df)
    summary_df_row$n_filtered_tracks <- n_filtered_tracks
    summary_df <- rbind(summary_df, summary_df_row)
    write.csv(summary_df, 'summary_df.csv', row.names = FALSE)
    if (n_filtered_tracks < 20){
      next
    }
    plot(df$days, df$distance, xlab = 'days elapsed', ylab = 'distance (km)',
         main = paste0(species_name, '\n', '(n = ', n_filtered_tracks, ')'))
  }
}
dev.off()

## OLD process


species_lookup <- fread('data/NABBP_Lookups_2022/species.csv')
summary_df <- data.frame(SPECIES_ID = integer(0),
                         species_name = character(0),
                         nrow_start_div2 = integer(0),
                         nrow_pre_expand_div2 = integer(0),
                         nrow_post_expand_div2 = integer(0),
                         nrow_dist_gr_0 = integer(0),
                         nrow_dist_gr_15 = integer(0))
for (filename in file_df$name){
  multispecies_df <- fread(file.path('data',filename),
                           colClasses = c(BIRD_STATUS = 'character',
                                          EXTRA_INFO = 'character'))
  species_tbl <- data.frame(SPECIES_ID = unique(multispecies_df$SPECIES_ID)) %>%
    left_join(species_lookup[,c('SPECIES_ID', 'SPECIES_NAME')], by = 'SPECIES_ID')
  for (species_tbl_row in seq_len(nrow(species_tbl))){
    df <- multispecies_df %>% filter(SPECIES_ID == species_tbl[species_tbl_row, 'SPECIES_ID'])
    species_name <- species_tbl[species_tbl_row, 'SPECIES_NAME']
    message(species_name)
    summary_df_row <- data.frame(SPECIES_ID = species_tbl[species_tbl_row, 'SPECIES_ID'],
                                  species_name = species_name,
                                  nrow_start_div2 = NA_integer_,
                                  nrow_pre_expand_div2 = NA_integer_,
                                  nrow_post_expand_div2 = NA_integer_,
                                  nrow_dist_gr_0 = NA_integer_,
                                  nrow_dist_gr_15 = NA_integer_)
    original_rows <- nrow(df)
    summary_df_row$nrow_start_div2 <- as.integer(original_rows / 2)
    print(nrow(df))
    df <- preprocess_data_types(df)
    df <- preprocess_exclusions(df)
    df <- preprocess_with_recovery(df)
    # report row attrition
    print(nrow(df) / original_rows)
    # skip if no more rows
    if (nrow(df) == 0){
      summary_df_row$nrow_pre_expand_div2 <- 0L
      summary_df_row$nrow_post_expand_div2 <- 0L
      next
    }
    # record nrows
    pre_expand <- nrow(df)
    summary_df_row$nrow_pre_expand_div2 <- as.integer(pre_expand / 2)
    # exclude bands that no longer have at least 2 timepoints
    # and expand data to separate BAND_TRACKs (two consecutive points)
    df <- df %>% group_by(BAND) %>%
      mutate(count = c(1, rep(2, n() - 2), 1)) %>%
      uncount(count) %>%
      mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
      ungroup
    # report expansion factor
    print(nrow(df) / pre_expand)
    # report number of linestring-able pairs
    print(nrow(df)/2)
    summary_df_row$nrow_post_expand_div2 <- as.integer(nrow(df) / 2)
    # get rid of same start and stop coordinates (multipoint filter also does it)
    df <- df %>%
      st_as_sf(coords = c('LON_DD', 'LAT_DD'), remove = FALSE, crs = 'wgs84') %>%
      group_by(BAND_TRACK) %>%
      filter(n_distinct(LON_DD) > 1 | n_distinct(LAT_DD) > 1) %>%
      ungroup
    if (nrow(df) > 0){
      # skip rest if no rows of data
      df <- df %>%
        group_by(BAND_TRACK) %>%
        summarise(start_date = min(EVENT_DATE),
                  stop_date = max(EVENT_DATE)) %>%
        #filter(st_geometry_type(.) == "MULTIPOINT") %>%
        st_cast("LINESTRING") %>%
        mutate(distance = as.numeric(st_length(.)))
      summary_df_row$nrow_dist_gr_0 <- nrow(df)
      # this line gets rid of anything less than 15000 meters
      df <- df[as.numeric(st_length(df)) > 15000,]
      # check length of data.frame after filtering
      print(nrow(df))
      summary_df_row$nrow_dist_gr_15 <- nrow(df)
      # make plot
      if (nrow(df) > 1 && length(unique(st_bbox(df))) == 4){
        try({
          pdf(paste0('pdf/', species_name, '.pdf'), 6, 6)
          coastline <- get_coastline(select(df, BAND_TRACK), buffer = 5)
          my_main <- paste0(species_name, '\n(n = ', nrow(df), ')')
          if (nrow(coastline) > 1){
            plot(coastline, main = my_main)
            plot(select(df, BAND_TRACK), add = TRUE)
          } else {
            # don't plot coastline if there isn't any
            plot(select(df, BAND_TRACK), main = my_main)
          }
          dev.off()
        })
      }
    } else {
      summary_df_row$nrow_dist_gr_0 <- 0L
      summary_df_row$nrow_dist_gr_15 <- 0L
    }
    summary_df <- rbind(summary_df, summary_df_row)
    # write csv after species, for troubleshooting
    write.csv(summary_df, 'summary_df.csv', row.names = FALSE)
  }
}

## Work on:  Having date1 and date2 in the sf object with each BAND_TRACK

# Species matching

# ## Taxonomy notes from XML file: <taxonpro>expert advice;;identification
# keys</taxonpro> <taxoncom>Taxonomic revisions by the American Ornithological
# Society (AOS) have resulted in many changes in bird classification over the
# past several decades. The Banding Offices acknowledge these changes and use
# common names assigned by AOS. AOS discontinued species numbers with the 7th
# edition of the checklist, and BBL continues to use numbers from the 6th
# edition with modifications. However, BBL maintains common names and species
# numbers where AOS has combined formerly acknowledged species. For a list of
# BBL species designations with 4-character alpha and numeric codes, and
# scientific and common names, refer to the species.csv lookup table included
# with the data release bundle for a particular year/version.</taxoncom>
# </taxonsys> <taxongen>Most birds are identified to species in the data, some
# to subspecies. For some species, there is an "unidentified" category, to be
# used only for truly unidentifiable or intergrade individuals - not in place of
# subspecific designation. For the most part, federal bands are used on species
# included in the Migratory Bird Treaty Act (MBTA). The taxa for which we have
# included formerly recognized species are: Townsend's Shearwater, Green-winged
# Teal, Snow Goose, Canada Goose, Brant, Tundra Swan, Great Blue Heron,
# Red-tailed Hawk, Northern Flicker, Savannah Sparrow, Seaside Sparrow,
# White-crowned Sparrow, Dark-eyed Junco, Yellow-rumped Warbler, and Palm
# Warbler. Some bird species are not banded with federal bands, and therefore
# are not represented in this dataset. These include gallinaceous birds (quail,
# turkey, grouse), and rock dove, or introduced species.</taxongen>

# Banding or encounter locations from United States or Canada include
# subdivisions (state/province); all other countries are reported as country
# only. Restrictions are applied to protect exact locations of gamebirds and
# sensitive species. Gamebirds, as defined in lookup table species.csv have
# locations generalized to country, state or 1-degree block. For raptors and
# endangered species, locations are generalized to a 10-minute block minimum
# resolution. All other species locations have coordinate precisions as
# reported.</enttypd>

# <attrdef>coordinates_precision.csv lookup table includes numeric codes for 13
# coordinate precision categories. All banding and encounter records for
# sensitive species are released at a 10-minute block coordinate precision (CP).
# Game birds include waterfowl, cranes, rails, woodcock, doves, crows and
# ravens. All bandings are released at a 1-degree block coordinate precision,
# encounters are released at coordinate precisions as they were originally
# provided.<
# 
# big_df <- lapply(file_df$name, function(i){
#   file.path('data', i) %>% fread
# }) %>% rbindlist

## Week cutoffs for S&T
#ebirdst::ebirdst_weeks
