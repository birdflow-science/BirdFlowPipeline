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


if (!dir.exists('output')) {dir.create('output')}
if (!dir.exists(file.path('output','maps'))) {dir.create(file.path('output','maps'))}

# taxonomy join ---------

tax_join <- fread(file.path('tax', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)

# functions ----------------------------------------------------

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

make_tracks <- function(
    df,
    remove_identical = TRUE,
    crs_in = 'wgs84',
    crs_out = birdflow_crs,
    min_dist_m = 15000){
  # Function to convert banding df to an sf object of linestrings of origin-destination tracks
  # expand to two steps
  df <- df %>% group_by(BAND) %>%
    mutate(count = c(1, rep(2, n() - 2), 1)) %>%
    uncount(count) %>%
    mutate(BAND_TRACK = paste(BAND, rep(1:(n()/2), each = 2), sep = '_')) %>%
    ungroup
  # get rid of same start and stop coordinates (multipoint filter also does it)
  if (remove_identical){
    df <- df %>%
      group_by(BAND_TRACK) %>%
      filter(n_distinct(LON_DD) > 1 | n_distinct(LAT_DD) > 1) %>%
      ungroup
  }
  # summarise
  df <- df %>%
    group_by(BAND_TRACK) %>%
    summarise(start_date = min(EVENT_DATE),
              stop_date = max(EVENT_DATE),
              geom = sprintf("LINESTRING(%s %s, %s %s)",
                             LON_DD[1], LAT_DD[1], LON_DD[2], LAT_DD[2])
    ) %>% ungroup
  df <- st_as_sf(df, wkt = "geom", crs = crs_in) %>% st_transform(crs_out)
  # add and filter distances
  df$distance <- as.numeric(st_length(df))
  df <- df %>% filter(distance >= min_dist_m)
  df
}

## Filtering attrition and days elapsed vs. distance graphs

attrition_df <- data.frame(species_code = character(0),
                         species_name = character(0),
                         n_i_rows = integer(0),
                         n_day180 = integer(0),
                         n_day180_dist0 = integer(0),
                         n_day180_dist15 = integer(0))
pdf(file = file.path('output', 'distance_days.pdf'), onefile = TRUE)
rds_files <- list.files('rds', full.names = TRUE)
for (file in rds_files){
  df <- readRDS(file)
  species_code <- basename(file) %>% sub('\\.rds$', '', .)
  species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`
  message(species_name)
  attrition_df_row <- data.frame(species_code = species_code,
                               species_name = species_name,
                               n_i_rows = nrow(df),
                               n_day180 = NA_integer_,
                               n_day180_dist0 = NA_integer_,
                               n_day180_dist15 = NA_integer_)
  print(nrow(df))
  df <- preprocess_calc_distance_days(df)
  df <- drop_na(df, distance, days)
  df <- filter(df, days > 0 & days < 365/2)
  print(nrow(df))
  n_day180 <- nrow(df)
  attrition_df_row$n_day180_dist0 <- filter(df, distance > 0) %>% pull(distance) %>% na.omit %>% length
  attrition_df_row$n_day180_dist15 <- filter(df, distance > 15) %>% pull(distance) %>% na.omit %>% length
  attrition_df_row$n_day180 <- n_day180
  attrition_df <- rbind(attrition_df, attrition_df_row)
  write.csv(attrition_df, file.path('output','attrition.csv'), row.names = FALSE)
  if (n_day180 < 20){
    next
  }
  plot(df$days, df$distance, xlab = 'days elapsed', ylab = 'distance (km)',
       main = paste0(species_name, '\n', '(n = ', n_day180, ')'))
}
dev.off()

## Map pairwise tracks

rds_files <- list.files('rds', full.names = TRUE)
for (file in rds_files){
  df <- readRDS(file)
  species_code <- basename(file) %>% sub('\\.rds$', '', .)
  species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`
  message(species_name)
  df <- make_tracks(df)
  if (nrow(df) > 1 && length(unique(st_bbox(df))) == 4){
    try({
      pdf(file.path('output','maps', paste0(species_name, '.pdf')), 6, 6)
      if (exists('coastline')) rm(coastline)
      try({coastline <- get_coastline(select(df, BAND_TRACK), buffer = 5)})
      my_main <- paste0(species_name, '\n(n = ', nrow(df), ')')
      if (exists('coastline') && nrow(coastline) > 1){
        plot(coastline, main = my_main)
        plot(select(df, BAND_TRACK), add = TRUE)
      } else {
        # don't plot coastline if there isn't any
        plot(select(df, BAND_TRACK), main = my_main)
      }
      dev.off()
    })
  }
}

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
# provided.
