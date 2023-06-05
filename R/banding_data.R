#' Download raw USGS banding files
#'
#' @param banding_raw_path Path to which to download the files. If not provided, it will download to `the$banding_raw_path`
#' @returns Function exists for its side effects, namely downloading the files listed in package data object [banding_raw_file_urls]
#' @seealso [banding_raw_file_urls]
#' @export
download_banding_files <- function(banding_raw_path) {
  # keep oringal option for download timeout when finished or error
  old_timeout_option <- options()$timeout
  on.exit({
    options(timeout = old_timeout_option)
  })
  # set new longer timeout option
  options(timeout = max(3600, getOption("timeout")))
  # check if argument provided. if not, use from pkg environment preset
  if (missing(banding_raw_path)){
    banding_raw_path <- the$banding_raw_path
  }
  # make directory if doesn't exist
  if (!dir.exists(banding_raw_path)) {
    dir.create(banding_raw_path)
  }
  # Download files
  for (i in seq_len(nrow(banding::banding_raw_file_urls))) {
    # download in reverse order (earlier files are huge)
    file_path <-
      file.path(banding_raw_path, banding::banding_raw_file_urls$name[i])
    utils::download.file(banding::banding_raw_file_urls$url[i], file_path)
  }
}


#' Function to take a set of raw files and produce per-species RDS
#'
#' @param collapse_list_item A vector containing CSV file numbers to process together
#' @returns Side effect is writing `.rds` file or files
#' @export
process_file_set <- function(collapse_list_item, banding_data_dir){
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
    df %>% (dplyr::filter)(.data$EBIRDST_CODE == species) %>% saveRDS(file.path(banding_data_dir, paste0(species, '.rds')))
  }
}

#' Batch preprocess banding data on the cluster
#'
#' @param banding_data_dir Directory containing CSV files
#' @return side effect, lots of rds files
#' @export
batch_preprocess_raw_files_to_rds <- function(banding_data_dir){
  
  # check if argument provided. if not, use from pkg environment preset
  if (missing(banding_data_dir)){
    banding_data_dir <- the$banding_rds_path
  }
  
  # Some file sets need to be collapsed together for processing because same ebirdst taxa span multiple files
  
  collapse_list <- lapply(banding::banding_csv_collapse_list, function(i) file.path(banding_data_dir, paste0('NABBP_2022_grp_', sprintf('%02d', i), '.csv')))
  
  if (!dir.exists(file.path(banding_data_dir))) {dir.create(banding_data_dir)}
  
  ## Batch process raw files to RDS
  
  my_suffix <- 'pf'
  batchtools::batchMap(process_file_set,
                       collapse_list,
                       more.args = list(banding_data_dir = banding_data_dir),
                       reg = batchtools::makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                                                      conf.file = system.file('batchtools.conf.R', package = 'banding')
                       ))
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = list(walltime = 60,
                                          memory = 8))
  batchtools::waitForJobs()
}


#' Function to convert banding data to linestring of origin-destination pairs
#'
#' @param rds_file path to rds file to make linestrings from
#' @param ... additional arguments passed to make_tracks
#'
#' @returns sf data.frame with linestring geometry
#' @export
#'
banding_data_to_linestring <- function(rds_file, ...) {
  file <- rds_file
  df <- readRDS(file)
  species_code <- sub('\\.rds$', '', basename(file))
  message(species_code)
  df <- make_tracks(banding_rds_path = file, ...)$obs_df
  df <- df %>%
    (dplyr::group_by)(.data$BAND_TRACK) %>%
    (dplyr::summarise)(
      start_date = min(.data$date),
      stop_date = max(.data$date),
      geom = sprintf("LINESTRING(%s %s, %s %s)",
                     .data$lon[1], .data$lat[1], .data$lon[2], .data$lat[2])
    ) %>% (dplyr::ungroup)
  sf::st_as_sf(df, wkt = "geom", crs = 'wgs84')
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
