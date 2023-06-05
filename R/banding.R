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


## Function to convert banding data to linestring.  Removed messy plotting.
# rds_files <- list.files('rds', full.names = TRUE)
# ... is additional arguments passed to make_tracks
banding_data_to_linestring <- function(rds_file, ...) {
  file <- rds_file
  df <- readRDS(file)
  species_code <- basename(file) %>% sub('\\.rds$', '', .)
  message(species_code)
  df <- make_tracks(banding_rds_path = file, ...)$obs_df
  df <- df %>%
    group_by(BAND_TRACK) %>%
    summarise(
      start_date = min(date),
      stop_date = max(date),
      geom = sprintf("LINESTRING(%s %s, %s %s)",
                     lon[1], lat[1], lon[2], lat[2])
    ) %>% ungroup
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
