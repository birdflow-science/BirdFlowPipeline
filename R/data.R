#' URLs for downloading raw USGS banding datafiles version 2022-07-14
#'
#' @format ## `banding_raw_file_urls`
#' A data frame with 60 obs. of 2 variables, scraped from the html source at the below DOI
#' \describe{
#'   \item{url}{download URL}
#'   \item{name}{filename}
#'
#' }
#' @source <https://dx.doi.org/10.5066/P9BSM38F>
"banding_raw_file_urls"

#' data.frame for matching USGS banding taxa to ebirdst taxa
#'
#' @format ## `taxonomy_crosswalk`
#' A data.frame with 1072 obs. of 8 variables, made from eBird 2020 Taxonomy, USGS banding dataset `species.csv` table, and manual file `taxonomic_join_overrides.csv`.
#' \describe{
#'   \item{BBL_SPECIES_ID}{4-digit USGS BBL species code}
#'   \item{BBL_SPECIES_NAME}{USGS BBL common name}
#'   \item{BBL_SCI_NAME}{USGS BBL scientific name}
#'   \item{BBL_GRP}{file number suffix of raw BBL data CSV}
#'   \item{EBIRD_SPECIES_CODE}{6-letter primary key for eBird, sometimes with trailing number}
#'   \item{EBIRDST_CODE}{same as eBird species code, but NA if taxon not included in Status and Trends}
#'   
#'
#' }
#' @source <https://dx.doi.org/10.5066/P9BSM38F>
#' @source <https://www.birds.cornell.edu/clementschecklist/download/>
"taxonomy_crosswalk"


#' Custom coordinate reference system for American BirdFlow Models
#'
#' @format ## `americas_crs`
#' A `crs` object as produced by [terra::crs()] this is Custom Lamber Azimuthal
#' projection with an origin of 30, -95 (latitude, longitude),
#' a false easting of  5.5e6, and a false northing of 9.5e6.
#' It is designed to show all of North and South America but with 
#' the origin closer to North America so that the distortion is minimal there.
#' This is an equal area projection but does distort distances and shapes.
"americas_crs"

#' Clipping polygon for the Americas
#'
#' @format ## `americas_clip`
#' A `sfc` object as used by the **sf** package. This is used to define 
#' North and South America when constraining BirdFlow models to just the 
#' Americas.  It is made by mergin together all the Natural Earth countries
#' that are in the Amercas, dropping Hawaii, and then buffering by 300 km.
"americas_clip"



