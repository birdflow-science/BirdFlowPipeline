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

#' Groupings for collapsing raw banding CSV files together before preprocessing.
#' 
#' For example, if multiple subspecies of one species are stored in separate files
#'
#' @format ## `banding_csv_collapse_list`
#' A list of length 54, each list item is numeric vector of of CSV file numbers to be processed together
#'
#' 
#' @source <https://dx.doi.org/10.5066/P9BSM38F>
"banding_csv_collapse_list"

#' data.frame for matching USGS banding taxa to ebirdst taxa
#'
#' @format ## `taxonomy_crosswalk`
#' A data.frame with 1072 obs. of 8 variables, made from eBird 2020 Taxonomy, USGS banding dataset `species.csv` table, and manual file `taxonomic_join_overrides.csv`.
#' \describe{
#'   \item{BBL_SPECIES_ID}{4-digit USGS BBL species code}
#'   \item{BBL_SPECIES_NAME}{USGS BBL common name}
#'   \item{BBL_SCI_NAME}{USGS BBL scientific name}
#'   \item{EBIRD_SPECIES_CODE}{6-letter primary key for eBird, sometimes with trailing number}
#'   \item{EBIRD_PRIMARY_COM_NAME}{eBird common name}
#'   \item{EBIRD_CATEGORY}{species, hybrid, slash, spuh, or NA}
#'   \item{EBIRDST_CODE}{same as eBird species code, consider removing one column}
#'   
#'
#' }
#' @source <https://dx.doi.org/10.5066/P9BSM38F>
#' @source <https://www.birds.cornell.edu/clementschecklist/download/>
"taxonomy_crosswalk"