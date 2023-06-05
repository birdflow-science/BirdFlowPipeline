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
