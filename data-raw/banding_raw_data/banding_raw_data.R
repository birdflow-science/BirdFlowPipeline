#library(rvest)
#library(magrittr)
#library(usethis)

# htmlpage.html is manually saved from Mozilla Firefox "View Source" at https://dx.doi.org/10.5066/P9BSM38F

file_info <- rvest::read_html(file.path('data-raw', 'banding_raw_file_urls', 'htmlpage.htm')) %>% rvest::html_nodes('td span.sb-download-link')
banding_raw_file_urls <- data.frame(
  url = rvest::html_attr(file_info, 'data-url'),
  name = rvest::html_text(file_info))
banding_raw_file_urls$url <- paste0('https://www.sciencebase.gov', banding_raw_file_urls$url)

usethis::use_data(banding_raw_file_urls)

# This next part is a manually created list of sets of the above raw banding CSV
# files that need to be collapsed together before processing For example,
# multiple subspecies of one species are stored in separate files

banding_csv_collapse_list <-
  list(
    1,
    c(2, 3),
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    40,
    41,
    c(42, 43),
    44,
    45,
    46,
    47,
    48,
    49,
    50,
    51,
    52,
    53,
    54,
    c(55, 56),
    57
  )

usethis::use_data(banding_csv_collapse_list)
