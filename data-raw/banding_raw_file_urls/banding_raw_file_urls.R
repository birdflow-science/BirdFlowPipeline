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
