# For big downloads not to timeout
options(timeout = max(3600, getOption("timeout")))

library(data.table)
library(dplyr)
library(rvest)
library(tidyr)

# path management ---------------------------------------------------------

if (!dir.exists('data')) {dir.create('data')}

# get datafile info -------------------------------------------------------

#read_html('https://www.sciencebase.gov/catalog/item/632b2d7bd34e71c6d67bc161')
file_info <- read_html('data/htmlpage.htm') %>% html_nodes('td span.sb-download-link')
file_df <- data.frame(
  url = html_attr(file_info, 'data-url'),
  name = html_text(file_info))
file_df$url <- paste0('https://www.sciencebase.gov', file_df$url)

### Download files

for (i in seq_len(nrow(file_df))){
  # download in reverse order (earlier files are huge)
  file_path <- file.path('data', file_df$name[i])
  download.file(file_df$url[i], file_path)
}
