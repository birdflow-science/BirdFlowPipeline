# For big downloads not to timeout
# options(timeout = max(3600, getOption("timeout")))

# library(data.table)
# library(dplyr)
# library(rvest)
# library(tidyr)

# /work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/banding_data
download_banding_files <- function(banding_data_dir){

# path management ---------------------------------------------------------

if (!dir.exists(banding_data_dir)) {dir.create(banding_data_dir)}

# get datafile info -------------------------------------------------------

#read_html('https://www.sciencebase.gov/catalog/item/632b2d7bd34e71c6d67bc161')
file_info <- rvest::read_html(file.path(banding_data_dir, 'htmlpage.htm')) %>% rvest::html_nodes('td span.sb-download-link')
file_df <- data.frame(
  url = rvest::html_attr(file_info, 'data-url'),
  name = rvest::html_text(file_info))
file_df$url <- paste0('https://www.sciencebase.gov', file_df$url)

### Download files

for (i in seq_len(nrow(file_df))){
  # download in reverse order (earlier files are huge)
  file_path <- file.path(banding_data_dir, file_df$name[i])
  download.file(file_df$url[i], file_path)
}

}
