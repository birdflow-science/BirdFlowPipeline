# For big downloads not to timeout
# options(timeout = max(3600, getOption("timeout")))

# library(data.table)
# library(dplyr)
# library(tidyr)

# /work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/banding_data
download_banding_files <- function(banding_data_dir){

# path management ---------------------------------------------------------

if (!dir.exists(banding_data_dir)) {dir.create(banding_data_dir)}

### Download files

for (i in seq_len(nrow(banding_raw_file_urls))){
  # download in reverse order (earlier files are huge)
  file_path <- file.path(banding_data_dir, banding_raw_file_urls$name[i])
  utils::download.file(banding_raw_file_urls$url[i], file_path)
}

}
