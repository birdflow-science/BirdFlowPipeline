# Load banding and MOTUS tracks dataframes

#' @export
load_banding_transitions_df <- function(banding_rds_path){
  
  # Read data
  # banding_rds_path = '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/banding/rds/amewoo.rds'
  file_exists <- dplyr::if_else(file.exists(banding_rds_path), TRUE, FALSE)
  if (!file_exists){
    return(NULL)
  }
  df <- readRDS(banding_rds_path)
  if (nrow(df) == 0){
    return(NULL)
  }
  
  band_df <- data.frame(
    route_id = df$BAND,
    date = as.Date(df$EVENT_DATE),
    lon = df$LON_DD,
    lat = df$LAT_DD,
    route_type = c("banding")
  )
  
  return(band_df)
}

#' @export
load_motus_transitions_df <- function(motus_rds_path){
  
  # Read data
  # motus_rds_path = '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/motus/rds/amewoo.rds'
  file_exists <- dplyr::if_else(file.exists(motus_rds_path), TRUE, FALSE)
  if (!file_exists){
    return(NULL)
  }
  df <- readRDS(motus_rds_path)
  if (nrow(df) == 0){
    return(NULL)
  }
  
  return(df)
}
