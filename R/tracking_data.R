# scripts to get the n_stopovers and straightness of the actual tracks

#args:  eval_metrics, params

# get a birdflow model from this run, for CRS/pixel info


rename_tracking_columns <- function(raw_tracking_df,
                                    datetime_col,
                                    long_col,
                                    lat_col,
                                    id_col){
  # more from Ben
  # remove non-location data
  # data <- data[!`sensor-type`=="accessory-measurements"]
  # data <- data[!is.na(`location-long`) & !is.na(`location-lat`)]
  #stopifnot(all(sensor.types %in% c("gps","argos-doppler-shift","solar-geolocator")))
  #
  # if ("gps" %in% names(data.split)) {
  #   data.split$gps <- filter_gps(data.split$gps)
  # }
  # if ("argos-doppler-shift" %in% names(data.split)) {
  #   data.split$`argos-doppler-shift` <- filter_argos(data.split$`argos-doppler-shift`)
  # }
  # if ("solar-geolocator" %in% names(data.split)) {
  #   data.split$`solar-geolocator` <- filter_geo(data.split$`solar-geolocator`)
  # }
  # check next line, from Ben
  # raw_tracking_df <- raw_tracking_df[is.na(`manually-marked-outlier`) | `manually-marked-outlier`==FALSE]
  ## from Ben ##
  # data <- data[,.(
  #   timestamp,
  #   lon=`location-long`,
  #   lat=`location-lat`,
  #   tag=`tag-local-identifier`,
  #   indiv=`individual-local-identifier`
  # )]
  ## from Ben, for geolocator ##
  # data <- data[!close_to_equinox(data$timestamp,n.pad = 20)]
  df <- data.table::setnames(raw_tracking_df,
                             old = c(id_col, datetime_col, long_col, lat_col),
                             new = c('bird_id', 'datetime', 'longitude', 'latitude')
  )
  as.data.frame(df)[,c('bird_id', 'datetime', 'longitude', 'latitude')]
}

tracks_to_rts <- function(df, bf, params){
  
  new_df <- as.data.frame(list(
    route_id = df$bird_id,
    date = as.Date(df$datetime),
    lon = df$longitude,
    lat = df$latitude,
    route_type = c('tracking')
  ))
  routes <- new_df |> BirdFlowR::Routes(species=params$species)
  routes$metadata <- bf$metadata
  birdflow_routes <- routes |> BirdFlowR::as_BirdFlowRoutes(bf=bf)
  return(birdflow_routes)
}

#' Get the BirdFlowRoutes object of the real track
#'
#' @param bf The BirdFlow model object
#' @param params The params object, containing at least the hdf_dir from which to learn bf object
#' @returns the BirdFlowRoutes object of the real track
#' @export
get_real_track <- function(bf, params, filter=FALSE){
  tracks_files <- list.files(the$tracking_data_path,
                             pattern = paste0("^", params$species, ".*tracks.*\\.csv"),
                             full.names = TRUE)
  if (length(tracks_files) == 0){
    return(NULL)
  }else{
    tracks_df_list <- lapply(tracks_files, data.table::fread)
    indiv_id_list <- lapply(tracks_df_list, function(i){unique(i$indiv)})
    # throw error if all the indiv ids are not unique across the different data.frames
    stopifnot(
      length(unique(Reduce(c, indiv_id_list))) == sum(sapply(indiv_id_list, function(i){length(unique(i))}))
    )
    # Combine potentially multiple tracking data.frames
    tracks <- as.data.frame(data.table::rbindlist(tracks_df_list, fill = TRUE))
    tracks <- rename_tracking_columns(tracks, 'timestamp', 'lon', 'lat', 'indiv')
    # Convert tracks to routes, using a birdflow object grid from this modelset
    rts <- tracks_to_rts(tracks, bf, params)
    
    if (filter){
      # appropriate season tracks only. This is because the n_stopover and straightness only make sense during migration.
      rts$data <- rts$data |> dplyr::filter(.data$timestep %in% BirdFlowR::lookup_season_timesteps(bf, params$season))
    }
      
    return(rts)
  }
}
# 
# } else {
#   return(list(straightness = NA_real_, length = NA_real_, displacement = NA_real_, 
#               n_stopovers = NA_real_))
#   

