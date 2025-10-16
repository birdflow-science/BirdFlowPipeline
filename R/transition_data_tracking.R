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
#' @param filter If True, only include data points that fall within the params$season.
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
    
    if (length(rts$data)==0) {
      return(NULL)
    }
    
    return(rts)
  }
}


clean_names_tracking_data <- function(df){
  names(df) <- names(df) |> 
    (\(x) gsub("\\.", "_", x))() |>
    (\(x) gsub("-", "_", x))()
  return(df)
}

preprocess_tracking_data_to_rds <- function() {
  tracking_raw_path <- the$tracking_raw_path
  tracking_rds_path <- the$tracking_rds_path
  
  output_path <- tracking_rds_path
  tracking_files = list.files(tracking_raw_path)
  names <- unique(sub("_.*", "", list.files(tracking_raw_path)))
  # names <- names[!names %in% c("commur", "kitmur", "yebloo")]
  
  for (name in names){

    print(name)
    
    # Get all matching file paths
    new_tracking_files <- list.files(tracking_raw_path)
    matching_files <- new_tracking_files[grepl(name, new_tracking_files)]
    full_paths <- file.path(tracking_raw_path, matching_files)
    
    # Read, clean, and bind all matched files
    new_tracking_data <- suppressWarnings(
      purrr::map_dfr(full_paths, ~ {
        read.csv(.x) |>
          clean_names_tracking_data() |>
          dplyr::filter(location_lat != 0 & location_long != 0) |>
          dplyr::summarise(
            date = lubridate::ymd_hms(timestamp),
            lat = location_lat,
            lon = location_long,
            route_id = as.character(individual_local_identifier),
            route_type = "tracking"
          ) |>
          dplyr::filter(dplyr::between(lon, -180, 180), dplyr::between(lat, -90, 90)) |>
          stats::na.omit()
      })
    )
    
    saveRDS(new_tracking_data, file.path(output_path, paste0(name, ".rds")))
  }
}





## Under developement
load_tracking_df <- function(tracking_rds_path) {
  
  # Read data
  file_exists <- dplyr::if_else(file.exists(tracking_rds_path), TRUE, FALSE)
  if (!file_exists){
    return(NULL)
  }
  df <- readRDS(tracking_rds_path)
  if (nrow(df) == 0){
    return(NULL)
  }
  
  return(df)
}


# 
# } else {
#   return(list(straightness = NA_real_, length = NA_real_, displacement = NA_real_, 
#               n_stopovers = NA_real_))
#   

