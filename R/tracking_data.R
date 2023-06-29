# scripts to get the n_stopovers and straightness of the actual tracks

#args:  ll_df, params

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
  ## rts structure ##
  # > str(.Last.value)
  # 'data.frame':	1500 obs. of  8 variables:
  #   $ x       : num  1458333 1458333 1458333 1458333 1458333 ...
  # $ y       : num  -375000 -375000 -375000 -375000 -375000 ...
  # $ route   : int  1 1 1 1 1 1 1 1 1 1 ...
  # $ timestep: num  4 5 6 7 8 9 10 11 12 13 ...
  # $ date    : chr  "2021-01-25" "2021-02-01" "2021-02-08" "2021-02-15" ...
  # $ i       : int  245 245 245 245 245 191 191 191 191 191 ...
  # $ stay_id : num  1 1 1 1 1 2 2 2 2 2 ...
  # $ stay_len: int  5 5 5 5 5 10 10 10 10 10 ...
  xy <- BirdFlowR::latlon_to_xy(lat = df$latitude, lon = df$longitude, bf = bf)
  df$x <- xy$x
  df$y <- xy$y
  df$route_id <- as.integer(as.factor(df$bird_id))
  # before doing timestep, really need to do rolling st nearest, like Ben's code, to subset to nearest
  df$timestep <- BirdFlowR::lookup_timestep(df$datetime, bf)
  # appropriate season tracks only
  df <- df %>% dplyr::filter(.data$timestep %in% BirdFlowR::lookup_season_timesteps(bf, params$season))
  season_start_step <- BirdFlowR::lookup_season_timesteps(bf, params$season)[1]
  season_stop_step <-  utils::tail(BirdFlowR::lookup_season_timesteps(bf, params$season), 1)
  # complete season tracks only (commented out because e.g. Wood Thrush tracks never include end of spring migration)
  #df <- df %>% group_by(route_id) %>%
  #  filter(all(c(season_start_step, season_stop_step) %in% timestep))
  df <- dplyr::filter(df, )
  df$i <- BirdFlowR::xy_to_i(x = df$x, y = df$y, bf)
  # convert back to xy from i to overwrite the fine-grained coordinates
  # required for calculating n_stopovers from trajr::TrajStepLengths()
  df$x <- BirdFlowR::i_to_x(i = df$i, bf)
  df$y <- BirdFlowR::i_to_y(i = df$i, bf)
  browser()
  df$date <- bf$dates$date[df$timestep]
  df <- df[,c('x', 'y', 'route_id', 'timestep', 'date', 'i')]
  df <- df %>% dplyr::group_by(.data$route_id) %>% dplyr::arrange(.data$timestep)
  df <- as.data.frame(stats::na.omit(df))
  
  add_stay_id <- function(df) {
    # Benjamin's function
    df |>
      dplyr::mutate(stay_id = cumsum(c(1, as.numeric(diff(.data$i)) != 0)),
                    stay_len = rep(rle(.data$stay_id)$lengths,
                                   times = rle(.data$stay_id)$lengths))
  }
  df |> dplyr::group_by(.data$route_id) |> add_stay_id() |> dplyr::ungroup() |>
    dplyr::arrange(.data$route_id, .data$timestep) |> as.data.frame()
}

#' Calculate rts_stats from real tracks file(s)
#'
#' @param ll_df The ll_df containing at least one valid model name, from which to load bf object for CRS/grid
#' @param params The params object, containing at least the hdf_dir from which to learn bf object
#' @returns rts_stats from the actual tracks
#' @export
real_track_stats <- function(ll_df, params){
  tracks_files <- list.files('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/tracking_data',
                             pattern = paste0("^", params$my_species, ".*tracks.*\\.csv"),
                             full.names = TRUE)
  if (length(tracks_files) > 0){
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
    bf <- BirdFlowR::import_birdflow(file.path(params$hdf_dir, ll_df$model[1]))
    rts <- tracks_to_rts(tracks, bf, params)
    # Get track data
    return(rts_stats(rts))
  } else {
    return(list(straightness = NA_real_, length = NA_real_, displacement = NA_real_, 
                n_stopovers = NA_real_))
  }
}
