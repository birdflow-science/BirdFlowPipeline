#' Functions to get ground truth routes and transitions for model tuning and validation, combining tracking, Motus, and banding.
#' @param params The params list preprocessed by `set_pipeline_params` and `preprocess_species_wrapper` functions.
#' @param bf The BirdFlow model, used to snap routes into BirdFlow coordinates.
#' @returns combined_routes_data, interval_obj, interval_one_week_obj
#' @export
get_ground_truth_routes_intervals_and_one_week_intervals <- function(params, bf) {
  
  ## 01. Combine data
  banding_df <- load_banding_transitions_df(file.path(the$banding_rds_path, paste0(params$species, '.rds')))
  motus_df <- load_motus_transitions_df(file.path(the$motus_rds_path, paste0(params$species, '.rds')))
  track_birdflowroutes_obj <- get_real_track(bf, params, filter=FALSE) # Real track. Not filtered by season. All year round.
  combined_data <- rbind(banding_df, motus_df, track_birdflowroutes_obj$data[,c('route_id','date','lon','lat','route_type')])
  combined_data <- na.omit(combined_data)
  
  ## 02. Dataframe to Routes
  source <- ''
  if (!is.null(banding_df)){
    if (source==''){
      source <- 'Banding'
    } else {
      source <- paste0(source, ' & ', 'Banding')
    }
  } else if (!is.null(motus_df)){
    if (source==''){
      source <- 'MOTUS'
    } else {
      source <- paste0(source, ' & ', 'MOTUS')
    }
  } else if (!is.null(track_birdflowroutes_obj)){
    if (source==''){
      source <- 'Tracking'
    } else {
      source <- paste0(source, ' & ', 'Tracking')
    }
  }
  
  if (source==''){
    source <- 'No Data'
  }
  
  routes_obj <- BirdFlowR::Routes(combined_data, species=bf$species, source=source)
  if (nrow(routes_obj$data)==0){
    stop("No Transition data available")
  }
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf=bf)
  
  ## 03. Extract transitions from BirdFlowRoutes
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=180,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  # Filter intervals to ask at least one leg in the migration season
  target_timesteps <- c(BirdFlowR::lookup_season_timesteps(bf, season='prebreeding'), 
                        BirdFlowR::lookup_season_timesteps(bf, season='postbreeding'))
  interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
  
  if (is.null(interval_obj)){
    stop("No intervals available")
  }
  if (nrow(interval_obj$data) <= 10*(1/0.7)) {
    stop("No enough transitions for tuning (<10*(1/0.7))")
  }
  
  ## 04. Extract one week samples from BirdFlowRoutes
  routes_one_week_obj <- BirdFlowR::Routes(combined_data, species=bf$species, source=source)
  interval_one_week_obj <- routes_one_week_obj |> BirdFlowR::as_BirdFlowRoutes(bf=bf) |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=13,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  interval_one_week_obj$data <-interval_one_week_obj$data[interval_one_week_obj$data$timestep2 - interval_one_week_obj$data$timestep1 == 1,]
  # Filter intervals to ask at least one leg in the migration season
  interval_one_week_obj$data <- interval_one_week_obj$data[(interval_one_week_obj$data$timestep1 %in% target_timesteps) | (interval_one_week_obj$data$timestep2 %in% target_timesteps),]
  
  
  return(list(combined_routes_data=combined_data,
         interval_obj=interval_obj,
         interval_one_week_obj=interval_one_week_obj))
}

