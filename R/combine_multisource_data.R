# Functions to combine multisource data

#' Combining the multsource data for one species
#' @param species the species to process specified by species code 
#'
#' @export
combine_and_save_ground_truth_data <- function(species) {
  
  tmp_dir <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  params <- set_pipeline_params(
    species = species,
    res = 150,
    gpu_ram = 10,
    hdf_path = tmp_dir,
    base_output_path = tmp_dir,
    model_selection = 'distance_metric',
    suffix = 'transition_data',
    skip_quality_checks = TRUE,
    min_season_quality = 1
  )
  params <- preprocess_species_wrapper(params)
  
  params$sp_output_path_routes <- paste0(
    the$combined_data_path_routes,
    '/', species, '.hdf5')
  params$sp_output_path_birdflowroutes <- paste0(
    the$combined_data_path_birdflowroutes,
    '/', species, '.hdf5')
  params$sp_output_path_birdflowintervals <- paste0(
    the$combined_data_path_birdflowintervals,
    '/', species, '.hdf5')
  
  if (!dir.exists(the$combined_data_path_routes)) {
    dir.create(the$combined_data_path_routes,
               recursive = TRUE)
  }
  if (!dir.exists(the$combined_data_path_birdflowroutes)) {
    dir.create(the$combined_data_path_birdflowroutes,
               recursive = TRUE)
  }
  if (!dir.exists(the$combined_data_path_birdflowintervals)) {
    dir.create(the$combined_data_path_birdflowintervals,
               recursive = TRUE)
  }
  
  # Get bf object (for converting to BirdFlowIntervals)
  pp_dir <- tempdir()
  bf <- BirdFlowR::preprocess_species(
    species = params$species,
    out_dir = pp_dir,
    gpu_ram = params$gpu_ram,
    res = params$res,
    season = dplyr::if_else(params$truncate_season, params$season, 'all'),
    clip = params$clip,
    crs = params$crs,
    skip_quality_checks = params$skip_quality_checks,
    trim_quantile = params$trim_quantile
  )

  # Combine all the data
  banding_df <- load_banding_transitions_df(file.path(
    the$banding_rds_path,
    paste0(params$species, '.rds')
  ))
  motus_df <- load_motus_transitions_df(file.path(
    the$motus_rds_path,
    paste0(params$species, '.rds')
  ))
  track_birdflowroutes_obj <- get_real_track(bf, params, filter = FALSE) 
  # Real track. Not filtered by season. All year round.
  
  combined_data <- rbind(
    banding_df, 
    motus_df, 
    track_birdflowroutes_obj$data[
      , c('route_id', 'date', 'lon', 'lat', 'route_type')
      ]
    )
  combined_data <- stats::na.omit(combined_data)

  # Dataframe to Routes
  source <- ''
  if (!is.null(banding_df)) {
    if (source == '') {
      source <- 'Banding'
    } else {
      source <- paste0(source, ' & ', 'Banding')
    }
  } else if (!is.null(motus_df)) {
    if (source == '') {
      source <- 'MOTUS'
    } else {
      source <- paste0(source, ' & ', 'MOTUS')
    }
  } else if (!is.null(track_birdflowroutes_obj)) {
    if (source == '') {
      source <- 'Tracking'
    } else {
      source <- paste0(source, ' & ', 'Tracking')
    }
  }

  if (source == '') {
    source <- 'No Data'
  }

  # Routes
  routes_obj <- BirdFlowR::Routes(combined_data,
                                  species = bf$species,
                                  source = source)
  if (nrow(routes_obj$data) == 0) {
    stop("No Routes data available")
  }
  BirdFlowR::write_Rotues(routes_obj, params$sp_output_path_routes)
  routes_obj <- BirdFlowR::read_Rotues(params$sp_output_path_routes)
  
  # BirdFlowRoutes
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf =
                                                                      bf)
  if (nrow(birdflow_routes_obj$data) == 0) {
    stop("No BirdFlowRoutes data available")
  }
  
  BirdFlowR::write_BirdFlowRotues(birdflow_routes_obj, params$sp_output_path_birdflowroutes)
  birdflow_routes_obj <- BirdFlowR::read_BirdFlowRotues(params$sp_output_path_birdflowroutes)
  
  # BirdFlowIntervals
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(
      max_n = 100000,
      min_day_interval = 1,
      max_day_interval = 270,
      min_km_interval = 0,
      max_km_interval = 8000
    )
  if (nrow(interval_obj$data) == 0) {
    stop("No transition data available after extractions")
  }
  
  BirdFlowR::write_BirdFlowIntervals(interval_obj, params$sp_output_path_birdflowintervals)
  interval_obj <- BirdFlowR::read_BirdFlowIntervals(params$sp_output_path_birdflowintervals)
}


#' Combining the multsource data for all species
#'
#' @export
#'
combine_data_for_all_sp <- function() {
  unique_names <- c(
    gsub('.rds','',list.files(the$motus_rds_path)),
    gsub('.rds','',list.files(the$banding_rds_path)),
    sub("_.*", "", list.files(the$tracking_data_path, recursive = F, 
                              include.dirs=F, pattern = "\\.csv$", 
                              full.names = FALSE))
  ) |> unique()
  
  n <- length(unique_names)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  for (i in seq_along(unique_names)) {
    sp <- unique_names[i]
    tryCatch({
      combine_and_save_ground_truth_data(sp)
    }, 
    error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
    })
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

