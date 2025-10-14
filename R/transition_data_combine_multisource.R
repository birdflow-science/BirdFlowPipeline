# Functions to combine multisource data

#' Combining the multsource data for one species
#' @param species the species to process specified by species code 
#'
#' @export
combine_and_save_ground_truth_data <- function(species, resolution=100, max_n=10000) {
  
  tmp_dir <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  params <- set_pipeline_params(
    species = species,
    res = resolution,
    gpu_ram = 10,
    hdf_path = tmp_dir,
    base_output_path = tmp_dir,
    model_selection = 'distance_metric',
    suffix = 'transition_data',
    skip_quality_checks = TRUE,
    min_season_quality = 1
  )
  params <- preprocess_species_wrapper(params)
  parent1 <- paste0(
    the$combined_data_path_routes,
    '/', as.character(resolution), 'km'
    )
  parent2 <- paste0(
    the$combined_data_path_birdflowroutes,
    '/', as.character(resolution), 'km'
  )
  parent3 <- paste0(
    the$combined_data_path_birdflowintervals,
    '/', as.character(resolution), 'km'
  )
  
  params$sp_output_path_routes <- paste0(
    parent1, '/', species, '.hdf5')
  params$sp_output_path_birdflowroutes <- paste0(
    parent2, '/', species, '.hdf5')
  params$sp_output_path_birdflowintervals <- paste0(
    parent3, '/', species, '.hdf5')
  
  if (file.exists(params$sp_output_path_routes) & 
      file.exists(params$sp_output_path_birdflowroutes) & 
      file.exists(params$sp_output_path_birdflowintervals)) {
    return() # Already finished!
  }
  
  if (!dir.exists(parent1)) {
    dir.create(parent1, recursive = TRUE)
  }
  if (!dir.exists(parent2)) {
    dir.create(parent2, recursive = TRUE)
  }
  if (!dir.exists(parent3)) {
    dir.create(parent3, recursive = TRUE)
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
  banding_df <- load_banding_df(file.path(
    the$banding_rds_path,
    paste0(params$species, '.rds')
  ))
  motus_df <- load_motus_df(file.path(
    the$motus_rds_path,
    paste0(params$species, '.rds')
  ))
  tracking_df <- load_tracking_df(file.path(
    the$tracking_rds_path,
    paste0(params$species, '.rds')
  ))
  # Real track. Not filtered by season. All year round.
  
  combined_data <- rbind(
    banding_df, 
    motus_df, 
    tracking_df
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
  }
  
  if (!is.null(motus_df)) {
    if (source == '') {
      source <- 'MOTUS'
    } else {
      source <- paste0(source, ' & ', 'MOTUS')
    }
  }
  
  if (!is.null(tracking_df)) {
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
  BirdFlowR::write_routes(routes_obj, params$sp_output_path_routes)
  routes_obj <- BirdFlowR::read_routes(params$sp_output_path_routes)
  
  # BirdFlowRoutes
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf = bf)
  if (nrow(birdflow_routes_obj$data) == 0) {
    stop("No BirdFlowRoutes data available")
  }
  
  BirdFlowR::write_routes(birdflow_routes_obj, params$sp_output_path_birdflowroutes)
  birdflow_routes_obj <- BirdFlowR::read_routes(params$sp_output_path_birdflowroutes)
  
  # BirdFlowIntervals
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(
      max_n = max_n,
      min_day_interval = 1,
      max_day_interval = 270,
      min_km_interval = 0,
      max_km_interval = 10000
    )
  if (nrow(interval_obj$data) == 0) {
    stop("No transition data available after extractions")
  }
  
  BirdFlowR::write_intervals(interval_obj, params$sp_output_path_birdflowintervals)
  interval_obj <- BirdFlowR::read_intervals(params$sp_output_path_birdflowintervals)
}

check_files_exist <- function(species, resolution) {
  parent1 <- paste0(
    the$combined_data_path_routes,
    '/', as.character(resolution), 'km'
  )
  parent2 <- paste0(
    the$combined_data_path_birdflowroutes,
    '/', as.character(resolution), 'km'
  )
  parent3 <- paste0(
    the$combined_data_path_birdflowintervals,
    '/', as.character(resolution), 'km'
  )
  
  sp_output_path_routes <- paste0(
    parent1, '/', species, '.hdf5')
  sp_output_path_birdflowroutes <- paste0(
    parent2, '/', species, '.hdf5')
  sp_output_path_birdflowintervals <- paste0(
    parent3, '/', species, '.hdf5')
  
  if (file.exists(sp_output_path_routes) & 
      file.exists(sp_output_path_birdflowroutes) & 
      file.exists(sp_output_path_birdflowintervals)) {
    return(TRUE) # Already finished!
  } else {
    return(FALSE)
  }
}


#' Combining the multsource data for all species
#'
#' @export
#'
combine_data_for_all_sp <- function(resolution=100, max_n=10000) {
  unique_names <- c(
    gsub('.rds','',list.files(the$motus_rds_path)),
    gsub('.rds','',list.files(the$banding_rds_path)),
    gsub('.rds','',list.files(the$tracking_rds_path))
  ) |> unique()
  
  args_df <- data.frame(list(species=unique_names, 
                             resolution=rep(resolution, length(unique_names)),
                             max_n=rep(max_n, length(unique_names))
                             ))
  args_df$exists <- apply(args_df, 1, function(row) {
    check_files_exist(row[["species"]], as.numeric(row[["resolution"]]))
  })
  args_df <- args_df[!args_df$exists,] |> dplyr::select(-exists)
  
  success <- FALSE
  batchtools::batchMap(
    fun = combine_and_save_ground_truth_data,
    args = args_df,
    reg = batchtools::makeRegistry(
      file.path('./', paste0(make_timestamp())),
      conf.file = system.file('batchtools.conf.R', 
                              package = 'BirdFlowPipeline')
    )
  )
  batchtools::submitJobs(dplyr::mutate(batchtools::findNotSubmitted(), chunk = 1L),
                         resources = list(walltime = 100,
                                          ncpu=1,
                                          memory = 5,
                                          measure.memory = TRUE))
  
  success <- batchtools::waitForJobs()
  
}

