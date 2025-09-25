#' Functions to get ground truth routes and transitions for model tuning and validation, combining tracking, Motus, and banding.
#' @param loader TransitionsLoader class
#' @returns list of combined_routes_data, interval_obj, interval_one_week_obj
#' @export
get_transitions <- function(loader) {
  
  ## 00.Validate
  validate_transition_loader(loader)
  
  ## 01. Combine data
  banding_df <- load_banding_df(file.path(the$banding_rds_path, paste0(loader$params$species, '.rds')))
  motus_df <- load_motus_df(file.path(the$motus_rds_path, paste0(loader$params$species, '.rds')))
  tracking_df <- load_tracking_df(file.path(the$tracking_rds_path, paste0(loader$params$species, '.rds')))
  combined_data <- rbind(banding_df, motus_df, tracking_df)
  combined_data <- stats::na.omit(combined_data)
  
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
  
  routes_obj <- BirdFlowR::Routes(combined_data, species=loader$bf$species, source=source)
  if (nrow(routes_obj$data)==0){
    stop("No Transition data available")
  }
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf=loader$bf)
  
  ## 03. Extract transitions from BirdFlowRoutes
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(max_n=10000,
                                    min_day_interval=1,
                                    max_day_interval=180,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  if (is.null(interval_obj)){
    stop("No intervals available")
  }
  
  # Filter intervals to ask at least one leg in the migration season
  target_timesteps <- c(BirdFlowR::lookup_season_timesteps(loader$bf, season='prebreeding'), 
                        BirdFlowR::lookup_season_timesteps(loader$bf, season='postbreeding'))
  interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
  
  if (nrow(interval_obj$data) <= 10*(1/0.7)) {
    stop("No enough transitions for tuning (<10*(1/0.7))")
  }
  
  ## 04. Extract one week samples from BirdFlowRoutes
  routes_one_week_obj <- BirdFlowR::Routes(combined_data, species=loader$bf$species, source=source)
  interval_one_week_obj <- routes_one_week_obj |> BirdFlowR::as_BirdFlowRoutes(bf=loader$bf) |>
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

#' @export
transitions_loader <- function(batch_trainer) {
  validate_batch_trainer(batch_trainer)
  
  obj <- new_transitions_loader(batch_trainer)
  validate_transition_loader(obj)
  
  return(obj)
}

new_transitions_loader <- function(batch_trainer) {
  # Get bf object (for converting to BirdFlowIntervals)
  pp_dir <- tempdir()
  bf <- BirdFlowR::preprocess_species(
    species = batch_trainer$params$species,
    out_dir = pp_dir,
    gpu_ram = batch_trainer$params$gpu_ram,
    res = batch_trainer$params$res,
    season = dplyr::if_else(batch_trainer$params$truncate_season, batch_trainer$params$season, 'all'),
    clip = batch_trainer$params$clip,
    crs = batch_trainer$params$crs,
    skip_quality_checks = batch_trainer$params$skip_quality_checks, 
    trim_quantile = batch_trainer$params$trim_quantile
  )
  bf$metadata <- batch_trainer$params$metadata
  
  obj <- list(
    bf = bf,
    params = batch_trainer$params
  )
  class(obj) <- c("TransitionsLoader", class(obj))
  return(obj)
}

#' @method load TransitionsLoader
#' @export
load.TransitionsLoader <- function(loader, loading_function=get_transitions) {
  
  # Validate
  validate_transition_loader(loader)
  
  # Routes to BirdFlowRoutes to BirdFlowIntervals
  # Load and save transition info
  # Here should combine banding and motus data and tracking and convert to BirdFlowIntervals class
  transitions <- loading_function(loader)
  
  # combined_routes_data <- res[['combined_routes_data']]
  # interval_obj <- res[['interval_obj']]
  # interval_one_week_obj <- res[['interval_one_week_obj']]
  
  loader$transitions <- transitions
  invisible(loader)
}


#' @method split TransitionsLoader
#' @export
split.TransitionsLoader <- function(loader, splitting_function=train_test_split, seed=NULL, ...) {
  
  split_data <- splitting_function(loader, seed, ...)
  validate_split_data(split_data) # The split_data should always be a list with names training_data and test_data
  
  return(split_data)
}

#' @param loader TransitionsLoader class
#' @param seed Random seed
#' @param training_n_transitions minimum n_transitions for training. Useful for learning curve analysis
#' @export
train_test_split <- function(loader, seed=NULL, training_n_transitions=NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Train-test split
  train_data <- loader$transitions$interval_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
  test_data <- dplyr::setdiff(loader$transitions$interval_obj$data, train_data)
  
  ## Downsample data
  if (is.null(training_n_transitions)) {
    ## Nothing happens
  } else {
    if (nrow(train_data) < training_n_transitions) {
      stop(glue::glue('Cannot sample {training_n_transitions} transitions -- not enough data.'))
    } else {
      train_data <- train_data|> dplyr::sample_n(training_n_transitions, replace = FALSE) ## Subsample, for learning curve analysis
    }
  }
  
  ## Make train and test BirdFlowIntervals
  train_data <- BirdFlowR::BirdFlowIntervals(data=train_data,
                                             species=loader$transitions$interval_obj$species,
                                             metadata=loader$transitions$interval_obj$metadata,
                                             geom=loader$transitions$interval_obj$geom,
                                             dates=loader$transitions$interval_obj$dates,
                                             source=loader$transitions$interval_obj$source)
  test_data <- BirdFlowR::BirdFlowIntervals(data=test_data,
                                            species=loader$transitions$interval_obj$species,
                                            metadata=loader$transitions$interval_obj$metadata,
                                            geom=loader$transitions$interval_obj$geom,
                                            dates=loader$transitions$interval_obj$dates,
                                            source=loader$transitions$interval_obj$source)
  
  train_data_one_week <- loader$transitions$interval_one_week_obj$data |> dplyr::sample_frac(0.7, replace = FALSE)
  test_data_one_week <- dplyr::setdiff(loader$transitions$interval_one_week_obj$data, train_data_one_week)
  train_data_one_week <- BirdFlowR::BirdFlowIntervals(data=train_data_one_week,
                                                      species=loader$transitions$interval_obj$species,
                                                      metadata=loader$transitions$interval_obj$metadata,
                                                      geom=loader$transitions$interval_obj$geom,
                                                      dates=loader$transitions$interval_obj$dates,
                                                      source=loader$params$source)
  test_data_one_week <- BirdFlowR::BirdFlowIntervals(data=test_data_one_week,
                                                     species=loader$transitions$interval_obj$species,
                                                     metadata=loader$transitions$interval_obj$metadata,
                                                     geom=loader$transitions$interval_obj$geom,
                                                     dates=loader$transitions$interval_obj$dates,
                                                     source=loader$transitions$interval_obj$source)
  
  return(list(training_data = list(train_data = train_data,
                              train_data_one_week = train_data_one_week),
              test_data = list(test_data = test_data,
                               test_data_one_week = test_data_one_week)
         ))
}


#' @param object The object to load data for
#' @export
load <- function(object, ...) {
  UseMethod("load")
}


#' @param object The object to split the data into training and test set
#' @export
split <- function(object, ...) {
  UseMethod("split")
}






