#' Get ground-truth transitions for tuning and validation
#'
#' Functions to get ground truth routes and transitions for model tuning and
#' validation by combining tracking, Motus, and banding sources, then converting
#' to [BirdFlowR::BirdFlowIntervals()] objects (full intervals and 1-week intervals).
#'
#' @param loader A [TransitionsLoader()] object.
#' @param max_n_intervals the maximum intervals to sample.
#' @return A list with:
#' \describe{
#'   \item{combined_routes_data}{`data.frame` of stacked banding, Motus, and tracking rows (NA-dropped).}
#'   \item{interval_obj}{[BirdFlowR::BirdFlowIntervals()] with 1–180 day / 0–8000 km constraints, filtered to migration seasons.}
#'   \item{interval_one_week_obj}{[BirdFlowR::BirdFlowIntervals()] with exactly 1-day steps (weekly objects), filtered to migration seasons.}
#' }
#'
#' @details
#' Side effects:
#' \itemize{
#'   \item Resolves/prints a `source` string reflecting which datasets were found.
#'   \item Errors if no transitions are available or if fewer than \eqn{10/0.7} intervals remain for tuning.
#' }
#'
#' @seealso [TransitionsLoader()], [split.TransitionsLoader()], [train_test_split()],
#'   [BirdFlowR::Routes()], [BirdFlowR::as_BirdFlowRoutes()], [BirdFlowR::as_BirdFlowIntervals()]
#' @export
#' @examples
#' \dontrun{
#' tl <- TransitionsLoader(trainer)
#' gt <- get_transitions(tl)
#' str(gt$interval_obj$data)
#' }
get_transitions <- function(loader, max_n_intervals=10000) {
  
  ## 00.Validate
  validate_TransitionsLoader(loader)
  
  ## 01. Combine data
  banding_rds_path <- file.path(the$banding_rds_path, paste0(loader$batch_trainer$params$species, '.rds'))
  motus_rds_path <- file.path(the$motus_rds_path, paste0(loader$batch_trainer$params$species, '.rds'))
  tracking_rds_path <- file.path(the$tracking_rds_path, paste0(loader$batch_trainer$params$species, '.rds'))

  if (file.exists(banding_rds_path)) {
    banding_df <- load_banding_df(banding_rds_path)
  } else {
    banding_df <- NULL
  }
  
  if (file.exists(motus_rds_path)) {
    motus_df <- load_motus_df(motus_rds_path)
  } else {
    motus_df <- NULL
  }
  
  if (file.exists(tracking_rds_path)) {
    tracking_df <- load_tracking_df(tracking_rds_path)
  } else {
    tracking_df <- NULL
  }
  
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

  routes_obj <- BirdFlowR::Routes(combined_data, species=loader$batch_trainer$bf$species, source=source)
  if (nrow(routes_obj$data)==0){
    stop("No Transition data available")
  }
  birdflow_routes_obj <- routes_obj |> BirdFlowR::as_BirdFlowRoutes(bf=loader$batch_trainer$bf)
  
  ## 03. Extract transitions from BirdFlowRoutes
  interval_obj <- birdflow_routes_obj |>
    BirdFlowR::as_BirdFlowIntervals(max_n=max_n_intervals,
                                    min_day_interval=1,
                                    max_day_interval=180,
                                    min_km_interval=0,
                                    max_km_interval=8000)
  if (is.null(interval_obj)){
    stop("No intervals available")
  }
  
  # Filter intervals to ask at least one leg in the migration season
  target_timesteps <- c(BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='prebreeding'), 
                        BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='postbreeding'))
  interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
  
  if (nrow(interval_obj$data) <= 10*(1/0.7)) {
    stop("No enough transitions for tuning (<10*(1/0.7))")
  }
  
  ## 04. Extract one week samples from BirdFlowRoutes
  routes_one_week_obj <- BirdFlowR::Routes(combined_data, species=loader$batch_trainer$bf$species, source=source)
  interval_one_week_obj <- routes_one_week_obj |> BirdFlowR::as_BirdFlowRoutes(bf=loader$batch_trainer$bf) |>
    BirdFlowR::as_BirdFlowIntervals(max_n=max_n_intervals,
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


#' Create a transitions loader
#'
#' Validates a [BatchBirdFlowTrainer()] and constructs a [TransitionsLoader()]
#' that can load and split combined movement data for evaluation/tuning.
#'
#' @param batch_trainer A [BatchBirdFlowTrainer()].
#'
#' @return A [TransitionsLoader()] object (invisible).
#' @seealso [new_transitions_loader()], [load.TransitionsLoader()], [split.TransitionsLoader()]
#' @export
#' @examples
#' \dontrun{
#' tl <- TransitionsLoader(trainer)
#' }
TransitionsLoader <- function(batch_trainer) {
  validate_BatchBirdFlowTrainer(batch_trainer)
  
  obj <- new_transitions_loader(batch_trainer)
  validate_TransitionsLoader(obj)
  
  return(obj)
}

#' Initialize a transitions loader (low-level constructor)
#'
#' Builds a [TransitionsLoader()] that carries a reference to the associated
#' [BatchBirdFlowTrainer()].
#'
#' @param batch_trainer A [BatchBirdFlowTrainer()].
#'
#' @return A [TransitionsLoader()] with field `batch_trainer`.
#' @seealso [TransitionsLoader()], [load.TransitionsLoader()]
#' @export
#' @examples
#' \dontrun{
#' tl <- new_transitions_loader(trainer)
#' }
new_transitions_loader <- function(batch_trainer) {
  # Get bf object (for converting to BirdFlowIntervals)
  
  obj <- list(
    batch_trainer = batch_trainer
  )
  class(obj) <- c("TransitionsLoader", class(obj))
  return(obj)
}


#' Load transitions into a TransitionsLoader (S3 method)
#'
#' Loads/combines banding, Motus, and tracking data via a `loading_function`
#' (default [get_transitions()]) and attaches the result to `loader$transitions`.
#'
#' @param object A [TransitionsLoader()].
#' @param loading_function A function with signature `function(loader)` that
#'   returns the list described in [get_transitions()].
#'
#' @return The modified `loader` (invisible) with `loader$transitions` populated.
#' @seealso [get_transitions()], [split.TransitionsLoader()]
#' @method load TransitionsLoader
#' @export
#' @examples
#' \dontrun{
#' tl <- transitions_loader(trainer)
#' tl <- load(tl)  # populates tl$transitions
#' }
load.TransitionsLoader <- function(object, loading_function=get_transitions, ...) {
  loader <- object
  # Validate
  validate_TransitionsLoader(loader)
  
  # Routes to BirdFlowRoutes to BirdFlowIntervals
  # Load and save transition info
  # Here should combine banding and motus data and tracking and convert to BirdFlowIntervals class
  transitions <- loading_function(loader)
  
  loader$transitions <- transitions
  invisible(loader)
}


#' Split transitions into training and test sets (S3 method)
#'
#' Uses a `splitting_function` (default [train_test_split()]) to create
#' training/test splits for interval data and 1-week interval data. Returns a
#' list containing both training and test bundles.
#'
#' @param object A [TransitionsLoader()] that has been loaded (see [load.TransitionsLoader()]).
#' @param splitting_function Function with signature `function(loader, seed, ...)`
#'   that returns the list described in [train_test_split()].
#' @param seed Optional integer to make the split reproducible.
#' @param ... Passed through to `splitting_function`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{training_data}{List with `train_data` and `train_data_one_week` (both [BirdFlowR::BirdFlowIntervals()]).}
#'   \item{test_data}{List with `test_data` and `test_data_one_week` (both [BirdFlowR::BirdFlowIntervals()]).}
#' }
#' @seealso [train_test_split()], [load.TransitionsLoader()]
#' @method split TransitionsLoader
#' @export
#' @examples
#' \dontrun{
#' tl <- load(transitions_loader(trainer))
#' parts <- split(tl, seed = 42)
#' names(parts)
#' }
split.TransitionsLoader <- function(object, splitting_function=train_test_split, seed=NULL, ...) {
  loader <- object
  split_data <- splitting_function(loader, seed, ...)
  validate_split_data(split_data) # The split_data should always be a list with names training_data and test_data
  
  return(split_data)
}


#' Train/test split helper for transitions
#'
#' Creates a random 70/30 split of interval transitions and one-week transitions,
#' with optional downsampling of the training set to a fixed number of transitions
#' (useful for learning-curve experiments). Returns four
#' [BirdFlowR::BirdFlowIntervals()] objects grouped into training/test lists.
#'
#' @param loader A loaded [TransitionsLoader()] (contains `$transitions`).
#' @param seed Optional integer for reproducibility (`set.seed()`).
#' @param training_n_transitions Optional integer. If supplied, subsamples the
#'   training intervals to exactly this number (errors if insufficient data).
#'
#' @return A list:
#' \describe{
#'   \item{training_data}{List with `train_data`, `train_data_one_week`.}
#'   \item{test_data}{List with `test_data`, `test_data_one_week`.}
#' }
#' @details
#' Side effects: none (beyond RNG if `seed` is provided).
#'
#' @seealso [split.TransitionsLoader()], [get_transitions()]
#' @export
#' @examples
#' \dontrun{
#' tl <- load(TransitionsLoader(trainer))
#' parts <- train_test_split(tl, seed = 1, training_n_transitions = 1000)
#' }
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


#' Generic loader S3
#'
#' Dispatches to class-specific `load()` methods (e.g.,
#' [load.TransitionsLoader()]).
#'
#' @param object The object to load data for.
#' @param ... Passed to methods.
#'
#' @return Method-dependent; see specific methods.
#' @export
#' @examples
#' \dontrun{
#' tl <- load(transitions_loader(trainer))
#' }
load <- function(object, ...) {
  UseMethod("load")
}


#' Generic split S3
#'
#' Dispatches to class-specific `split()` methods (e.g.,
#' [split.TransitionsLoader()]).
#'
#' @param object The object to split the data for.
#' @param ... Passed to methods.
#'
#' @return Method-dependent; see specific methods.
#' @export
#' @examples
#' \dontrun{
#' parts <- split(load(transitions_loader(trainer)), seed = 123)
#' }
split <- function(object, ...) {
  UseMethod("split")
}






