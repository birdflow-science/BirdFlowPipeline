# make timestamp
make_timestamp <- function(tz = "America/Los_Angeles"){
  datetime <- Sys.time()
  datetime <- `attr<-`(datetime, "tzone", tz)
  format(datetime, "%Y-%m-%d_%H-%M-%S")
}

# save info from preprocess-species batch function
save_preprocessing_info <- function(){
  my_result_list <- lapply(seq_len(nrow(getJobPars())), loadResult)
  lst <- list()
  lst$mem <- sapply(getJobPars()$job.pars, function(i){i$gpu_ram})
  lst$species <- sapply(my_result_list, function(i){i$species$species_code})
  lst$res <- sapply(my_result_list, function(i){i$geom$res[1]/1000})
  lst
}

# organize grid expansion for arguments...
setup_modelfit_arguments <- function(
    preprocess_list,
    grid_search_list){
  orig <- data.frame(preprocess_list)
  orig$id <- seq_len(nrow(orig))
  grid_search_list$id <- orig$id
  expanded <- expand.grid(grid_search_list)
  mf_args <- left_join(orig, expanded, by = 'id')
  mf_args$id <- NULL
  mf_args
}

# fit model container function
# grid search parameters from paper not included as default arguments here:
#  mf_dist_weight = 0.005
#  mf_ent_weight = seq(from = 0, to = 0.006, by = 0.001)
#  mf_dist_pow <- seq(from = 0.1, to = 1.0, by = 0.1)
fit_model_container <- function(
    mypy = "/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py",
    mydir,
    mysp,
    myres,
    mf_dist_weight,
    mf_ent_weight,
    mf_dist_pow,
    mf_obs_weight = 1,
    mf_learning_rate = 0.1,
    mf_training_steps = 1500,
    mf_rng_seed = 17
){
  system2('python',
          args = c(
            mypy,
            mydir,
            mysp,
            myres,
            paste0('--dist_weight=', mf_dist_weight),
            paste0('--ent_weight=', mf_ent_weight),
            paste0('--dist_pow=', mf_dist_pow),
            paste0('--obs_weight=', mf_obs_weight),
            paste0('--learning_rate=', mf_learning_rate),
            paste0('--training_steps=', mf_training_steps),
            paste0('--rng_seed=', mf_rng_seed)
          ))
}
