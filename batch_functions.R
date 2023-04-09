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

# fit model container function
fit_model_container <- function(
    mypy,
    mydir,
    mysp,
    myres,
    mf_dist_weight,
    mf_ent_weight,
    mf_dist_pow,
    mf_obs_weight,
    mf_learning_rate,
    mf_training_steps,
    mf_rng_seed
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

# organize grid expansion for arguments...
setup_modelfit_arguments <- function(params, pp_info){
  orig <- data.frame(mypy  = params$mf_script,
                     mydir = params$dir,
                     mysp  = pp_info$species,
                     myres = pp_info$res)
  orig$id <- seq_len(nrow(orig))
  expanded <- expand.grid(
    id = orig$id,
    mf_dist_weight = params$mf_dist_weight,
    mf_ent_weight = params$mf_ent_weight,
    mf_dist_pow = params$mf_dist_pow,
    mf_obs_weight = params$mf_obs_weight,
    mf_learning_rate = params$mf_learning_rate,
    mf_training_steps = params$mf_training_steps,
    mf_rng_seed = params$mf_rng_seed
  )
  mf_args <- left_join(orig, expanded, by = 'id')
  mf_args$id <- NULL
  mf_args
}
