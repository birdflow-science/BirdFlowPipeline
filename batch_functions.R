# make timestamp
make_timestamp <- function(tz = "America/Los_Angeles"){
  datetime <- Sys.time()
  datetime <- `attr<-`(datetime, "tzone", tz)
  format(datetime, "%Y-%m-%d_%H-%M-%S")
}

# preprocess species wrapper
preprocess_species_wrapper <- function(...) {
  suppressMessages(invisible(capture.output(
    bf <- BirdFlowR::preprocess_species(...)
  )))
  # return res
  BirdFlowR::res(bf)[1] / 1000
}

# organize grid expansion for arguments...
birdflow_modelfit_args <- function(
    preprocessed_list,
    grid_search_list){
  orig <- data.frame(preprocessed_list)
  orig$id <- seq_len(nrow(orig))
  grid_search_list$id <- orig$id
  expanded <- expand.grid(grid_search_list)
  args <- left_join(orig, expanded, by = 'id')
  args$id <- NULL
  args
}

# fit model container function
# grid search parameters from paper not included as default arguments here:
#  dist_weight = 0.005 # a
#  ent_weight = seq(from = 0, to = 0.006, by = 0.001) # B
#  dist_pow <- seq(from = 0.1, to = 1.0, by = 0.1) # E
birdflow_modelfit <- function(
    mypy = "/home/dslager_umass_edu/birdflow/update_hdf.py",
    mydir,
    mysp,
    myres,
    dist_weight,
    ent_weight,
    dist_pow,
    obs_weight = 1,
    learning_rate = 0.1,
    training_steps = 600,
    rng_seed = 17
){
  system2('python',
          args = c(
            mypy,
            mydir,
            mysp,
            myres,
            paste0('--dist_weight=', dist_weight),
            paste0('--ent_weight=', ent_weight),
            paste0('--dist_pow=', dist_pow),
            paste0('--obs_weight=', obs_weight),
            paste0('--learning_rate=', learning_rate),
            paste0('--training_steps=', training_steps),
            paste0('--rng_seed=', rng_seed)
          ))
}

model_information_row <- function(i){
  mn <- i$model
  df <- tibble(
    model = mn,
    obs = sub('.*obs(.*?)_.*', '\\1', mn) %>% as.numeric,
    ent = sub('.*ent(.*?)_.*', '\\1', mn) %>% as.numeric,
    dist = sub('.*dist(.*?)_.*', '\\1', mn) %>% as.numeric,
    pow = sub('.*pow(.*?)\\.hdf5', '\\1', mn) %>% as.numeric,
    ll = sum(i$ll$log_likelihood, na.rm = TRUE),
    nll = sum(i$ll$null_ll, na.rm = TRUE),
    ll_raw_n = nrow(i$ll),
    ll_n = length(na.omit(i$ll$log_likelihood)),
    mean_distr_cor = i$mean_distr_cor
  )
  bf <- import_birdflow(file.path(my_dir, mn))
  rts <- route_migration(bf, 100, 'prebreeding')
  stats <- rts_stats(rts)
  for (i in names(stats)){
    df[[i]] <- stats[[i]]
  }
  df
}


