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

# Find dist weight and ent weight, for a given obs proportion and de ratio
# where c = dist_weight/ent_weight
#       d = obs_weight ( = 1 - dist_weight - ent_weight)
#.      x = dist_weight
#.      y = ent_weight
find_xy <- function(c, d, s = 5){
  x = (c - c * d) / (c * d + d)
  y = (1 - d) / (c * d + d)
  return(
    c(x = signif(x, s),
      y = signif(y, s))
  )
}

# organize grid expansion for arguments... NEW
birdflow_modelfit_args_NEW <- function(
    preprocessed_list,
    grid_search_list_NEW){
  orig <- data.frame(preprocessed_list)
  orig$id <- seq_len(nrow(orig))
  grid_search_list_NEW$id <- orig$id
  df <- expand.grid(grid_search_list_NEW)
  for (i in seq_len(nrow(df))){
    xy <- find_xy(df$c[i], df$d[i])
    df$dist_weight[i] <- xy['x']
    df$ent_weight[i] <- xy['y']
  }
  args <- left_join(orig, df, by = 'id')
  args$id <- NULL
  args$c <- NULL
  args$d <- NULL
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
  bf <- import_birdflow(file.path(hdf_dir, mn))
  rts <- route_migration(bf, 100, 'prebreeding')
  stats <- rts_stats(rts)
  for (i in names(stats)){
    df[[i]] <- stats[[i]]
  }
  df
}

# function to make modelfit arguments df from old and new style grid_search_list
# OLD EXAMPLE:
# grid_search_list <- list(
#   dist_weight = seq(from = 0.0008, to = 0.0018, length.out = 5),
#   ent_weight = seq(from = 0.00015, to = 0.0004, length.out = 5),
#   dist_pow = seq(from = 0.1, to = .9, length.out = 5)
# )
# NEW EXAMPLE:
# grid_search_list <- list(
#   c = c(2, 4, 8, 16),
#   d = c(0.95, 0.975, 0.99, 0.999, 0.9999),
#   dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
#   dist_weight = NA_real_,
#   ent_weight = NA_real_
# )
make_birdflow_modelfit_args_df <- function(
    grid_search_type = NULL, grid_search_list = NULL){
  if (grid_search_type == 'old'){
    out <- birdflow_modelfit_args(
      preprocessed_list = list(
        mydir = hdf_dir,
        mysp = my_species,
        myres = my_res),
      grid_search_list = grid_search_list
    )
  } else if (grid_search_type == 'new'){
    out <- birdflow_modelfit_args_NEW(
      preprocessed_list = list(
        mydir = hdf_dir,
        mysp = my_species,
        myres = my_res),
      grid_search_list_NEW = grid_search_list)
  }
  out
}
