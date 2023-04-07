# batch preprocess species function
batch_preprocess_species <- function(params = params){
  # See ?Registry for more info on configuration files, e.g., always loading
  # certain packages or starting in certain working directories
  reg <- makeRegistry(params$pp_reg, conf.file = file.path('conf', 'preprocess_species.batchtools.conf.R'))
  # saveRegistry()
  # ?setDefaultRegistry
  # not needed because once we make registry, it stays for session as reg
  batchMap(fun = BirdFlowR::preprocess_species,
           args = expand.grid(
             species = params$species,
             out_dir = params$dir,
             gpu_ram = params$mem_mf,
             stringsAsFactors = FALSE)
  )
  rez <- list(walltime = params$wt_pp, ncpus = params$ncpu_pp, memory = params$mem_pp * 1000, partition = params$part_pp)
  submitJobs(resources = rez)
  waitForJobs()
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
    mymem,
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
                     myres = pp_info$res,
                     mymem = pp_info$mem + 1)
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

## Model Fitting ##
batch_fit_models <- function(params, pp_info){
  reg <- makeRegistry(params$mf_reg, conf.file = file.path('conf', 'modelfit.batchtools.conf.R'))

  ## Possible way to get around the static resources issue??
  # Note that all variables defined in a JobCollection can be used inside the
  # template. If you need to pass extra variables, you can set them via the
  # argument resources of submitJobs().
  # 
  # tmp = makeRegistry(file.dir = NA, make.default = FALSE, packages = "methods")
  # batchMap(identity, 1:5, reg = tmp)
  # 
  # # resources are usually set in submitJobs()
  # jc = makeJobCollection(1:3, resources = list(foo = "bar"), reg = tmp)
  # ls(jc)
  # jc$resources
  
  mf_args <- setup_modelfit_arguments(params, pp_info)
  
  batchMap(fun = fit_model_container, args = mf_args)
  
  # resources the same across all jobs
  static_rez <- list(walltime = params$wt_mf,
                     ncpus = 1,
                     ngpus = 1,
                     partition = params$part_mf,
                     mypy =  params$mf_script,
                     mydir = params$dir)
  
  # Need to loop through here to adjust variable "resources" for each job,
  # since species and resolution needs to be a static "resource"
  jobinfo <- getJobPars()$job.pars
  for (i in seq_along(jobinfo)){
    # variable rez here = Python arguments to be deparsed into sbatch file via brew
    rez       <- static_rez
    rez$mysp  <- jobinfo[[i]]$mysp
    rez$myres <- jobinfo[[i]]$myres
    rez$mymem <- jobinfo[[i]]$mymem
    rez$mf_dist_weight <- jobinfo[[i]]$mf_dist_weight
    rez$mf_ent_weight <- jobinfo[[i]]$mf_ent_weight
    rez$mf_dist_pow <- jobinfo[[i]]$mf_dist_pow
    rez$mf_obs_weight <- jobinfo[[i]]$mf_obs_weight
    rez$mf_learning_rate <- jobinfo[[i]]$mf_learning_rate
    rez$mf_training_steps <- jobinfo[[i]]$mf_training_steps
    rez$mf_rng_seed <- jobinfo[[i]]$mf_rng_seed
    
    stopifnot(all(lapply(rez, length) == 1))
    submitJobs(ids = i, resources = rez)
  }
  waitForJobs()
  #getJobTable()
}
